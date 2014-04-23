namespace WebLibrary
module WebSocketContext =
    open System     
    open System.Collections.Generic
    open System.Threading
    open System.Threading.Tasks
    open Routing

    type WebSocketAccept = Action<IDictionary<string, Object>, // options
                                  Func<IDictionary<string, Object>, Task>>

    type WebSocketCloseAsync = Func<int , // closeStatus 
                                    string , // closeDescription 
                                    CancellationToken, // cancel 
                                    Task>
    type WebSocketReceiveAsync =
            Func<ArraySegment<byte>, // data */
                 CancellationToken ,  // cancel */
                 Task<Tuple<int ,     // messageType */
                            bool ,           // endOfMessage */
                            int >>>          // count */
    type WebSocketSendAsync =
            Func<ArraySegment<byte> , // data */
                 int ,                 // messageType */
                 bool ,                // endOfMessage */
                 CancellationToken ,   // cancel */
                 Task>

    type WebSocketReceiveResult = Tuple<int, // type
                                        bool, // end of message?
                                        int> // count

    type CloseStatus = CloseStatus of int

    type WebSocketMessage = 
    | Binary of byte[]
    | Text of string

    type WebSocketContext = {
        cancellation : CancellationToken
        receive : ArraySegment<byte> -> CancellationToken -> Task<Tuple<int, bool, int>>
        send    : ArraySegment<byte> -> int -> bool -> CancellationToken -> Task
        close   : int -> string -> CancellationToken -> Task
        requestContext : RequestContext
        processor      : MailboxProcessor<WebSocketMessage>
    }

    let create (requestContext: RequestContext) (simpleContext: IDictionary<string, Object>) =
        let context = requestContext.context
        let sendAsync = simpleContext.Item("websocket.SendAsync") :?> WebSocketSendAsync
        let receiveAsync =  simpleContext.Item("websocket.ReceiveAsync") :?> WebSocketReceiveAsync
        let closeAsync = simpleContext.Item("websocket.CloseAsync") :?> WebSocketCloseAsync
        let cancellation = context.Get<CancellationToken>("websocket.CallCancelled")

        let receive = (fun (segment: ArraySegment<byte>) (callCancelled: CancellationToken) 
                        -> receiveAsync.Invoke(segment, callCancelled))
        let close = (fun (closeStatus: int) (closeDescription: string) (cancellationToken: CancellationToken)
                        -> closeAsync.Invoke(closeStatus, closeDescription, cancellationToken))
        let send = (fun (data: ArraySegment<byte>) (messageType: int) (endOfMessage: bool) (cancellationToken: CancellationToken)
                        -> sendAsync.Invoke(data, messageType, endOfMessage, cancellationToken))


        let processor = new MailboxProcessor<WebSocketMessage>(fun inbox ->
            let rec loop() =
                async { 
                    let! message = inbox.Receive()
                    let buffer = match message with
                                 | Binary(data) -> new ArraySegment<byte>(data)
                                 | Text(data) -> new ArraySegment<byte>(System.Text.Encoding.UTF8.GetBytes(data))
                    let task = send buffer 1 true cancellation 
                    do! Async.AwaitIAsyncResult(task) |> Async.Ignore
                    return! loop()// Async.AwaitIAsyncResult(loop()) |> Async.Ignore
                }
            loop())
        processor.Start()

        {   receive = receive
            send = send
            close = close
            requestContext = requestContext
            cancellation = cancellation
            processor = processor 
        }

module WebSocketRoute = 
    open Routing
    open WebSocketContext

    type WebSocketRoute =  {        
        onPing      : (WebSocketContext -> unit) option
        onPong      : (WebSocketContext -> unit) option
        onBinary    : (WebSocketContext -> byte[] -> unit) option
        onText      : (WebSocketContext -> string -> unit) option
        onClose     : (CloseStatus -> unit) option
        path        : string
        routeEntry  : RouteEntry
    }

    let asRouteEntry wsr =
        wsr.routeEntry

    let createDefault =
        {
            onPing = None
            onPong = None
            onText = None
            onBinary = None
            onClose = None
            path = ""
            routeEntry = Route defaultRoute
        }

module WebSocket =
    open System
    open System.Collections.Generic
    open System.Threading
    open System.Threading.Tasks
    open System.Security.Cryptography

    open Microsoft.FSharp.Control
   
    open Routing
    open Router
    open Methods

    open WebSocketContext
    open WebSocketRoute

    [<Literal>]    
    let ``Sec-WebSocket-Accept`` = "Sec-WebSocket-Accept"
    let ``Sec-WebSocket-Key`` = "Sec-WebSocket-Key"
    let ``Sec-WebSocket-Version`` = "Sec-WebSocket-Version"
    let ``Sec-WebSocket-Key-Guid`` = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"


    let isWebSocketRequest (requestContext: RequestContext) =
        let r = requestContext.request
        let accept = requestContext.context.Get<WebSocketAccept>("websocket.Accept")
        let isValidWebSocketRequest = 
            accept <> null &&
            //r.Scheme = "ws" &&
            r.Method = GETMethod &&
            (r.hasHeaderWith "Upgrade" "websocket") &&
            (r.hasHeaderWith "Connection" "Upgrade") &&
            (r.hasHeaderWith ``Sec-WebSocket-Version`` "13") &&
            (r.hasHeader ``Sec-WebSocket-Key``)        
        isValidWebSocketRequest
        

    let handleWebSocket webSocketRoute (requestContext: RequestContext) (webSocketContext: WebSocketContext) = 
        let context = requestContext.context

        let buffer = Array.zeroCreate 1024
        let action = 
            async {
                let rec loop() = async {
                    let found, status = context.Environment.TryGetValue("websocket.ClientCloseStatus") 
                    if not found || (status :?> int) = 0 then
                        let! received = Async.AwaitTask (webSocketContext.receive (ArraySegment<byte>(buffer)) webSocketContext.cancellation)
                        let messageType, endOfMessage, count = received.Item1, received.Item2, received.Item3       
                                        
                        if messageType = 1 && webSocketRoute.onText.IsSome then
                            // Consider what do do when count > size of the buffer
                            let text = System.Text.Encoding.UTF8.GetString(buffer, 0, count)

                            try
                                webSocketRoute.onText.Value webSocketContext text
                            with
                            | ex -> ()
                        
                        ()
                        // main
                    
                    return! loop()
                }
                do! loop()

                let closeStatus = context.Get<int>("websocket.ClientCloseStatus")
                let closeDescription = context.Get<string>("websocket.ClientCloseDescription")
                do! Async.AwaitTask(webSocketContext.close closeStatus closeDescription webSocketContext.cancellation :?> Task<unit>) 
                ()
            }
        
        let task = new Task(fun () -> Async.RunSynchronously action)
        task.Start()
        task

    let sendText (webSocketContext: WebSocketContext) (text: string) =
        webSocketContext.processor.Post(Text text)

    let acceptWebSocket webSocketRoute (requestContext: RequestContext) =
        let context = requestContext.context
        let acceptws = requestContext.context.Get<WebSocketAccept>("websocket.Accept")
        let accept = (fun (options: IDictionary<string, Object>) (action: Func<IDictionary<string, Object>, Task>) 
                        -> acceptws.Invoke(options, action))
        
        accept null (System.Func<_,_>( fun (options: IDictionary<string, Object>) ->
            handleWebSocket webSocketRoute requestContext (WebSocketContext.create requestContext options)))

    let webSocketRouteAction webSocketRoute path =
        let route = GET path
                    |> restrict isWebSocketRequest
                    |> action (fun requestContext -> 
                        acceptWebSocket webSocketRoute requestContext
                        ContinueRequest)
        route

    let WEBSOCKET path = 
        let webSocketRoute = { WebSocketRoute.createDefault with path = path }
        let route = webSocketRouteAction webSocketRoute path
        
        // We loose the above captured webSocketRoute already here
        { webSocketRoute with routeEntry = route }

    let onText action webSocketRoute  =
        // We also loose the captured webSocketRoute here
        let onTextRoute = { webSocketRoute with onText = Some action }
        let route = webSocketRouteAction onTextRoute (webSocketRoute.path)
        { webSocketRoute with routeEntry = route }
        
    let onBinary action webSocketRoute =
        let onBinaryRoute = { webSocketRoute with onBinary = Some action}
        let route = webSocketRouteAction onBinaryRoute (webSocketRoute.path)
        { webSocketRoute with routeEntry = route }