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

    type MessageType =
    | ContinuationFrame = 0
    | TextFrame = 1
    | BinaryFrame = 2
    | ConnectionClose = 8
    | Ping = 9
    | Pong = 10

    type WebSocketMessage = 
    | Stop
    | Binary of byte[]
    | Text of string

    type Receive = ArraySegment<byte> -> CancellationToken -> Task<Tuple<int, bool, int>>
    type Send = ArraySegment<byte> -> int -> bool -> CancellationToken -> Task
    type Close = int -> string -> CancellationToken -> Task
    

    type WebSocketContext(cancellation: CancellationToken, 
                          receive: Receive, 
                          send: Send, 
                          close: Close, 
                          requestContext: RequestContext, 
                          sendMessageProcessor: MailboxProcessor<WebSocketMessage>) = 
        member self.Cancellation  with get() = cancellation
        member self.Receive with get() = receive
        member self.Send with get() = send
        member self.Close with get() = close
        member self.RequestContext with get() = requestContext
        member self.SendMessageProcessor with get() = sendMessageProcessor

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
                    if message = Stop then
                        ()
                    else 
                        let buffer = match message with
                                     | Binary(data) -> new ArraySegment<byte>(data)
                                     | Text(data)   -> new ArraySegment<byte>(System.Text.Encoding.UTF8.GetBytes(data))
                                     | Stop         -> failwith "Unexpected Stop message received."
                        let task = send buffer 1 true cancellation 
                        do! Async.AwaitIAsyncResult(task) |> Async.Ignore
                        return! loop()// Async.AwaitIAsyncResult(loop()) |> Async.Ignore
                }
            loop())
        processor.Start()

        WebSocketContext(cancellation, receive, send, close, requestContext, processor)

module WebSocketRoute = 
    open Routing
    open WebSocketContext

    type WebSocketRoute(path: string) =
        member self.Path with get() = path
        member val OnConnect : (WebSocketContext -> unit) option = None with get, set
        member val OnPing    : (WebSocketContext -> unit) option = None with get, set
        member val OnPong    : (WebSocketContext -> unit) option = None with get, set
        member val OnBinary  : (WebSocketContext -> byte[] -> unit) option = None with get, set
        member val OnText    : (WebSocketContext -> string -> unit) option = None with get, set
        member val OnClose   : (WebSocketContext -> unit) option = None with get, set
        member val RouteEntry: RouteEntry option = None with get, set

    let asRouteEntry (webSocketRoute: WebSocketRoute) =
        webSocketRoute.RouteEntry.Value

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
        
    let callOnConnect (webSocketRoute: WebSocketRoute) (webSocketContext: WebSocketContext)= 
        if webSocketRoute.OnConnect.IsSome then
            try
                webSocketRoute.OnConnect.Value webSocketContext
            with
                _ -> ()

    let callOnText (webSocketRoute: WebSocketRoute) (webSocketContext: WebSocketContext) text = 
        if (webSocketRoute.OnText.IsSome) then
            try
                webSocketRoute.OnText.Value webSocketContext text
            with
                _ -> ()

    let callOnClose (webSocketRoute: WebSocketRoute) (webSocketContext: WebSocketContext) =
        if (webSocketRoute.OnClose.IsSome) then
            try
                webSocketRoute.OnClose.Value webSocketContext 
            with
                _ -> ()
        
    let handleWebSocket (webSocketRoute: WebSocketRoute) (requestContext: RequestContext) (webSocketContext: WebSocketContext) = 
        let context = requestContext.context
        
        callOnConnect webSocketRoute webSocketContext

        let buffer = Array.zeroCreate 1024
        let action = 
            async {
                let rec loop() = async {
                    let found, status = context.Environment.TryGetValue("websocket.ClientCloseStatus") 
                    if not found || (status :?> int) = 0 then
                        let! received = Async.AwaitTask (webSocketContext.Receive (ArraySegment<byte>(buffer)) webSocketContext.Cancellation)
                        let messageType, endOfMessage, count = enum<MessageType>(received.Item1), received.Item2, received.Item3       
                           
                        if messageType = MessageType.TextFrame && webSocketRoute.OnText.IsSome then
                            // Consider what do do when count > size of the buffer
                            let text = System.Text.Encoding.UTF8.GetString(buffer, 0, count)
                            callOnText webSocketRoute webSocketContext text
                        
                        if messageType = MessageType.ConnectionClose then
                            webSocketContext.SendMessageProcessor.Post Stop
                            ()
                    else
                        return! loop()
                }
                do! loop()

                let closeStatus = context.Get<int>("websocket.ClientCloseStatus")
                let closeDescription = context.Get<string>("websocket.ClientCloseDescription")
                callOnClose webSocketRoute webSocketContext
                //do! Async.AwaitTask(webSocketContext.Close closeStatus closeDescription webSocketContext.Cancellation :?> Task<unit>) 
                ()
            }
        
        let task = new Task(fun () -> Async.RunSynchronously action)
        task.Start()
        task

    let sendText (webSocketContext: WebSocketContext) (text: string) =
        webSocketContext.SendMessageProcessor.Post(Text text)

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
        let webSocketRoute = WebSocketRoute(path)
        let route = webSocketRouteAction webSocketRoute path
        webSocketRoute.RouteEntry <- Some(route)
        webSocketRoute
       
    let onConnect action (webSocketRoute: WebSocketRoute) =
        webSocketRoute.OnConnect <- Some(action)
        webSocketRoute
        
    let onDisconnect action (webSocketRoute: WebSocketRoute) =
        webSocketRoute.OnClose <- Some(action)
        webSocketRoute
        
    let onText action (webSocketRoute: WebSocketRoute) =
        webSocketRoute.OnText <- Some(action)
        webSocketRoute
        
    let onBinary action (webSocketRoute: WebSocketRoute) =
        webSocketRoute.OnBinary <- Some(action)
        webSocketRoute