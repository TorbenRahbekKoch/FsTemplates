namespace Routing
open System
open System.Linq
open System.Collections.Generic
open System.Runtime.Remoting.Messaging
open System.Threading
open System.Threading.Tasks
open System.Web
open Microsoft.Owin
open Routing
open RequestContext

//[<AutoOpen>]
module HttpMethod = 
    type HttpMethod = 
        | DELETE
        | GET
        | HEAD
        | OPTIONS
        | PATCH
        | POST
        | PUT

    let create (httpMethod: string) =
        match httpMethod.ToUpperInvariant() with
        | "DELETE"  -> DELETE
        | "GET"     -> GET
        | "HEAD"    -> HEAD
        | "OPTIONS" -> OPTIONS
        | "PATCH"   -> PATCH
        | "POST"    -> POST
        | "PUT"     -> PUT
        | _ -> failwith ("Unknown HttpMethod: " + httpMethod)
        
    let value httpMethod =
        match httpMethod with
        | DELETE  -> "DELETE"
        | GET     -> "GET"
        | HEAD    -> "HEAD"
        | OPTIONS -> "OPTIONS" 
        | PATCH   -> "PATCH"
        | POST    -> "POST"
        | PUT     -> "PUT"  
        
[<AutoOpen>]
module RoutingPath = 
    type RoutingPath = | RoutingPath of string
               
    let create (route: string) =
        let success, uri = System.Uri.TryCreate(route, UriKind.Relative)
        if not success then failwith ("Incorrectly formatted route: " + route)
        else RoutingPath route

    let value (RoutingPath value) =
        value

    let add (left: RoutingPath) (right: RoutingPath) =
        create (value left + value right);

    let inline (/+) ((left: RoutingPath), (right: RoutingPath)) =
            add left right

    type Q = | Q of string
        with static member (+) (Q(s1), Q(s2)) = Q(s1)

//    let inline add ((left: Q), (right: Q)) =
//        left.ToString()// + right.ToString()

    let r1 = create "/test"
    let r2 = create "/test"

    //let r3 = (+) (r1, r2)
    //let r4 = r1 /+ r2

[<AutoOpen>]
module Routing =
    /// Basic splitting of a path and  query (latter not implemented yet)
    let splitRoutePath (path:string) =       
        let pathItems = path.Split([|'/'|], StringSplitOptions.RemoveEmptyEntries)
                        |> Array.filter(fun p -> p <> "/")
                        |> Array.toList
        (pathItems, "")
//    let splitRoutePath (path:string) =       
//        let fakeUri = "http://dummy/" + path
//        let uri = Uri(fakeUri, UriKind.Absolute)        
//        let pathItems = uri.AbsolutePath.Split([|'/'|], StringSplitOptions.RemoveEmptyEntries)
//                        |> Array.filter(fun p -> p <> "/")
//                        |> Array.toList
//        (pathItems, uri.Query)
    
    /// Route used for building route matching

    [<NoComparison>]
    [<NoEquality>]
    type Route = {
        /// Http Method (GET/PUT/etc) for this route
        httpMethod: HttpMethod.HttpMethod
        /// The full, host-relative path, eventually with query string
        route : RoutingPath
        preAction : Action option
        action    : Action
        restrictions : Restriction list option
    }
    and RequestPipelineType = 
    | Pre
    | Post
    and [<NoComparison>]
        [<NoEquality>]
    RequestPipeline = {
        requestType : RequestPipelineType 
        action      : Action
        restrictions: Restriction list option
    }
    and [<NoComparison>]
        [<NoEquality>]
    RouteGroup = {
        rootRoute : RoutingPath
        routes : RouteEntry list
        action  : Action option
        queries : Dictionary<string, string> option
        restrictions : Restriction list option
    }
    and [<NoComparison>]
        [<NoEquality>]
    RouteEntry = 
        | Route of Route
        | RouteGroup of RouteGroup   
        | RequestPipeline of RequestPipeline
    
    let defaultRoute = { httpMethod = HttpMethod.GET; route = RoutingPath.create "/"; preAction = None; action = (fun ctx -> ContinueRequest); restrictions = None}
    let defaultRouteGroup = { rootRoute = RoutingPath.create "/"; routes = []; restrictions = None; action = None; queries = None}
    let defaultRequestPipeline = { requestType = Pre; action = (fun ctx -> ContinueRequest); restrictions = None }

module MethodBush = 
    open HttpMethod
    // TODO: Implement a consistency check, which can check for e.g. duplicate template values in a path
//    [<NoComparison>]
//    [<NoEquality>]
//    type HttpMethodBush = {
//        bush     : Dictionary<HttpMethod, RouteNode>
//    }
//    with
//        member self.rootNode httpMethod =
//            self.bush.[httpMethod]
//
//        member self.BuildMethodBush routeEntries =
//            let rec buildBush parentRouteGroup routeEntries =
//                routeEntries
//                |> List.collect(fun routeEntry -> 
//                    match routeEntry with
//                    | Route(route)              -> [ {route with route = parentRouteGroup.rootRoute + route.route; preAction = route.preAction; action=route.action} ]
//                    | RouteGroup(routeGroup)    -> buildBush routeGroup routeGroup.routes
//                    | RequestPipeline(pipeline) -> [])
//                |> List.sortBy(fun route -> route.route)
//
//            buildBush defaultRouteGroup routeEntries
//            |> List.iter (fun route ->
//                let pathParts, queryPart = splitRoutePath route.route
//                self.insertBranchAndLeaf route.httpMethod pathParts route.preAction route.action route.predicates |> ignore) 
//
    let rootNode httpMethod (bush: Map<HttpMethod.HttpMethod, RouteNode>) =
        bush.[httpMethod]
        
    let matchRequest (requestContext: RequestContext) (bush: Map<HttpMethod.HttpMethod, RouteNode>)=
        let absoluteUriPath = requestContext.Uri.AbsolutePath
        let rootNode = rootNode (HttpMethod.create requestContext.Method) bush
        let pathParts, query = splitRoutePath absoluteUriPath
        let node = rootNode.findMatchingLeafNode pathParts
        match node with
        | None -> FailedMatch
        | Some(node) ->
            let matchContext = { 
                requestContext = requestContext; 
                preAction = node.matchedRoute.preAction;
                action = node.matchedRoute.action;
                templateValues = node.templateValues }
            match node.matchedRoute.restrictions with
            | None -> SuccessfulMatch matchContext
            | Some(restrictions) -> 
                let restrictResult = restrictions |> List.fold(fun state item -> 
                        match state with
                        | NotRestricted           -> item requestContext
                        | RestrictedAndHandled    -> RestrictedAndHandled
                        | RestrictedAndNotHandled -> RestrictedAndNotHandled) NotRestricted

                match restrictResult with
                | NotRestricted           -> SuccessfulMatch matchContext
                | RestrictedAndHandled    -> RestrictedMatch { requestContext = requestContext; handled = true }
                | RestrictedAndNotHandled -> RestrictedMatch { requestContext = requestContext; handled = false }

    let ensureHttpMethod (bush: Dictionary<HttpMethod, RouteNode>) httpMethod preAction action restrictions =
        let found, branch = bush.TryGetValue(httpMethod)
        if found then
            branch
        else
            let rootNode = { defaultRouteNode() with 
                                action = action; 
                                preAction = preAction; 
                                restrictions = restrictions; 
                            }
            bush.Add(httpMethod, rootNode)
            rootNode

    let insertBranchAndLeaf (bush: Dictionary<HttpMethod, RouteNode>) httpMethod pathParts preAction action restrictions =
        let rootNode = match pathParts with // Special case for root node
                        | [] -> ensureHttpMethod bush httpMethod preAction action restrictions
                        | _ -> let httpMethodRoot = ensureHttpMethod bush httpMethod None (fun ctx -> ContinueRequest) None
                               httpMethodRoot.insertRoute pathParts preAction action restrictions
        rootNode


    let create routeEntries =
        let bush = Dictionary<HttpMethod, RouteNode>()
        let rec buildBush parentRouteGroup routeEntries =
            routeEntries
            |> List.collect(fun routeEntry -> 
                match routeEntry with
                | Route(route)              -> 
                    
                    let t = parentRouteGroup.rootRoute
                    [ {route with route = (RoutingPath.(/+)(t, route.route)); preAction = route.preAction; action=route.action} ]
                | RouteGroup(routeGroup)    -> buildBush routeGroup routeGroup.routes
                | RequestPipeline(pipeline) -> [])
            |> List.sortBy(fun route -> RoutingPath.value route.route)

        buildBush defaultRouteGroup routeEntries
        |> List.iter (fun route ->
            let pathParts, queryPart = splitRoutePath (RoutingPath.value route.route)
            insertBranchAndLeaf bush route.httpMethod pathParts route.preAction route.action route.restrictions |> ignore) 
        [for keyValue in bush -> (keyValue.Key, keyValue.Value)]
        |> Map.ofList

module RouteBuilding =
    open Routing


    let addRequestPipeline (requestPipelines: Dictionary<RequestPipelineType, List<RequestPipeline>>) requestPipeline =
        let found, pipelines = requestPipelines.TryGetValue(requestPipeline.requestType)            
        if not found then 
            let pipelines = List<RequestPipeline>()
            pipelines.Add(requestPipeline)
            requestPipelines.Add(requestPipeline.requestType, pipelines)
        else
            pipelines.Add(requestPipeline)


    let buildRequestPipeline routeEntries =
        let requestPipelines = Dictionary<RequestPipelineType, List<RequestPipeline>>()
        routeEntries
        |> List.iter(fun entry -> 
            match entry with
            | RequestPipeline(pipeline) -> addRequestPipeline requestPipelines pipeline
            | _ -> ())
        [for keyValue in requestPipelines -> (keyValue.Key, List.ofSeq keyValue.Value)]
            |> Map.ofList


[<AutoOpen>]
module Router =
    open RouteBuilding

//    [<NoComparison>]
//    [<NoEquality>]
//    type Router = {
//        methodBushMap : Map<HttpMethod, RouteNode>
//        requestPipeline : Map<RequestPipelineType, RequestPipeline list>
//    }

//    let create routeEntries =        
//        let methodBush = buildMethodBush routeEntries
//        let requestPipeline = buildRequestPipeline routeEntries
//        
//        Router_2(methodBush, requestPipeline)
        

    [<AbstractClass>]
    type Router(routeEntries) = //  (methodBush: Map<HttpMethod, RouteNode>, requestPipeline: Map<RequestPipelineType, RequestPipeline list>) = 
        let methodBush = MethodBush.create routeEntries
        let requestPipeline = RouteBuilding.buildRequestPipeline routeEntries

        member self.ExecuteRequestAsync (context: IOwinContext): Task =
            let r = context.Request
            
            let requestContext = ``from context`` context 
            
            let actionResult = self.executeRequestPipeline RequestPipelineType.Pre requestContext
            match actionResult with
            | StopRequest -> Task.FromResult 0 :> Task
            | ContinueRequest -> 
                let matchContext = MethodBush.matchRequest requestContext methodBush

                match matchContext with
                | FailedMatch               -> self.NotFound context
                | RestrictedMatch context   -> Task.FromResult(0) :> Task
                | SuccessfulMatch matchContext   ->

                    let executeActions (matchContext: SuccessfulMatchContext) =
                        let getExecutableAction action =
                            match action with
                            | None          -> (fun requestContext -> ContinueRequest)
                            | Some(action)  -> (fun requestContext -> action requestContext)

                        let preAction = getExecutableAction matchContext.preAction
                        let requestContext = { matchContext.requestContext with templateValues = matchContext.templateValues }                
                
                        Async.StartAsTask(
                            async {
                                let preActionResult = preAction requestContext
                                match preActionResult with
                                | StopRequest -> ()
                                | ContinueRequest -> matchContext.action requestContext |> ignore                    
                            },
                            TaskCreationOptions.None,
                            requestContext.request.CallCancelled) :> Task

                    executeActions matchContext

        member private self.executeRequestPipeline requestPipelineType (requestContext: RequestContext) =
            let pipelines = requestPipeline.TryFind(requestPipelineType)
            match pipelines with
            | None            -> ContinueRequest
            | Some(pipelines) ->
                let executeConditional (action: Action) restrictions =
                    match restrictions with
                    | None      -> ContinueRequest
                    | Some(restrictions) -> 
                        let restrictResult = restrictions |> List.fold(fun state item -> 
                                match state with
                                | NotRestricted        -> item requestContext
                                | RestrictedAndHandled -> RestrictedAndHandled
                                | RestrictedAndNotHandled -> RestrictedAndNotHandled) NotRestricted 

                        match restrictResult with
                        | NotRestricted    -> action requestContext
                        | RestrictedAndHandled -> StopRequest
                        | RestrictedAndNotHandled -> ContinueRequest

                let actionResult = pipelines |> List.tryPick(fun pipeline -> 
                        match executeConditional pipeline.action pipeline.restrictions with
                        | ContinueRequest -> Some(ContinueRequest)
                        | StopRequest     -> None)
                match actionResult with
                | Some(actionResult) -> actionResult
                | None               -> StopRequest

        member private self.NotFound (context: IOwinContext) =
                context.Response.StatusCode <- 404
                context.Response.Write("No matching route found.")
                Task.FromResult(0) :> Task

//self.executeRequestPipeline RequestPipelineType.Post requestContext//
//        member this.MethodBush with get () = methodBushMap
//        //member this.RequestPipeline with get() = ref requestPipeline
//
//        member self.Init() =
//            self.Register() 
//            |> self.BuildRequestPipeline
//            |> self.BuildMethodBush
//            self
//
//        abstract Register : unit -> RouteEntry list
//

//

//        
//        member private self.BuildMethodBush routeEntries =
//            methodBushMap.BuildMethodBush routeEntries
//
//        member private self.BuildRequestPipeline routeEntries =
//            let requestPipelines = Dictionary<RequestPipelineType, List<RequestPipeline>>()
//            routeEntries
//            |> List.iter(fun entry -> 
//                match entry with
//                | RequestPipeline(pipeline) -> self.addRequestPipeline requestPipelines pipeline
//                | _ -> ())
//            self.requestPipeline <- [for keyValue in requestPipelines -> (keyValue.Key, List.ofSeq keyValue.Value)]
//                |> Map.ofList
//            routeEntries
//
//        member self.addRequestPipeline (requestPipelines: Dictionary<RequestPipelineType, List<RequestPipeline>>) requestPipeline =
//            let found, pipelines = requestPipelines.TryGetValue(requestPipeline.requestType)            
//            if not found then 
//                let pipelines = List<RequestPipeline>()
//                pipelines.Add(requestPipeline)
//                requestPipelines.Add(requestPipeline.requestType, pipelines)
//            else
//                pipelines.Add(requestPipeline)


    let restrictions restrictions restriction =
        match restrictions with
        | None           -> Some([restriction])
        | Some(existing) -> Some(restriction::existing)

    let routes routes =
        routes

    let group path routes =
        RouteGroup { defaultRouteGroup with rootRoute = RoutingPath.create path; routes = routes }
        
    let GET path =
        Route { defaultRoute with httpMethod = HttpMethod.GET; route = RoutingPath.create path }

    let POST path =
        Route { defaultRoute with httpMethod = HttpMethod.POST; route = RoutingPath.create path }

    let PUT path =
        Route { defaultRoute with httpMethod = HttpMethod.PUT; route = RoutingPath.create path }

    let DELETE path =
        Route { defaultRoute with httpMethod = HttpMethod.DELETE; route = RoutingPath.create path }

    let PREREQUEST =
        RequestPipeline { defaultRequestPipeline with requestType = Pre}

    let POSTREQUEST =
        RequestPipeline { defaultRequestPipeline with requestType = Post}

    let action (action: Action) (routeEntry: RouteEntry) =
        match routeEntry with
        | Route(route)              -> Route { route with action = action }
        | RouteGroup(group)         -> RouteGroup { group with action = Some(action)}
        | RequestPipeline(request)  -> RequestPipeline { request with action = action }

    let view (text:string) (Route route) =
        Route {route with action = (fun r -> r.context.Response.Write(text);ContinueRequest)}
//        match routeEntry with
//        | Route(route)              -> Route {route with action = Some(fun r -> r.context.Response.Write(text))}
//        | RouteGroup(group)         -> failwith "Cannot use view on RouteGroup."
//        | RequestPipeline(request)  -> failwith "Cannot use view on RequestPipelines."

    let restrict restriction (routeEntry: RouteEntry) =
        let matchRestriction = (fun requestContext -> 
            if restriction requestContext then NotRestricted
            else RestrictedAndNotHandled)
        match routeEntry with
        | Route(route) -> 
            Route({ route with restrictions = restrictions route.restrictions matchRestriction})
        | RouteGroup(group) -> 
            RouteGroup({ group with restrictions = restrictions group.restrictions matchRestriction})
        | RequestPipeline(request) ->
            RequestPipeline({ request with restrictions = restrictions request.restrictions matchRestriction})

    let matchRestrict matchRestriction (routeEntry: RouteEntry) = 
        match routeEntry with
        | Route(route) -> 
            Route({ route with restrictions = restrictions route.restrictions matchRestriction})
        | RouteGroup(group) -> 
            RouteGroup({ group with restrictions = restrictions group.restrictions matchRestriction})
        | RequestPipeline(request) ->
            RequestPipeline({ request with restrictions = restrictions request.restrictions matchRestriction})
