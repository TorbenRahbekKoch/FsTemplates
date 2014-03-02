namespace Routing
open System
open System.Linq
open System.Collections.Generic
open System.Runtime.Remoting.Messaging
open System.Threading
open System.Threading.Tasks
open System.Web
open Microsoft.Owin
open RouteMatching
open RequestContext
//open Route

module Routing =
    type HttpMethod = string

    /// Basic splitting of a path
    let splitRoutePath (path:string) =       
        let fakeUri = "http://dummy/" + path
        let uri = Uri(fakeUri, UriKind.Absolute)        
        let pathItems = uri.AbsolutePath.Split([|'/'|], StringSplitOptions.RemoveEmptyEntries)
                        |> Array.filter(fun p -> p <> "/")
                        |> Array.toList
        (pathItems, uri.Query)
    
    /// Route used for building route matching
    [<NoComparison>]
    [<NoEquality>]
    type Route = {
        /// Http Method (GET/PUT/etc) for this route
        httpMethod: string
        /// The full, host-relative path, eventually with query string
        route : string;
        preAction : (RequestContext -> unit) option;
        action    : (RequestContext -> unit) option;
        predicates : (RequestContext -> bool) list option;
    }
    and [<NoComparison>]
        [<NoEquality>]
        RouteGroup = {
            rootRoute : string;
            routes : RouteEntry list;
            action  : (RequestContext -> unit) option;
            queries : Dictionary<string, string> option;
            predicates : (RequestContext -> bool) list option;
        }
        and [<NoComparison>]
            [<NoEquality>]
            RouteEntry = 
                | Route of Route
                | RouteGroup of RouteGroup   
    
    let defaultRoute = { httpMethod = "GET"; route = String.Empty; preAction = None; action = None; predicates = None}
    let defaultRouteGroup = { rootRoute = String.Empty; routes = []; predicates = None; action = None; queries = None}

    [<NoComparison>]
    [<NoEquality>]
    type MatchContext = {
        requestContext  : RequestContext;
        preAction       : (RequestContext -> unit) option;
        action          : (RequestContext -> unit) option;
        templateValues  : Dictionary<string, string>;
    }

    // TODO: Implement a consistency check, which can check for e.g. duplicate template values in a path
    [<NoComparison>]
    [<NoEquality>]
    type HttpMethodBush = {
        bush : Dictionary<HttpMethod, RouteNode>
        }
    with
        member self.ensureHttpMethod httpMethod preAction action predicates =
            let found, branch = self.bush.TryGetValue(httpMethod)
            if found then
                branch
            else
                let rootNode = { 
                    pathItem = Path(""); 
                    queries = None;
                    preAction = preAction;
                    action = action; 
                    predicates = predicates; 
                    children = Dictionary<string, RouteNode>();
                    templateChildren = Dictionary<string, RouteNode>()}
                self.bush.Add(httpMethod, rootNode)
                rootNode

        member self.insertBranchAndLeaf httpMethod pathParts preAction action predicates =
            let rootNode = match pathParts with // Special case for root node
                           | [] -> self.ensureHttpMethod httpMethod preAction action predicates
                           | _ -> let httpMethodRoot = self.ensureHttpMethod httpMethod None None None
                                  httpMethodRoot.insertRoute pathParts preAction action predicates
            rootNode

        member self.rootNode httpMethod =
            self.bush.[httpMethod]

        member self.BuildMethodBush routeEntries =
            let rec buildBush parentRouteGroup routeEntries =
                routeEntries
                    |> List.collect(fun routeEntry -> 
                        match routeEntry with
                        | Route(route)           -> [ {route with route = parentRouteGroup.rootRoute + route.route; preAction = route.preAction} ]
                        | RouteGroup(routeGroup) -> buildBush routeGroup routeGroup.routes)
                    |> List.sortBy(fun route -> route.route)
            buildBush defaultRouteGroup routeEntries
            |> List.iter (fun route ->
                let pathParts, queryPart = splitRoutePath route.route
                self.insertBranchAndLeaf route.httpMethod pathParts route.preAction route.action route.predicates |> ignore) 

        member self.matchRequest (requestContext: RequestContext) =
            let absoluteUriPath = requestContext.Uri.AbsolutePath
            let rootNode = self.rootNode requestContext.Method
            let pathParts, query = splitRoutePath absoluteUriPath
            let node = rootNode.findMatchingLeafNode pathParts
            match node with
            | None -> None
            | Some(node) ->
                match node.matchedRoute.action with
                | None -> None
                | Some(action) -> 
                    let matchContext = { 
                        requestContext = requestContext; 
                        preAction = node.matchedRoute.preAction;
                        action = node.matchedRoute.action;
                        templateValues = node.templateValues }
                    match node.matchedRoute.predicates with
                    | None -> Some(matchContext)
                    | Some(predicates) -> 
                        if List.exists (fun predicate -> predicate requestContext) predicates  then 
                            Some(matchContext)
                        else 
                            None

    [<AbstractClass>]
    type Router() = 
        let methodBushMap = { bush = Dictionary<HttpMethod, RouteNode>()}

        member this.MethodBush with get () = methodBushMap

        member self.Init() =
            self.Register() |> self.BuildMethodBush
            self

        abstract Register : unit -> RouteEntry list

        member self.ExecuteRequestAsync (context: IOwinContext): Task =
            let r = context.Request
            
            let requestContext = { context = context; templateValues = Dictionary<string, string>() }
            let matchContext = self.MethodBush.matchRequest requestContext

            let executeAction (action: (RequestContext -> unit) option) matchContext =
                match action with
                | None -> Task.FromResult(0) :> Task
                | Some(action) -> 
                    Task.Run(fun() -> action matchContext) 

            match matchContext with
            | None -> self.NotFound context
            | Some(m) -> executeAction m.action m.requestContext            

//            let rootNode = methodBushMap.rootNode r.Method
//            let pathParts, query = splitRoutePath absoluteUriPath
//            let node = rootNode.findMatchingLeafNode pathParts
//            match node with
//            | None -> self.NotFound context
//            | Some(node) ->
//                match node.matchedRoute.action with
//                | None -> self.NotFound context
//                | Some(action) -> 
//                    let requestContext = { context = context; templateValues = node.templateValues }
//                    match node.matchedRoute.predicates with
//                    | None -> Task.Run(fun() -> action requestContext)
//                    | Some(predicates) -> 
//                        if List.exists (fun predicate -> predicate requestContext) predicates  then 
//                            Task.Run(fun() -> action requestContext) 
//                        else 
//                            self.NotFound context

        member private self.NotFound (context: IOwinContext) =
                context.Response.StatusCode <- 404
                context.Response.Write("No matching route found.")
                Task.FromResult(0) :> Task
        
        member private self.BuildMethodBush routeEntries =
            methodBushMap.BuildMethodBush routeEntries

    let routes routes =
        routes

    let group path routes =
        RouteGroup { defaultRouteGroup with rootRoute = path; routes = routes }
        
    let GET path =
        Route { defaultRoute with httpMethod = "GET"; route = path }

    let POST path =
        Route { defaultRoute with httpMethod = "POST"; route = path }

    let PUT path =
        Route { defaultRoute with httpMethod = "PUT"; route = path }

    let DELETE path =
        Route { defaultRoute with httpMethod = "DELETE"; route = path }

    let action (action:RequestContext -> unit) (route: RouteEntry) =
        match route with
        | Route(route) -> Route { route with action = Some(action) }
        | RouteGroup(group) -> RouteGroup { group with action = Some(action)}

    let view (text:string) (route: RouteEntry) =
        match route with
        | Route(route) -> Route {route with action = Some(fun r -> r.context.Response.Write(text))}
        | RouteGroup(group) -> failwith "Cannot use view on RouteGroup."

    let restrict predicate (route: RouteEntry) =
        match route with
        | Route(route) -> 
            Route({ route with predicates = match route.predicates with
                                            | None -> Some([predicate]) 
                                            | Some(existing) -> Some(predicate::existing)})
        | RouteGroup(group) -> 
            RouteGroup({ group with predicates = match group.predicates with
                                                 | None -> Some([predicate]) 
                                                 | Some(existing) -> Some(predicate::existing)})
