namespace Routing
open System
open System.Linq
open System.Collections.Generic
open System.Runtime.Remoting.Messaging
open System.Threading
open System.Threading.Tasks
open System.Web
open Microsoft.Owin
open RequestContext

module Routing =
    type HttpMethod = string

    /// Basic splitting of a path
    let splitRoutePath (path:string) =
        let pathItems = path.Split([|'/'|], StringSplitOptions.RemoveEmptyEntries)
                        |> Array.filter(fun p -> p <> "/")
                        |> Array.toList
        pathItems

    /// Route used for building route matching
    [<NoComparison>]
    [<NoEquality>]
    type Route = {
        /// Http Method (GET/PUT/etc) for this route
        httpMethod: string
        /// The full, host-relative path
        route : string;
        preAction : (RequestContext -> unit) option;
        action    : (RequestContext -> unit) option;
        queries   : Dictionary<string, string> option;
        predicates : (RequestContext -> bool) list option;
    }
    and [<NoComparison>]
        [<NoEquality>]
        RouteGroup = {
            root : string;
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
    
    let defaultRoute = { httpMethod = "GET"; route = String.Empty; preAction = None; action = None; predicates = None; queries = None}
    let defaultRouteGroup = { root = String.Empty; routes = []; predicates = None; action = None; queries = None}

    /// Path Items (between /) can be either a template ({id}) or just a simple path part
    type PathItem =
        | Template of string
        | Path of string
    with 
        override self.ToString() =
            match self with
            | Template(s) -> s
            | Path(s) -> s

    /// Create a PathItem from a pathPart/template
    let createPathItem (pathPart: string) =
        if pathPart.StartsWith("{") && pathPart.EndsWith("}") && pathPart.Length > 2 then
            Template(pathPart.Substring(1, pathPart.Length - 2))
        else
            Path(pathPart)

    /// The result from a route match
    [<NoComparison>] [<NoEquality>]
    type MatchedRouteNode = {
        templateValues : Dictionary<string, string>
        matchedRoute : RouteNode
    }
    /// A node in the route
    and [<NoComparison>] [<NoEquality>]  
        RouteNode = {
            pathItem  : PathItem
            preAction : (RequestContext -> unit) option
            action    : (RequestContext -> unit) option
            children  : Dictionary<string, RouteNode>
            templateChildren : Dictionary<string, RouteNode>
            predicates       : (RequestContext -> bool) list option;
    }
    with 
        /// Inserts the given path, actions and predicates
        member self.insertRoute (pathParts: string list) preAction action predicates : RouteNode =
            match pathParts with
            | last::[] -> 
                let leafNode = self.ensureRouteNode last preAction action predicates
                leafNode
            | first::rest -> 
                let node = self.ensureRouteNode first None None None
                node.insertRoute rest preAction action predicates
            | [] -> // Root path
                match action with // the root only matches if it has an action
                | Some(a) -> self.ensureRouteNode "" preAction action predicates
                | None -> failwith "Cannot add leaf node without action!"

        member private self.ensureRouteNode pathPart preAction action predicates =
            let found, node = self.children.TryGetValue(pathPart)
            if found then// If it already exists, we should do some tests on whether there is a predicate etc.
                node     // since we cannot have two different nodes with action/predicate on the same path, there should at least be a predicate on all but one
            else
                let pathItem = createPathItem(pathPart)
                let node = { 
                    pathItem = pathItem; 
                    action = action; 
                    preAction = preAction;
                    predicates = predicates; 
                    children = Dictionary<string, RouteNode>();
                    templateChildren = Dictionary<string, RouteNode>()}
                match pathItem with
                | Template(s) -> self.templateChildren.Add(pathItem.ToString(), node) 
                | Path(s) -> self.children.Add(pathItem.ToString(), node)
                node    

        member self.findMatchingLeafNode pathParts : MatchedRouteNode option =
            match pathParts with
            | leaf::[] ->
                let matchedLeaf = self.tryFindMatchingChildNode leaf
                match matchedLeaf with
                | Some(matchedLeaf) -> Some({ templateValues = Dictionary<string, string>(); matchedRoute = matchedLeaf })
                | None -> // No fixed path was matched, try to see if any template matches
                    self.tryFindMatchingTemplateChildNode leaf

            | first::rest ->
                let childNode = self.tryFindMatchingChildNode first
                match childNode with
                | Some(node) -> childNode.Value.findMatchingLeafNode rest
                | None -> // No fixed path was matched, try to see if any template matches
                    let templateChildNode = self.tryFindMatchingTemplateChildNode first
                    match templateChildNode with
                    | None -> None
                    | Some(node) -> // Now find any matching leafs on our template node
                        let childMatch = node.matchedRoute.findMatchingLeafNode rest
                        match childMatch with
                        | None -> None
                        | Some(matchedChildNode) -> // Combine the recursively collected template values
                            let allTemplateValues = 
                                matchedChildNode.templateValues
                                                .Union(node.templateValues)
                                                .ToDictionary((fun item -> item.Key), (fun (item: KeyValuePair<string,string>) -> item.Value))
                            Some({ matchedChildNode with templateValues = allTemplateValues })
            | _ -> // Trying to match root node                
                match self.action with
                | Some(action) -> Some({ matchedRoute = self; templateValues = Dictionary<string, string>()})
                | None -> None

        /// 
        member private self.tryFindMatchingChildNode pathPart = 
            let found, node = self.children.TryGetValue(pathPart)
            match found with
            | true -> Some(node)
            | false -> None

        /// Tries to match the given value to the template. Currently all templates
        /// match, but in the future templates could be specialized by type or other interesting stuff
        member private self.tryMatchTemplate template value =
            true

        member private self.tryFindMatchingTemplateChildNode nodeValue = 
            let matchingTemplateChildren = 
                self.templateChildren
                |> Seq.filter(
                    fun child -> self.tryMatchTemplate child.Value nodeValue)
                |> Seq.toList
            match matchingTemplateChildren with
            | leafValue::[] -> 
                let templateValues = Dictionary<string, string>()
                templateValues.Add(leafValue.Key, nodeValue)
                Some({ templateValues = templateValues; matchedRoute = leafValue.Value })
            | [] -> None
            | _ -> None // Having more matches should be reported somehow, since this indicates a configuration error                   

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
                        | Route(route)           -> [ {route with route = parentRouteGroup.root + route.route; preAction = route.preAction} ]
                        | RouteGroup(routeGroup) -> buildBush routeGroup routeGroup.routes)
                    |> List.sortBy(fun route -> route.route)
            buildBush defaultRouteGroup routeEntries
            |> List.iter (fun route ->
                let pathParts = splitRoutePath route.route
                self.insertBranchAndLeaf route.httpMethod pathParts route.preAction route.action route.predicates |> ignore) 

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
            let absoluteUriPath = r.Uri.AbsolutePath
            let rootNode = methodBushMap.rootNode r.Method

            let requestContext = { context = context; templateValues = Dictionary<string, string>() }
            let pathParts = splitRoutePath absoluteUriPath
            let node = rootNode.findMatchingLeafNode pathParts
            match node with
            | None -> self.NotFound context
            | Some(node) ->
                match node.matchedRoute.action with
                | None -> self.NotFound context
                | Some(action) -> 
                    let requestContext = { context = context; templateValues = node.templateValues }
                    match node.matchedRoute.predicates with
                    | None -> Task.Run(fun() -> action requestContext)
                    | Some(predicates) -> 
                        if List.exists (fun predicate -> predicate requestContext) predicates  then 
                            Task.Run(fun() -> action requestContext) 
                        else 
                            self.NotFound context

        member private self.NotFound (context: IOwinContext) =
                context.Response.StatusCode <- 404
                context.Response.Write("No matching route found.")
                Task.FromResult(0) :> Task
        
        member private self.BuildMethodBush routeEntries =
            methodBushMap.BuildMethodBush routeEntries

    let routes routes =
        routes

    let group path routes =
        RouteGroup { defaultRouteGroup with root = path; routes = routes }
        
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
