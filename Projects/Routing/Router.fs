namespace Routing
open System
open System.Linq
open System.Collections.Generic
open System.Runtime.Remoting.Messaging
open System.Threading
open System.Threading.Tasks
open System.Web
open Microsoft.Owin

module Routing =
    type HttpMethod = string

    /// Basic splitting of a path
    let splitRoutePath (path:string) =
        let pathItems = path.Split([|'/'|], StringSplitOptions.RemoveEmptyEntries)
                        |> Array.filter(fun p -> p <> "/")
                        |> Array.toList
        pathItems

    /// Context used by actions
    [<NoComparison>]
    type RequestContext = {
        context : IOwinContext;
        /// Name/value pairs for matched templates
        templateValues : Dictionary<string, string>;
    }

    /// Route used for building route matching
    [<NoComparison>]
    [<NoEquality>]
    type Route = {
        /// The full, host-relative path
        route : string;
        action  : (RequestContext -> unit) option;
        /// Http Method (GET/PUT/etc) for this route
        httpMethod: string
        predicate : (RequestContext -> bool) option;
    }

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
    [<NoComparison>]
    [<NoEquality>]
    type MatchedRouteNode = {
        templateValues : Dictionary<string, string>
        matchedRoute : RouteNode
    }
    /// A node in the route
    and [<NoComparison>]
        [<NoEquality>]  
        RouteNode = {
        pathItem : PathItem
        action : (RequestContext -> unit) option
        children : Dictionary<string, RouteNode>
        templateChildren : Dictionary<string, RouteNode>
        predicate : (RequestContext -> bool) option;
    }
    with 
        /// 
        member self.insertRoute (pathParts: string list) action predicate : RouteNode =
            match pathParts with
            | last::[] -> 
                let leafNode = self.ensureRouteNode last action predicate
                leafNode
            | first::rest -> 
                let node = self.ensureRouteNode first None None
                node.insertRoute rest action predicate
            | [] -> // Root path
                match action with // the root only matches if it has an action
                | Some(a) -> self.ensureRouteNode "" action predicate
                | None -> failwith "Cannot add leaf node without action!"

        member private self.ensureRouteNode pathPart action predicate =
            let found, node = self.children.TryGetValue(pathPart)
            if found then// If it already exists, we should do some tests on whether there is a predicate etc.
                node     // since we cannot have two different nodes with action/predicate on the same path, there should at least be a predicate on all but one
            else
                let pathItem = createPathItem(pathPart)
                let node = { 
                    pathItem = pathItem; 
                    action = action; 
                    predicate = predicate; 
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
        member private self.tryMatchTemplate1 template name value =
            (true, name, value)

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
        member self.ensureHttpMethod httpMethod action predicate =
            let found, branch = self.bush.TryGetValue(httpMethod)
            if found then
                branch
            else
                let rootNode = { 
                    pathItem = Path(""); 
                    action = action; 
                    predicate = predicate; 
                    children = Dictionary<string, RouteNode>();
                    templateChildren = Dictionary<string, RouteNode>()}
                self.bush.Add(httpMethod, rootNode)
                rootNode

        member self.insertBranchAndLeaf httpMethod pathParts action predicate =
            let rootNode = match pathParts with // Special case for root node
                           | [] -> self.ensureHttpMethod httpMethod action predicate
                           | _ -> let httpMethodRoot = self.ensureHttpMethod httpMethod None None
                                  httpMethodRoot.insertRoute pathParts action predicate
            rootNode

        member self.rootNode httpMethod =
            self.bush.[httpMethod]

    [<AbstractClass>]
    type Router() = 
        let methodBushMap = { bush = Dictionary<HttpMethod, RouteNode>()}

        member this.MethodBush with get () = methodBushMap

        member self.Init() =
            self.Register() |> self.BuildMethodBush
            self

        abstract Register : unit -> Route list

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
                    match node.matchedRoute.predicate with
                    | None -> Task.Run(fun() -> action requestContext)
                    | Some(predicate) -> 
                        if predicate requestContext then 
                            Task.Run(fun() -> action requestContext) 
                        else 
                            self.NotFound context

        member private self.NotFound (context: IOwinContext) =
                context.Response.StatusCode <- 404
                context.Response.Write("No matching route found.")
                Task.FromResult(0) :> Task
        
        member self.BuildMethodBush routes =
            routes 
                |> List.sortBy(fun item -> item.route)
                |> List.iter 
                (fun route -> 
                    let pathParts = splitRoutePath route.route
                    methodBushMap.insertBranchAndLeaf route.httpMethod pathParts route.action route.predicate |> ignore) 

    let defaultRoute = { httpMethod = "GET"; route = String.Empty; action = None; predicate = None}

    let routes r =
        r

    let GET path =
        { defaultRoute with httpMethod = "GET"; route = path }

    let POST path =
        { defaultRoute with httpMethod = "POST"; route = path }

    let PUT path =
        { defaultRoute with httpMethod = "PUT"; route = path }

    let DELETE path =
        { defaultRoute with httpMethod = "DELETE"; route = path }

    let action (action:RequestContext -> unit) (route: Route) =
        { route with action = Some(action)}

    let view (text:string) (route: Route) =
        { route with action = Some(fun r -> r.context.Response.Write(text))}

    let restrict predicate (route: Route)  =
        { route with predicate = Some(predicate) }
