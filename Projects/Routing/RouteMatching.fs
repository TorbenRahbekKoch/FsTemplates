namespace Routing
[<AutoOpen>]
module RouteMatching =
    open System.Collections.Generic
    open System.Linq
    open Routing.RequestContext

    [<NoComparison>]
    [<NoEquality>]
    type MatchContext = {
        requestContext  : RequestContext;
        preAction       : (RequestContext -> unit) option;
        action          : (RequestContext -> unit) option;
        templateValues  : Dictionary<string, string>;
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
    [<NoComparison>] [<NoEquality>]
    type MatchedRouteNode = {
        templateValues : Dictionary<string, string>
        matchedRoute : RouteNode
    }
    /// A node in the matched route
    and [<NoComparison>] [<NoEquality>]  
        RouteNode = {
            pathItem  : PathItem
            queries   : Dictionary<string, string> option
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
                    queries = None;
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
                | Some(node) -> node.findMatchingLeafNode rest
                | None -> // No fixed path was matched, try to see if any template matches
                    let templateChildNode = self.tryFindMatchingTemplateChildNode first
                    match templateChildNode with
                    | None -> None
                    | Some(matchedTemplateNode) -> // Now find any matching leafs on our template node
                        let childMatch = matchedTemplateNode.matchedRoute.findMatchingLeafNode rest
                        match childMatch with
                        | None -> None
                        | Some(matchedChildNode) -> // Combine the recursively collected template values
                            let allTemplateValues = 
                                matchedChildNode.templateValues
                                                .Union(matchedTemplateNode.templateValues)
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

    let defaultRouteNode() = 
        {
            pathItem = Path(""); 
            queries = None;
            preAction = None;
            action = None; 
            predicates = None; 
            children = Dictionary<string, RouteNode>();
            templateChildren = Dictionary<string, RouteNode>()
        }