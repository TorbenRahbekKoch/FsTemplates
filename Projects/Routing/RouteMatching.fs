namespace Routing
[<AutoOpen>]
module RouteMatching =
    open System.Collections.Generic
    open System.Linq
    open Routing.RequestContext

    type ActionResult =
    | StopRequest
    | ContinueRequest

    type Action = RequestContext -> ActionResult

    type RestrictionResult = 
    | RestrictedAndHandled
    | RestrictedAndNotHandled
    | NotRestricted 

    type RestrictStopAction = RequestContext -> unit

    type Restriction = RequestContext -> RestrictionResult

    [<NoComparison>]
    [<NoEquality>]
    type SuccessfulMatchContext = {
        requestContext  : RequestContext;
        preAction       : Action option;
        action          : Action;
        templateValues  : Dictionary<string, string>;
    }

    [<NoComparison>]
    [<NoEquality>]
    type RestrictedMatchContext = {
        requestContext : RequestContext
        handled        : bool
    }

    [<NoComparison>]
    [<NoEquality>]
    type MatchContext =
    | SuccessfulMatch of SuccessfulMatchContext
    | RestrictedMatch of RestrictedMatchContext
    | FailedMatch

    /// Path Items (between /) can be either a template ({id}) or just a simple path part
    type PathItem =
        | Template of string
        | Path of string
    with 
        override self.ToString() =
            match self with
            | Template(s) -> s
            | Path(s)     -> s

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
            preAction : Action option
            action    : Action
            children  : Dictionary<string, RouteNode>
            templateChildren : Dictionary<string, RouteNode>
            restrictions     : Restriction list option;
    }
    with 
        /// Inserts the given path, actions and restrictions
        member self.insertRoute (pathParts: string list) preAction action restrictions : RouteNode =
            match pathParts with
            | last::[] -> 
                let leafNode = self.ensureRouteNode last preAction action restrictions
                leafNode
            | first::rest -> 
                let node = self.ensureRouteNode first None (fun ctx -> ContinueRequest) None
                node.insertRoute rest preAction action restrictions
            // Root path
            | [] -> self.ensureRouteNode "" preAction action restrictions 

        member private self.ensureRouteNode pathPart preAction action restrictions =
            let found, node = self.children.TryGetValue(pathPart)
            if found then// If it already exists, we should do some tests on whether there is a predicate etc.
                node     // since we cannot have two different nodes with action/predicate on the same path, there should at least be a predicate on all but one
            else
                let pathItem = createPathItem(pathPart)
                let node = { 
                    pathItem = pathItem
                    queries = None
                    action = action
                    preAction = preAction
                    restrictions = restrictions
                    children = Dictionary<string, RouteNode>()
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
                Some({ matchedRoute = self; templateValues = Dictionary<string, string>()})
                

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
            action = (fun ctx -> ContinueRequest); 
            restrictions = None; 
            children = Dictionary<string, RouteNode>();
            templateChildren = Dictionary<string, RouteNode>()
        }