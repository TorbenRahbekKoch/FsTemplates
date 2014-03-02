namespace Routing.Tests.Unit
open System.Linq
open System.Collections.Generic
open Routing.Routing
open Swensen.Unquote

module BushHelpers = 
    let defaultRouteNode() = 
        { pathItem = createPathItem(""); preAction = None; action= None; queries = None;
          children = Dictionary<string, RouteNode>(); templateChildren = Dictionary<string, RouteNode>(); predicates = None}

    let defaultBush() = { bush = Dictionary<HttpMethod,RouteNode>()}

    let defaultBushWithRoot httpMethod root = 
        let bush = Dictionary<HttpMethod, RouteNode>()
        bush.Add(httpMethod, root)
        bush

    let buildMethodBush routeEntry =
        let bush = { bush = Dictionary<HttpMethod, RouteNode>()}
        bush.BuildMethodBush routeEntry
        bush.bush

    let createChild path children = 
        let child = { defaultRouteNode() with pathItem = createPathItem(path) }
        children |> List.iter(fun item -> child.children.Add(item.pathItem.ToString(), item))
        child
    
    let createDefaultRoot children =
        let root = defaultRouteNode()
        children |> List.iter(fun child -> root.children.Add(child.pathItem.ToString(), child))
        root

    let compareBushes (actual:Dictionary<HttpMethod, RouteNode>) (expected:Dictionary<HttpMethod, RouteNode>) = 
        let verify (expr: Quotations.Expr<bool>) actualPathDescription expectedPathDescription =
            if not (expr.Eval()) then
                printf "Current path: %A - %A" actualPathDescription expectedPathDescription
            test expr 

        let loopDicts (actual:Dictionary<HttpMethod, RouteNode>) (expected:Dictionary<HttpMethod, RouteNode>) actualPathDescription expectedPathDescription action =
            for actualChild in actual do
                let key = actualChild.Key
                let found, expectedChild = expected.TryGetValue(key)
                let actualPath = actualPathDescription + "/" + key
                let expectedPath = expectedPathDescription + "/" + key
                match found with
                | false -> printf "Missing child at path: actual: %A - expected: %A" actualPath expectedPath
                           test <@ found @> 
                | true -> action actualChild.Value expectedChild actualPath expectedPath

        let rec compareNode1 (actual:Dictionary<HttpMethod, RouteNode>) (expected:Dictionary<HttpMethod, RouteNode>) actualPathDescription expectedPathDescription =
            verify <@ actual.Count = expected.Count @> actualPathDescription expectedPathDescription
            if (actual.Count > 0) then
                for actualChild in actual do
                    loopDicts actual expected actualPathDescription expectedPathDescription 
                              (fun (actual:RouteNode) (expected:RouteNode) actualPath expectedPath ->
                                verify <@ actual.pathItem = expected.pathItem @> actualPath expectedPath
                                verify <@ actual.preAction.IsNone = expected.preAction.IsNone @> actualPath expectedPath
                                verify <@ actual.preAction.IsSome = expected.preAction.IsSome @> actualPath expectedPath
                                verify <@ actual.children.Count = expected.children.Count @> actualPath expectedPath
                                verify <@ actual.templateChildren.Count = expected.templateChildren.Count @> actualPath expectedPath
                                verify <@ actual.predicates.IsNone = expected.predicates.IsNone @> actualPath expectedPath                                   
                              )

        let rec compareNode (actual: RouteNode) (expected: RouteNode) actualPath expectedPath =
            verify <@ actual.pathItem = expected.pathItem @> actualPath expectedPath
            verify <@ actual.preAction.IsNone = expected.preAction.IsNone @> actualPath expectedPath
            verify <@ actual.preAction.IsSome = expected.preAction.IsSome @> actualPath expectedPath
            verify <@ actual.children.Count = expected.children.Count @> actualPath expectedPath
            verify <@ actual.templateChildren.Count = expected.templateChildren.Count @> actualPath expectedPath
            verify <@ actual.predicates.IsNone = expected.predicates.IsNone @> actualPath expectedPath                                   

            loopDicts actual.children expected.children actualPath expectedPath compareNode

        verify <@ actual.Count = expected.Count @> "/" "/"
        loopDicts actual expected "/" "/" compareNode
        ()