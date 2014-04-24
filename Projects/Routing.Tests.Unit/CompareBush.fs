namespace Routing.Tests.Unit
open System.Linq
open System.Collections.Generic
open Routing.Routing
open Routing
open Swensen.Unquote

// Various helper methods for making the tests more readable
module BushHelpers = 
    let defaultRouteNode() = 
        { pathItem = createPathItem(""); preAction = None; action= (fun ctx -> ContinueRequest); queries = None;
          children = Dictionary<string, RouteNode>(); templateChildren = Dictionary<string, RouteNode>(); restrictions = None}

    let defaultBush() = Dictionary<HttpMethod,RouteNode>()

    let defaultBushWithRoot httpMethod root = 
        let bush = [(httpMethod, root)] |> Map.ofSeq
        bush

    let buildMethodBush routeEntry =        
        let bush = MethodBush.create routeEntry
        bush

    let createChild path children = 
        let child = { defaultRouteNode() with pathItem = createPathItem(path) }
        children |> List.iter(fun item -> child.children.Add(item.pathItem.ToString(), item))
        child
    
    let createDefaultRoot children =
        let root = defaultRouteNode()
        children |> List.iter(fun child -> root.children.Add(child.pathItem.ToString(), child))
        root

    let compareBushes (actual:Map<HttpMethod, RouteNode>) (expected:Map<HttpMethod, RouteNode>) = 
        let verify (expr: Quotations.Expr<bool>) actualPathDescription expectedPathDescription =
            if not (expr.Eval()) then
                printf "Current path: %A - %A" actualPathDescription expectedPathDescription
            test expr 

        let loopDicts (actual:Map<HttpMethod, RouteNode>) (expected:Map<HttpMethod, RouteNode>) actualPathDescription expectedPathDescription action =
            for actualChild in actual do
                let key = actualChild.Key
                let expectedChild = expected.TryFind(key)
                let actualPath = actualPathDescription + "/" + key
                let expectedPath = expectedPathDescription + "/" + key
                match expectedChild with
                | None -> printf "Missing child at path: actual: %A - expected: %A" actualPath expectedPath
                          test <@ false @> 
                | Some(expectedChild) -> action actualChild.Value expectedChild actualPath expectedPath

        let rec compareNode1 (actual:Map<HttpMethod, RouteNode>) (expected:Map<HttpMethod, RouteNode>) actualPathDescription expectedPathDescription =
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
                                verify <@ actual.restrictions.IsNone = expected.restrictions.IsNone @> actualPath expectedPath                                   
                              )

        let rec compareNode (actual: RouteNode) (expected: RouteNode) actualPath expectedPath =
            verify <@ actual.pathItem = expected.pathItem @> actualPath expectedPath
            verify <@ actual.preAction.IsNone = expected.preAction.IsNone @> actualPath expectedPath
            verify <@ actual.preAction.IsSome = expected.preAction.IsSome @> actualPath expectedPath
            verify <@ actual.children.Count = expected.children.Count @> actualPath expectedPath
            verify <@ actual.templateChildren.Count = expected.templateChildren.Count @> actualPath expectedPath
            verify <@ actual.restrictions.IsNone = expected.restrictions.IsNone @> actualPath expectedPath                                   

            let actualChildren = actual.children.Select(fun keyValue -> (keyValue.Key, keyValue.Value)) |> Map.ofSeq
            let expectedChildren = expected.children.Select(fun keyValue -> (keyValue.Key, keyValue.Value)) |> Map.ofSeq
            loopDicts actualChildren expectedChildren actualPath expectedPath compareNode

        verify <@ actual.Count = expected.Count @> "/" "/"
        loopDicts actual expected "/" "/" compareNode
        ()