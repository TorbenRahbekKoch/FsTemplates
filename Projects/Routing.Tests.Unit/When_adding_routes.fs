namespace Routing.Tests.Unit
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    

open System
open System.Linq
open System.Collections.Generic
open NUnit.Framework
open Swensen.Unquote
open Routing.Routing
open CompareBush

[<TestFixture>]
type ``When adding routes``() = 

    [<Test>]
    member x.``Verify that when adding one path the bush contains one root with one child``() = 
        let expected = x.expected()
        let actual = x.actual()
        compareBushes actual expected;
        //x.compareBushes actual expected
        
//        test <@ actual.Count = expected.Count @> 
//        test <@ actual.First().Key = expected.First().Key @>
//        test <@ actual.First().Value.pathItem = expected.First().Value.pathItem @>
//        test <@ actual.First().Value.preAction.IsNone = expected.First().Value.preAction.IsNone @>
//        test <@ actual.First().Value.preAction.IsSome = expected.First().Value.preAction.IsSome @>
//        test <@ actual.First().Value.children.Count = expected.First().Value.children.Count @>
//        test <@ actual.First().Value.templateChildren.Count = expected.First().Value.templateChildren.Count @>
//        test <@ actual.First().Value.predicates.IsNone = expected.First().Value.predicates.IsNone @>

//        let expectedChild = expected.First().Value.children.First().Value
//        let actualChild = actual.First().Value.children.First().Value

        
        //test <@ actualChild.pathItem = expectedChild.pathItem @>

    member x.actual(): Dictionary<HttpMethod, RouteNode> = 
        let routes = [ GET "/path" |> view "Text"]
        let bush = { bush = Dictionary<HttpMethod, RouteNode>()}
        bush.BuildMethodBush routes
        bush.bush

    member x.expected(): Dictionary<HttpMethod, RouteNode> = 
        let expected = { bush = Dictionary<HttpMethod,RouteNode>()}
        let root = { pathItem = createPathItem(""); preAction = None; action= None; 
              children = Dictionary<string, RouteNode>(); templateChildren = Dictionary<string, RouteNode>(); predicates = None}
        let child = { pathItem = createPathItem("path"); preAction = None; action= None; 
              children = Dictionary<string, RouteNode>(); templateChildren = Dictionary<string, RouteNode>(); predicates = None}
        root.children.Add("path", child)
        expected.bush.Add("GET", root)
        expected.bush
        
    member x.compareBushes (actual:Dictionary<HttpMethod, RouteNode>) (expected:Dictionary<HttpMethod, RouteNode>) = 
        test <@ actual.Count = expected.Count @> 
        test <@ actual.First().Key = expected.First().Key @>
        test <@ actual.First().Value.pathItem = expected.First().Value.pathItem @>
        test <@ actual.First().Value.preAction.IsNone = expected.First().Value.preAction.IsNone @>
        test <@ actual.First().Value.preAction.IsSome = expected.First().Value.preAction.IsSome @>
        test <@ actual.First().Value.children.Count = expected.First().Value.children.Count @>
        test <@ actual.First().Value.templateChildren.Count = expected.First().Value.templateChildren.Count @>
        test <@ actual.First().Value.predicates.IsNone = expected.First().Value.predicates.IsNone @>
        ()


