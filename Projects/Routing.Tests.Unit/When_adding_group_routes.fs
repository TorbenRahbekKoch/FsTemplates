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
type ``When adding group routes``() = 
    [<Test>]
    member x.``Verify that when adding one group with one child the bush contains one root with one child``() = 
        let child =  { defaultRouteNode() with pathItem = createPathItem("path") }

        let root = createDefaultRoot [child]
        let expected = defaultBushWithRoot "GET" root

        let group = [ group "/" [
                        GET "/path" 
                        |> view "Test"]]
        let actual = buildMethodBush group

        compareBushes actual expected;

    [<Test>]
    member x.``Verify that when adding one group with two children the bush contains one root with two children``() = 
        let child1 = { defaultRouteNode() with pathItem = createPathItem("path1") }
        let child2 = { defaultRouteNode() with pathItem = createPathItem("path2") }
        let root = createDefaultRoot [child1; child2]
        let expected = defaultBushWithRoot "GET" root

        let group = [ group "/" [
                        GET "/path1" 
                        |> view "Test";
                        GET "/path2"
                        |> view "Test"]]
        let actual = buildMethodBush group

        compareBushes actual expected;

    [<Test>]
    member x.``Verify that when adding two groups with one child each the bush contains one root with two children``() = 
        let child1 = { defaultRouteNode() with pathItem = createPathItem("path1") }
        let child2 = { defaultRouteNode() with pathItem = createPathItem("/api/path2") }
        let root = createDefaultRoot [child1; child2]
        let expected = defaultBushWithRoot "GET" root

        let group = [ group "/" [
                        GET "/path1" 
                        |> view "Test"];
                      group "/api" [
                        GET "/path2"
                        |> view "Test"]]
        let actual = buildMethodBush group
        compareBushes actual expected;

    [<Test>]
    member x.``Verify that when adding a group with one child group the bush contains one root with one child``() = 
        let child = { defaultRouteNode() with pathItem = createPathItem("/api/path") }
        let root = createDefaultRoot [child]
        let expected = defaultBushWithRoot "GET" root

        let group = [ group "/" [
                        group "/api" [
                            GET "/path"
                            |> view "Test"]]]
        let actual = buildMethodBush group
        compareBushes actual expected;
    