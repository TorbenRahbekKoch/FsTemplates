namespace Routing.Tests.Unit
#if INTERACTIVE
#r "../../packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#endif    

open System
open System.Linq
open System.Collections.Generic
open NUnit.Framework
open Swensen.Unquote
open Routing
open BushHelpers

[<TestFixture>]
type ``When adding routes``() = 

    [<Test>]
    member x.``Verify that when adding one path the bush contains one root with one child``() = 
        let child =  { defaultRouteNode() with pathItem = createPathItem("path") }
        let root = createDefaultRoot [child]
        let expected = defaultBushWithRoot "GET" root

        let routes = [ GET "/path" |> view "Text"]
        let actual = buildMethodBush routes

        compareBushes actual expected;