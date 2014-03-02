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
open BushHelpers

[<TestFixture>]
type ``When adding routes_with_queries``() = 

    [<Test>]
    member x.``Verify that when adding one path the bush contains one root with one child``() = 
        let queries = Dictionary<string, string>()
        queries.Add("id", "{id}");
        let child = { defaultRouteNode() with pathItem = createPathItem("path1"); queries = Some(queries) }
        let root = createDefaultRoot [child]
        let expected = defaultBushWithRoot "GET" root

        let group = [ GET "/path1?id={id}" 
                        |> view "Test";
                    ]
        let actual = buildMethodBush group

        compareBushes actual expected;
