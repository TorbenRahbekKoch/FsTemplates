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
open Routing.RequestContext
open Routing.Routing
open BushHelpers
open Microsoft.Owin

[<TestFixture>]
type ``When matching routes``() = 
    [<Test>]
    member x.``Verify that one simple route is matched correctly``() = 
        let child =  { defaultRouteNode() with pathItem = createPathItem("path") }
        let root = createDefaultRoot [child]
        let expected = defaultBushWithRoot "GET" root

        let routes = [ GET "/path" |> view "Text"]
        let bush = {bush = buildMethodBush routes }
        
        let request = Microsoft.Owin.OwinRequest()
        request.Method <- "GET"
        request.Scheme <- "http"
        request.Host <- HostString("localhost")
        request.PathBase <- PathString("/")
        request.Path <- PathString("/path")

        let context = Microsoft.Owin.OwinContext(request.Environment)        

        let requestContext = { context = context; templateValues = Dictionary<string, string>() }

        let matchContext = bush.matchRequest requestContext

        test <@ Option.isSome matchContext @>

