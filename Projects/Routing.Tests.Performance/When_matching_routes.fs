namespace Routing.Tests.Performance
#if INTERACTIVE
#r "packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "packages/NUnit.2.6.2/lib/nunit.framework.dll"
#endif    

open System
open System.Diagnostics
open System.Linq
open System.Collections.Generic
open Microsoft.Owin
open NUnit.Framework
open Swensen.Unquote
open Routing.RequestContext
open Routing.Routing
open Routing.Tests.Unit.BushHelpers

[<TestFixture>]
type ``When matching routes``() = 

    [<Test>]
    member x.``Verify 5000 matches in 100ms is possible``() = 
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

        let watch = Stopwatch()
        watch.Start()
        for count in 1..5000 do
            bush.matchRequest requestContext |> ignore

        watch.Stop()
        test <@ watch.ElapsedMilliseconds < 100L @>
        
        printf "Actual time: %A" watch.ElapsedMilliseconds