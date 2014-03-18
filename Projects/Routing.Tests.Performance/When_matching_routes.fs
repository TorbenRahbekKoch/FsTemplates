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
open Routing
open Routing.Tests.Unit.BushHelpers

[<TestFixture>]
type ``When matching routes``() = 

    member x.buildrequestContexts() =
        let child =  { defaultRouteNode() with pathItem = createPathItem("path") }
        let root = createDefaultRoot [child]
        let expected = defaultBushWithRoot "GET" root


        let paths = [ for x in 0..99 -> "/path" + x.ToString()] 
                    |> List.collect (fun p -> [for l in 'a'..'y' -> p + "/path" + l.ToString()])
        
        let routes = [ for x in 1..2500 -> GET paths.[x %  2500] |> view "Text" ]
                   @ [ for x in 1..2500 -> POST paths.[x % 2500] |> view "Text" ]
                   @ [ for x in 1..2500 -> PUT paths.[x %  2500] |> view "Text" ]
                   @ [ for x in 1..2500 -> DELETE paths.[x % 2500] |> view "Text" ]
        let bush = {bush = buildMethodBush routes }
        
        let request = Microsoft.Owin.OwinRequest()
        request.Scheme <- "http"
        request.Host <- HostString("localhost")
        request.PathBase <- PathString("/")
        request.Path <- PathString("/path0/subpath1/subpath")

        request.Method <- "GET"
        let getContexts = [ for x in 1..2500 -> 
                            request.Path <- PathString(paths.[x % 2500]) 
                            ``from context`` (Microsoft.Owin.OwinContext(request.Environment))]

        request.Method <- "PUT"
        let putContexts = [ for x in 1..2500 -> 
                            request.Path <- PathString(paths.[x % 2500]) 
                            ``from context`` (Microsoft.Owin.OwinContext(request.Environment))]
        let putContext = Microsoft.Owin.OwinContext(request.Environment)        

        request.Method <- "DELETE"
        let deleteContexts = [ for x in 1..2500 -> 
                               request.Path <- PathString(paths.[x % 2500]) 
                               ``from context`` (Microsoft.Owin.OwinContext(request.Environment))]
        let deleteContext = Microsoft.Owin.OwinContext(request.Environment)        

        request.Method <- "POST"
        let postContexts = [ for x in 1..2500 -> 
                             request.Path <- PathString(paths.[x % 2500]) 
                             ``from context`` (Microsoft.Owin.OwinContext(request.Environment))]

        let requestContexts = getContexts
                            @ putContexts
                            @ postContexts
                            @ deleteContexts
        bush, requestContexts.ToArray()

    member x.matchXTimes matchCount = 
        let bush, requestContexts = x.buildrequestContexts()
        let watch = Stopwatch()
        watch.Start()
        for count in 0..(matchCount-1) do
            bush.matchRequest (requestContexts.[count]) |> ignore            
        watch.Stop()
        double watch.ElapsedMilliseconds

    [<Test>]
    member x.``Verify 10000 matches in 100ms is possible``() = 
        let elapsedMilliSeconds = x.matchXTimes 9999
        test <@ elapsedMilliSeconds <= 100.0 @>
        
        printfn "Actual time: %A" elapsedMilliSeconds
