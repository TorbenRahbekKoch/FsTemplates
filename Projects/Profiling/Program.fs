// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open Routing.Tests.Performance

[<EntryPoint>]
let main argv = 
    let t = ``When matching routes``()
    let times = [t.matchXTimes 10000;
                 t.matchXTimes 10000;
                 t.matchXTimes 10000;
                 t.matchXTimes 10000;
                 t.matchXTimes 10000;]
    printfn "Average: %A " (List.average times)
    //System.Console.ReadLine() |> ignore

    0 // return an integer exit code
    