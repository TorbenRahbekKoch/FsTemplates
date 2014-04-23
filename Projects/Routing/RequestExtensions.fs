namespace Routing
[<AutoOpen>]
module RequestExtensions =
    open System.Linq
    open Microsoft.Owin

    type IOwinRequest with
    member this.header headerName =
        this.Headers.[headerName]
    member this.hasHeader headerName =
        this.Headers.ContainsKey headerName
    member this.hasHeaderWith headerName headerValue =
        let found, value = this.Headers.TryGetValue headerName
        found && value.Any(fun v -> v.Contains(headerValue))

