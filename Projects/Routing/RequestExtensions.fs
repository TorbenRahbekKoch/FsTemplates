namespace Routing
[<AutoOpen>]
module RequestExtensions =
    open System.Collections.Generic
    open System.Globalization
    open System.Linq
    open Microsoft.Owin
    
    [<Literal>]
    let ``Upper case header key`` = "FsTemplates.UpperCaseHeaders"

    type IOwinRequest with
    
    member this.UpperCaseHeaders() =
        let found, cached = this.Environment.TryGetValue(``Upper case header key``)
        if found then
            cached :?> IDictionary<string, string[]>
        else
            let t = this.Headers    
                    |> Seq.map(fun header -> 
                        (header.Key.ToUpper CultureInfo.InvariantCulture, 
                         header.Value |> Array.map(fun value -> value.ToUpper CultureInfo.InvariantCulture)))
                    |> dict
            this.Environment.[``Upper case header key``] <- t
            t

    member this.header (headerName: string) =
        let upperCaseHeaderName = headerName.ToUpper(CultureInfo.InvariantCulture)
        this.UpperCaseHeaders().[upperCaseHeaderName]

    member this.hasHeader (headerName: string) =
        let upperCaseHeaderName = headerName.ToUpper(CultureInfo.InvariantCulture)
        this.UpperCaseHeaders().ContainsKey upperCaseHeaderName

    member this.hasHeaderWith (headerName: string) (headerValue: string) =
        let upperCaseHeaderName = headerName.ToUpper(CultureInfo.InvariantCulture)
        let upperCaseHeaderValue = headerValue.ToUpper(CultureInfo.InvariantCulture)
        let found, value = this.UpperCaseHeaders().TryGetValue upperCaseHeaderName
        found && value.Any(fun v -> v.Contains(upperCaseHeaderValue))

