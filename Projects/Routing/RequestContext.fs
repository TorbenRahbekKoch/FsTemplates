namespace Routing
module RequestContext = 
    open Microsoft.FSharp.Linq.NullableOperators
    open System.Collections.Generic
    open System.IO
    open System.Text
    open Microsoft.Owin
    open StatusCode

    /// Context used by actions
    [<NoComparison>]
    type RequestContext = {
        context : IOwinContext;
        /// Name/value pairs for matched templates
        templateValues : Dictionary<string, string>;
    } with 
    member this.OK content =   
       this.context.Response.StatusCode <- OK
       this.Write content

    member this.Created content =   
       this.context.Response.StatusCode <- Created
       this.Write content

    member private this.Write (content: string) =
        let bytes = Encoding.UTF8.GetBytes(content)
        this.context.Response.ContentLength <- System.Nullable (int64 bytes.Length)
        this.context.Response.Write(bytes)// <- new MemoryStream(bytes)
        
