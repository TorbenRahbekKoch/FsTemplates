namespace Routing
[<AutoOpen>]
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
        //authenticator: Security.IAuthenticationManager option
        request  : IOwinRequest
        response : IOwinResponse
        context  : IOwinContext;

        /// Name/value pairs for matched templates
        templateValues : Dictionary<string, string>;
    } with 
    member this.OK content =   
       this.context.Response.StatusCode <- OK
       this.Write content

    member this.Created content =   
       this.context.Response.StatusCode <- Created
       this.Write content

    member self.Method =
        self.context.Request.Method

    member self.Uri =
        self.context.Request.Uri

    member private this.Write (content: string) =
        let bytes = Encoding.UTF8.GetBytes(content)
        this.response.ContentLength <- System.Nullable (int64 bytes.Length)
        this.response.Write(bytes)
        
    member private this.WriteAsync (content: string) =
        let bytes = Encoding.UTF8.GetBytes(content)
        this.response.ContentLength <- System.Nullable (int64 bytes.Length)
        this.response.WriteAsync(bytes)
        
[<AutoOpen>]
module RequestContexts = 
    open Microsoft.Owin
    open System.Collections.Generic

    let ``from context`` (context: IOwinContext) = 
        { context = context; request = context.Request; response = context.Response; templateValues = Dictionary<string, string>() }

    let ``with context and values`` (context: IOwinContext) (templateValues: Dictionary<string, string>)= 
        { context = context; request = context.Request; response = context.Response; templateValues = templateValues }

