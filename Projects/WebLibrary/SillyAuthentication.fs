module SillyAuthentication
open Routing

let authenticate (context: RequestContext) =
    if (context.request.User <> null 
        && context.request.User.Identity <> null 
        && context.request.User.Identity.IsAuthenticated) then
        true
    else
        context.response.StatusCode <- 401
        context.response.Write("Get lost!")
        false        