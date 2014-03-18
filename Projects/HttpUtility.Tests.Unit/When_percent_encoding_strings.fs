namespace HttpUtility.Tests.Unit
#if INTERACTIVE
#r "../../packages/Unquote.2.2.2/lib/net40/unquote.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#endif    

open System
open System.Linq
open System.Collections.Generic
open NUnit.Framework
open Swensen.Unquote
open HttpUtility

[<TestFixture>]
type ``When percent encoding strings``() = 
    let unreservedCharacters = List.ofSeq ['a'..'z'] @ ['A'..'Z'] @ ['0'..'1'] @ ['-'; '_'; '.'; '~']
                               |> List.toArray |> (fun s -> System.String s) 
    
    let reservedCharacters = List.ofSeq ['%'; '!'; '*'; '\''; '('; ')'; ';'; ':'; '@'; '&'; '='; '+'; '$'; '/'; '?'; '#'; '['; ']']
                             |> List.toArray |> (fun s -> System.String s) 

    [<Test>]
    member x.``Verify that unreserved characters are not converted``() = 
        let expected = unreservedCharacters
        let actual = percentEncode unreservedCharacters
        test <@ expected = actual @>

    [<Test>]
    member x.``Verify that reserved characters are all converted``() = 
        let expected = "%25%21%2a%27%28%29%3b%3a%40%26%3d%2b%24%2f%3f%23%5b%5d"
        let actual = percentEncode reservedCharacters
        test <@ expected = actual @>

        test <@ percentDecode "a%25b" = "a%b"@>
        test <@ percentDecode "abc%c3%a6%c3%b8%c3%a5" = "abcæøå" @>
