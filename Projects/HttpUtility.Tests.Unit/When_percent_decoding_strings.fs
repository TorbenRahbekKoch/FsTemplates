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
type ``When percent decoding strings``() = 
    let unreservedCharacters = List.ofSeq ['a'..'z'] @ ['A'..'Z'] @ ['0'..'1'] @ ['-'; '_'; '.'; '~']
                               |> List.toArray |> (fun s -> System.String s) 
    
    let reservedCharacters = List.ofSeq ['%'; '!'; '*'; '\''; '('; ')'; ';'; ':'; '@'; '&'; '='; '+'; '$'; '/'; '?'; '#'; '['; ']']
                             |> List.toArray |> (fun s -> System.String s) 

    [<Test>]
    member x.``Verify that reserved characters are converted back``() = 
        let expected = "%!*'();:@&=+$/?#[]"
        let actual = percentDecode "%25%21%2a%27%28%29%3b%3a%40%26%3d%2b%24%2f%3f%23%5b%5d"
        test <@ expected = actual @>

    [<Test>]
    member x.``Verify that unreserved characters are not converted``() = 
        let expected = unreservedCharacters
        let actual = percentDecode unreservedCharacters
        test <@ expected = actual @>

//test <@ percentEncode "a" = "a"@>
//test <@ percentEncode "!" = "%21"@>
//test <@ percentEncode "abcæøå" = "abc%c3%a6%c3%b8%c3%a5"@>
//test <@ percentEncode "abcæøå" |> percentDecode = "abcæøå"@>
//test <@ let original = "http://stackoverflowæ.com/questionsø/3170523å/converting-unicode-character-to-a-single-hexadecimal-value-in-c-sharp%"
//        percentEncode original
//        |> percentDecode = original
//     @>
//
//
//[1..1000000] |> List.iter (fun i -> percentEncode "a!bcæøå" |> ignore)
//[1..1000000] |> List.iter (fun i -> percentEncode "http://stackoverflowæ.com/questionsø/3170523å/converting-unicode-character-to-a-single-hexadecimal-value-in-c-sharp%" |> ignore)
//[1..1000000] |> List.iter (fun i -> percentEncode "http://stackoverflow.com/questions/3170523/converting-unicode-character-to-a-single-hexadecimal-value-in-c-sharp" |> ignore)
//
