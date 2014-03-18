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
open Dictionary
open HttpUtility

[<TestFixture>]
type ``When parsing query strings``() = 
    [<Test>]
    member x.``Verify that empty query string results in empty dictionary``() = 
        let expected = 0
        let actual = (parseQueryString "").Count
        test <@ expected = actual @>

    [<Test>]
    member x.``Verify that empty query string with question mark results in empty dictionary``() =     
        let expected = 0
        let actual = (parseQueryString "").Count
        test <@ expected = actual @>

    
    [<Test>]
    member x.``Verify that single query parameter is parsed correctly``() =     
        let expected = ``create expected`` ["a";"2"]

        let actual = parseQueryString("?a=2")
        test <@ isEqualTo expected actual @>

    [<Test>]
    member x.``Verify that empty, single parameter is parsed correctly``() =
        let expected = ``create expected`` ["a";""]

        let actual = parseQueryString("?a=")
        test <@ isEqualTo expected actual @>

    [<Test>]
    member x.``Verify that empty, single parameter, ended with ; is parsed correctly``() =
        let expected = ``create expected`` ["a";""]

        let actual = parseQueryString "?a=;" 
        test <@ isEqualTo expected actual @>

    [<Test>]
    member x.``Verify that two different parameters, divided with ; is parsed correctly``() =
        let expected = ``create expected`` ["a";"1";"b";"2"]

        let actual = parseQueryString "?a=1;b=2" 
        test <@ isEqualTo expected actual @>

    [<Test>]
    member x.``Verify that two different parameters, divided with & is parsed correctly``() =
        let expected = ``create expected`` ["a";"1";"b";"2"]

        let actual = parseQueryString "?a=1&b=2" 
        test <@ isEqualTo expected actual @>

    [<Test>]
    member x.``Verify that two equally named parameters with different values are parsed correctly``() =
        let expected = ``create expected`` ["a";"1";"a";"2"]

        let actual = parseQueryString "?a=1&a=2" 
        test <@ isEqualTo expected actual @>
