module Dictionary

open System.Collections.Generic
open System.Linq
open Swensen.Unquote
open HttpUtility

let ``create expected`` (items: string list) =
    let rec createPairs items = 
        match items with
        | [] -> []
        | key::value::rest -> (key,value)::createPairs rest
        | key::rest -> failwith "Uneven number of items"
    let dictionary = Dictionary<string, List<string>>()
    createPairs items 
    |> Seq.iter(fun item -> addItem (fst item) (snd item) dictionary)
    dictionary

let ``isEqualTo`` (dict1: Dictionary<string, List<string>>) (dict2: Dictionary<string, List<string>>) =
    let dict1HasExcessKeys = dict1.Keys.Except(dict2.Keys).Any()
    let dict2HasExcessKeys = dict2.Keys.Except(dict1.Keys).Any()
    if dict1HasExcessKeys || dict2HasExcessKeys then
        false
    else
        let differentKeys = dict1.Keys |> Seq.filter(fun dict1Item ->
            let dict1Key = dict1Item
            let dict1ValueCount = dict1.[dict1Key]
            let dict2ValueCount = dict2.[dict1Key]
            dict1ValueCount = dict2ValueCount
            )
        if differentKeys.Any() then
            raise (AssertionFailedException("Dictionaries not equal"))
        else
            true
         

//type IDictionary<'Key, 'Value> when 'Value :> List<string> with
//    member x.isEqualTo (other: Dictionary<'Key, 'Value>) =
//        if x.Except(other).Count() = 0 then
//            true
//        else
//            let diff = x.Except(other).ToString()
//            raise (AssertionFailedException("Dictionaries not equal: " + diff))
//
////    static member (====) (right: Dictionary<'a, 'b>, left: Dictionary<'a, 'b>) =
////        if right.Except(left).Count() = 0 then
////            true
////        else
////            raise (AssertionFailedException("Dictionaries not equal"))
////        
