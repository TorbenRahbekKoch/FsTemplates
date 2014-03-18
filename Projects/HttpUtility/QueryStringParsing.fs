namespace HttpUtility
[<AutoOpen>]
module QueryStringParsing =
    open System
    open System.Text
    open System.Collections.Generic
    open Encoding

    /// Adds a key, value pair to an existing Dictionary. Allows for multiple
    /// entries for the same key
    let addItem name value (items: Dictionary<string, List<string>>) =
        let found, entry = items.TryGetValue(name)
        if (not found) then
            let entry = List<string>()
            entry.Add(value)
            items.Add(name, entry)
        else
            entry.Add(value)

    // Parses a query string that has either no "weird" characters in 
    // Assumes that the queryString given is only the actual query string and not
    // any part of url or bookmark 
    // This implementation allows multiple parameters with same name
    let parseQueryString (queryString: string) =
        let actualQuery = if queryString.StartsWith "?" then
                              if queryString.Length > 1 then
                                  queryString.Substring 1
                              else
                                  ""  
                          else 
                              queryString
        let queryItems = actualQuery.Split([|'&'; ';'|], StringSplitOptions.RemoveEmptyEntries)
        let result = Dictionary<string, List<string>>()
        for index = 0 to queryItems.Length-1 do
            let parts = queryItems.[index].Split('=')
            addItem (parts.[0]) (percentDecode(parts.[1])) result
        result
