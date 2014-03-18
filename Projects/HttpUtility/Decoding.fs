namespace HttpUtility
[<AutoOpen>]
module Decoding = 
    open System.Text
    open System
    open System.Text
    open System.Collections.Generic

    let percentDecode (value: string) =
        let utf8Decoder = Encoding.UTF8

        let decodeChars (value: string) pos =
            let byteCollector = List<byte>()
            let mutable index = pos
            while ((index < value.Length) && (value.[index] = '%')) do
                let hexPair = value.Substring(index + 1, 2)
                let byte = Convert.ToByte(hexPair, 16)
                byteCollector.Add byte
                index <- index + 3
            utf8Decoder.GetString(byteCollector.ToArray()), index

        let rec m (value:string) index (result: StringBuilder) =
            let pos = value.IndexOf('%', index)
            if (pos >= index) then
                let decodedChars, newPos = decodeChars value pos
                result.Append(value.Substring(index, pos-index))
                      .Append(decodedChars) |> ignore
                m value newPos result
            else
                result.Append(value.Substring(index))

        let result = (m value 0 (StringBuilder()))
        result.ToString()

