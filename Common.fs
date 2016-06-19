module Common

open System.IO

let launchDebugger () = System.Diagnostics.Debugger.Launch () |> ignore

let pathDir path =
    FileInfo(path).Directory.FullName

let pathCombine x y = 
    Path.Combine (x, y)

let (|ValidFilePath|_|) path =
    match not (Directory.Exists path) && File.Exists path with
    | true -> Some path
    | false -> None

let (|NotEmpty|_|) = function 
    | s when not (System.String.IsNullOrWhiteSpace s) -> Some s
    | _ -> None

let tryParseBool = bool.TryParse >> function
    | true, v -> Some v
    | false, _ -> None
   
let safe f x = 
    try f x |> Some with _ -> None

[<AutoOpen>]
module OptionUtils = 
    let (>>=) o f = Option.bind f o
    let (<!>) o f = Option.map f o

    type OptionBuilder() =
        member this.Bind(m, f) = Option.bind f m
        member this.Return v = Some v
        member this.ReturnFrom v = v

    let opt = OptionBuilder()

[<AutoOpen>]
module Xml =
    open System.Xml.Linq

    let xName n = 
        n |> XName.Get

    let xElementValue (el:XElement) =
        el.Value

    let xAttrValue (a:XAttribute) =
        a.Value

    let xChildElements name (el:XContainer) = 
        match name |> xName |> el.Elements |> List.ofSeq with
        | [] -> None
        | items -> Some (items :> XElement seq)

    let xChildElement name (el:XContainer) =
        match name |> xName |> el.Element with
        | null -> None
        | el -> Some el

    let xAttr name (el:XElement) =
        match name |> xName |> el.Attribute with
        | null -> None
        | a -> Some a

    let xChildValue name (el:XElement) =
        el |> xChildElement name <!> xElementValue

    let xAttributeValue name (el:XElement) =
        el |> xAttr name <!> xAttrValue

    let readXml path = 
        try
            path
            |> File.ReadAllText
            |> XDocument.Parse
            |> Some
        with _ ->
            None
