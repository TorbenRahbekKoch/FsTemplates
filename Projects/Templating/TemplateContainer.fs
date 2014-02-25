module Templating 
open System
open System.Collections.Generic
open System.IO
open System.Xml.Linq
open System.Xml.XPath
open Routing.RequestContext
open Routing.Routing

//type TemplateDocument =
//| Text of string
//| Xml of XDocument    

type TextTemplateAction =
| ReplaceText of searchFor: string * replaceWith: (RequestContext -> string)
| InsertBefore of searchFor: string * insertText: (RequestContext -> string)
| InsertAfter of searchFor: string * insertText: (RequestContext -> string)


type XmlTemplateActionType = 
| AppendText of string 
| AppendChild of string
| Repeat of string * Dictionary<string, XmlTemplateActionType>

//    type XDocument with
//        member this.ApplyDataItem (actions: Dictionary<string, XmlTemplateAction>) (itemName: string) (data: Object) =
//            let action = actions.Item(itemName)
//            match action with
//                | AppendChild xpath -> this.XPathSelectElement(xpath).Add(data)
//                | AppendText xpath -> this.XPathSelectElement(xpath).Add(data)        
        
type String with
//        member this.ApplyDataItem (actions: Dictionary<string, TextTemplateAction>) (itemName: string) (data: Object) =
//            let action = actions.Item(itemName)
//            match action with
//                | ReplaceText(text, action) -> this.Replace(text, action().ToString()) //|> ignore
//                | InsertBefore text -> this.Insert(this.IndexOf(text), data.ToString()) //|> ignore
//                | InsertAfter text -> this.Insert(this.IndexOf(text) + data.ToString().Length, data.ToString()) //|> ignore
    member this.ApplyDataItem (action: TextTemplateAction) (requestContext: RequestContext) =
        match action with
            | ReplaceText(text, action) -> this.Replace(text, action(requestContext).ToString()) //|> ignore
            | InsertBefore(text, action) -> this.Insert(this.IndexOf(text), action(requestContext).ToString()) //|> ignore
            | InsertAfter(text, action) -> 
                let insertText = action(requestContext).ToString()
                this.Insert(this.IndexOf(text) + text.Length, insertText.ToString()) //|> ignore
        

//type ITemplate =
//    abstract ApplyDataItem (itemName: string) data


type IActionRule =
    abstract member Evaluate : unit -> bool

type XmlTemplateAction =
    {
        name  : string
        rules : IActionRule list
        actionType : XmlTemplateActionType
    }

type XmlTemplateDocument =
    {
        xmlTemplate : XDocument
        actions : XmlTemplateAction list
    }

let replace searchFor action =
    TextTemplateAction.ReplaceText(searchFor, action)

let insertBefore searchFor action =
    TextTemplateAction.InsertBefore(searchFor, action)

let insertAfter searchFor action =
    TextTemplateAction.InsertAfter(searchFor, action)

type TextTemplateDocument =
    {
        textTemplate : string
        actions : TextTemplateAction list
    }
    member this.Execute requestContext = 
        this.actions
        |> List.fold (fun (current: string) action -> (current.ApplyDataItem action requestContext)) this.textTemplate 

    override this.ToString() =
        this.textTemplate

type TemplateDocument = 
    | XmlTemplate of XmlTemplateDocument
    | TextTemplate of TextTemplateDocument
    member this.Execute requestContext =
        match this with
        | XmlTemplate(doc) -> doc.xmlTemplate.ToString()
        | TextTemplate(doc) -> doc.Execute requestContext

    override this.ToString() =
        match this with
        | XmlTemplate(doc) -> doc.xmlTemplate.ToString()
        | TextTemplate(doc) -> doc.ToString() 

//    override this.ToString() = 
//        match this with
//        | XmlTemplate (doc, _) -> doc.ToString()
//        | TextTemplate (doc, _) -> doc.ToString()
//
//    member this.ApplyDataItem itemName data =
//        match this with
//        | XmlTemplate (doc, actions) -> doc.ApplyDataItem actions itemName data
//        | TextTemplate (doc, actions) -> doc.ApplyDataItem actions itemName data
//        this


type ITemplateRule =
    abstract member Evaluate : unit -> bool
    
type Template =
    {
        name : string
        document : TemplateDocument
        rules : ITemplateRule list //??
    }
//    with 
//        member this.ApplyDataItem(itemName:string) (data: Object) =
//            match this.document with
//            | XmlTemplate(doc) -> String.Empty
//            | TextTemplate(doc) -> doc.textTemplate.ApplyDataItem doc.actions itemName data

//    type Template(templateNameIn: string, templateDocumentIn: TemplateDocument) =
//        let templateName = templateNameIn
//        let templateDocument = templateDocumentIn

let inlineXmlDoc (doc: string) =
    XmlTemplate( {xmlTemplate = XDocument.Load(new StringReader(doc)); actions=[]})

let fileTextTemplate templateName rootRelativeFilename actions =
    {   name=templateName;
        document = TextTemplate { textTemplate = File.ReadAllText(rootRelativeFilename); actions=actions }
        rules = []
    }

let action name action =
    let dict = Dictionary<string, XmlTemplateAction>()
    dict.Add(name, action)
    dict            
    
//let actions actionItems list =
//    for actionItem in list do 
//        ;


//let action name action =
//    let dict = Dictionary<string, TextTemplateAction>()
//    dict.Add(name, action)
//    dict    

let x = 
    let rand = new Random()
    fun () -> rand.Next()

let y = System.Random().Next()

//let masterTemplate: Template = 
//    (inlineXmlDoc("<body><p>Welcome </p></body>"),
//        (actionItem "name" (AppendText "/body/p")))

//let applyDataItemToXmlTemplate (doc: XDocument)  (actions: Dictionary<string, XmlTemplateAction>) (itemName: string) (data: Object) =
//    let action = actions.Item(itemName)
//    match action with
//        | AppendChild xpath -> doc.XPathSelectElement(xpath).Add(data)
//        | AppendText xpath -> doc.XPathSelectElement(xpath).Add(data)
//    XmlTemplate(doc, actions)


//let applyDataItemToTemplate(template: Template) (itemName: string) (data: Object) =
//    match template with
//    | XmlTemplate(doc, actions) -> applyDataItemToXmlTemplate doc actions itemName data

//    let action = (snd template).Item(itemName)
//    let doc = fst template
//    match action with
//        | AppendChild xpath -> doc.XPathSelectElement(xpath).Add(data)
//        | AppendText xpath -> doc.XPathSelectElement(xpath).Add(data)
//    (doc, snd template)

//let applyDataItem(template: Template) (data: string*string) =
//    applyDataItemToTemplate template (fst data) (snd data)

//let applyData (template: Template) (data: Dictionary<string, Object>) =
//    match data with
    
let toDict<'TActionType> items =
    let dict = Dictionary<string, 'TActionType>()
    items |> Seq.iter(fun item -> dict.Add(fst item, snd item)) |> ignore
    dict

let xmlTemplate templateName doc xmlActions =
    let dictActions = xmlActions |> Seq.map(fun x -> {name = fst x; rules =[]; actionType = snd x}) 
    dictActions
    //let something = dictActions |> Seq.map|> toDict<XmlTemplateAction>
    //let t = { name = templateName; document = XmlTemplate({ xmlTemplate = doc; actions = dictActions }); rules = []}
    //t
//
//let textTemplate doc actions =
//    TextTemplate(doc, actions |> Seq.map(fun x -> (fst x, snd x)) |> toDict<TextTemplateAction>)

//let part() = 
//    xmlTemplate 
//        (inlineXmlDoc("<label id=\"username\"/>")()) 
//        [("username", (AppendChild("//label")))]



//    let part() = xmlTemplate 
//                    "My Template"
//                    (inlineXmlDoc("<label id=\"username\"/>")()) 
//                    [("username", (AppendChild("//label")))]
//let run =    
//    printfn "%A" (part())
//    let t = part()
//    printfn "1: %A" (fst t)
//    //let appliedTemplate = applyDataItem t ("username", "Kurt")
//    let appliedTemplate = applyDataItemToTemplate t "username" "Kurt"
//    printfn "T: %A" appliedTemplate
//
//    //let test = inlineXmlDoc("<label id=\"username\"/>")
//    //test.XPathSelectElement("//label").Add("Kurt")
//    printfn "2: %A" (fst (part()))
//
//    printfn "Before: %A" (fst (part()))
//    //printfn "After: %s" ((fst appliedTemplate).ToString())



let templates t = t
let actions a = a

let scriptTag scriptPath = "<script src=\"" + scriptPath + "\"></script>"
let script scriptPath = insertBefore "</head>" (fun ctx -> scriptTag scriptPath)
let scripts scriptPaths =
    let scriptBundle = 
        scriptPaths 
        |> List.map (fun path -> scriptTag path)
        |> List.fold (fun state path -> state + path) ""
    insertBefore "</head>" (fun ctx -> scriptBundle)

let scriptsBottom scriptPaths =
    let scriptBundle = 
        scriptPaths 
        |> List.map (fun path -> scriptTag path)
        |> List.fold (fun state path -> state + path) ""
    insertBefore "</body>" (fun ctx -> scriptBundle)

let styleTag stylePath = "<link rel=\"stylesheet\" href=\"" + stylePath + "\">"

let styles stylePaths = 
    let styleBundle =
        stylePaths 
        |> List.map(fun path -> styleTag path)
        |> List.fold(fun state path -> state+path) ""
    insertBefore "</head>" (fun ctx -> styleBundle)



[<AbstractClass>]
type TemplateContainer(physicalTemplateRoot: string) =
    let physicalTemplateRoot = physicalTemplateRoot
