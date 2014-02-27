﻿namespace WebLibrary
open System
open System.IO
open System.Collections.Generic
open System.Threading.Tasks
open Routing.RequestContext
open Routing.Routing
open Newtonsoft.Json
open MyTemplates

type TodoItem = {
    title : string;
    completed : bool;
}

type MyRouter(applicationRootDirectory: string) = 
    inherit Router()    
    let todos = List<TodoItem>()
    let templateContainer = MyTemplateContainer(applicationRootDirectory)
    let myView() = 
        String.Format("<html><body><h1>Hello world - the time is {0}</h1></body></html>", DateTime.UtcNow)

    let myUpdatingView() = 
        fun (r: RequestContext) -> 
            r.context.Response.Write(String.Format("<html><body><h1>Hello world - the time is {0}</h1></body></html>", DateTime.UtcNow))

    let myIdView() = 
        fun (r: RequestContext) -> 
            r.context.Response.Write(
                String.Format("<html><body><h1>Hello world - the ids are {0}, {1}</h1></body></html>", 
                    r.templateValues.["id1"], r.templateValues.["id2"]))
    do
        let item = {title = "My Todo Item"; completed= false} 
        todos.Add item

    member private self.template name route =
        template templateContainer name route

    override self.Register() =
        routes [
            group "/" [
//                GET "/{id1}/{id2}" 
//                    |> action (myIdView()) 
//                    |> restrict (fun request -> true)
//                GET "/test1" 
//                    |> view (myView()) 
//                    |> restrict (fun request -> true)
//                GET "" 
//                    |> view "<html><body><h1>Root Page!</h1></body></hmtl>"
//                GET "/test2"
//                    |> view "<html><body><h1>Hello world</h1></body></html>"
//                GET "/test3"
//                    |> view "<html><body><h3>Hello world</h3></body></html>"
//                GET "/test4" 
//                    |> action (myUpdatingView())
//                    |> restrict (fun request -> true)
//                GET "/test5/"
//                    |> view "<html><body><h3>Hello world - test 5</h3></body></html>";
//                GET "/test5/test"
//                    |> view "<html><body><h3>Hello world - test 5/test</h3></body></html>";

                GET "/todos/"
                    |> self.template "todos";
        
                (group "/api/"
                [
                    GET "/todos/"
                        |> action (fun ctx ->
                            //ctx.AsJson(todos)
                            //ctx.AsXml(todos)
                            let json = JsonConvert.SerializeObject(todos)
                            ctx.OK json)
                    POST "/todos/"
                        //|> restrict (fun ctx -> ctx.context.Request.Headers.ContentLength < 1000)
                        |> restrict (fun ctx -> ctx.context.Request.ContentType.Contains "application/json")
                        |> action (fun ctx ->
                            let textReader = new StreamReader(ctx.context.Request.Body, System.Text.Encoding.UTF8)
                            let todoItem = JsonConvert.DeserializeObject<TodoItem>(textReader.ReadToEnd())
                            todos.Add(todoItem)
                            ctx.Created "")
                ]
                |> restrict (fun ctx -> ctx.context.Request.ContentType.Contains "application/json"));]]
            
//    override self.Register() = 
//        []
//        routes[
//            GET "/{id1}/{id2}" 
//                |> action (myIdView()) 
//                |> restrict (fun request -> true)
//            GET "/test1" 
//                |> view (myView()) 
//                |> restrict (fun request -> true)
//            GET "" 
//                |> view "<html><body><h1>Root Page!</h1></body></hmtl>"
//            GET "/test2"
//                |> view "<html><body><h1>Hello world</h1></body></html>"
//            GET "/test3"
//                |> view "<html><body><h3>Hello world</h3></body></html>"
//            GET "/test4" 
//                |> action (myUpdatingView())
//                |> restrict (fun request -> true)
//            GET "/test5/"
//                |> view "<html><body><h3>Hello world - test 5</h3></body></html>";
//            GET "/test5/test"
//                |> view "<html><body><h3>Hello world - test 5/test</h3></body></html>";
//
//            GET "/todos/"
//                |> self.template "todos";
//
//            GET "/api/todos/"
//                |> action (fun ctx ->
//                    //ctx.AsJson(todos)
//                    //ctx.AsXml(todos)
//                    let json = JsonConvert.SerializeObject(todos)
//                    ctx.OK json)
//            POST "/api/todos/"
//                //|> restrict (fun ctx -> ctx.context.Request.Headers.ContentLength < 1000)
//                |> restrict (fun ctx -> ctx.context.Request.ContentType.Contains "application/json")
//                |> action (fun ctx ->
//                    let textReader = new StreamReader(ctx.context.Request.Body, System.Text.Encoding.UTF8)
//                    let todoItem = JsonConvert.DeserializeObject<TodoItem>(textReader.ReadToEnd())
//                    todos.Add(todoItem)
//                    ctx.Created "")
//            ]
//
//            group "/api/"
////                |> restrict (fun ctx -> ctx.context.Request.ContentType = "application/json")
////                |> action (fun ctx -> ctx.templateValues.Add("Test", "Test"))
//                
//                [GET ["/api/todos/{id}"; "/api/todos/?id={id}"]
//                    |> action (fun ctx -> ctx.OK "TODO!");
//
//                GET "/api/todos/?id={id}"
//                    |> action (fun ctx -> ctx.OK "TODO!");
//
//                GET "/api/todos/"
//                    |> action (fun ctx ->
//                        let json = JsonConvert.SerializeObject(todos)
//                        ctx.OK json)
//                POST "/api/todos/"
//                    //|> restrict (fun ctx -> ctx.context.Request.Headers.ContentLength < 1000)
//                    //|> restrict (fun ctx -> ctx.context.Request.ContentType = "application/json")
//                    |> action (fun ctx ->
//                        let textReader = new StreamReader(ctx.context.Request.Body, System.Text.Encoding.UTF8)
//                        let todoItem = JsonConvert.DeserializeObject<TodoItem>(textReader.ReadToEnd())
//                        todos.Add(todoItem)
//                        ctx.Created "")]
//
////            WEBSOCKET "/api/observers/{id}"
////                |> 
//        ]
//
