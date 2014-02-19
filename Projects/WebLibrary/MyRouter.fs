namespace WebLibrary
open System
open System.Threading.Tasks
open Routing.Routing
open MyTemplates

type MyRouter(applicationRootDirectory: string) = 
    inherit Router()    
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

    member private self.template name route =
        template templateContainer name route

    override self.Register() = 
        routes[
            GET "/{id1}/{id2}" 
                |> action (myIdView()) 
                |> restrict (fun request -> true);
            GET "/test1" 
                |> view (myView()) 
                |> restrict (fun request -> true);
            GET "" 
                |> view "<html><body><h1>Root Page!</h1></body></hmtl>";
            GET "/test2"
                |> view "<html><body><h1>Hello world</h1></body></html>";
            GET "/test3"
                |> view "<html><body><h3>Hello world</h3></body></html>";
            GET "/test4" 
                |> action (myUpdatingView())
                |> restrict (fun request -> true);
            GET "/test5/"
                |> view "<html><body><h3>Hello world - test 5</h3></body></html>";
            GET "/test5/test"
                |> view "<html><body><h3>Hello world - test 5/test</h3></body></html>";

            GET "/todos/"
                |> self.template "todos"

//            GET "/api/todos"
//                |> action ""
        ]

