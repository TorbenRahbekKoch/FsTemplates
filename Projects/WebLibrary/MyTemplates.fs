module MyTemplates
open System.Collections.Generic
open Routing.RequestContext
open Routing.Routing
open Templating


type MyTemplateContainer(physicalTemplateRoot: string) =
    inherit TemplateContainer(physicalTemplateRoot) 
    let physicalTemplateRoot = physicalTemplateRoot
    let angularJsBundle = scriptsBottom
                           [ "/Scripts/angular.js" ]
    let todoAppBundle = scriptsBottom
                         [ "/Scripts/Todos/TodoService.js"
                           "/Scripts/Todos/ITodoScope.js";
                           "/Scripts/Todos/TodoItem.js";
                           "/Scripts/Todos/TodoController.js";                           
                           "/Scripts/Todos/App.js"; ]
    let scriptBundles = [angularJsBundle;todoAppBundle; ]

    let styleBundles = [styles ["/Content/Styles/base.css"]]

    let allTemplates = 
        templates  
            [ fileTextTemplate "todos" 
                (physicalTemplateRoot + "/todos.html") 
                (actions 
                    [ replace "{CurrentTime}" (fun ctx -> System.DateTime.Now.ToString());] 
                    @ scriptBundles
                    @ styleBundles)
            ]
            |> List.map(fun item -> (item.name, item))
            |> dict

    member self.ExecuteTemplate (template: Template) (requestContext: RequestContext) =
        template.document.Execute requestContext

    member self.Item name =
        allTemplates.Item(name)

let templateAction (container: MyTemplateContainer) (template:Template) =
    fun (r: RequestContext) ->        
        r.context.Response.Write(container.ExecuteTemplate template r)
    
let action1 (action:RequestContext -> unit) (route: Route) =
    { route with action = Some(action)}

let template (container: MyTemplateContainer) name (route: RouteEntry) =
    match route with
    | Route(route)          -> Route {route with action = Some(templateAction container (container.Item(name)))}
    | RouteGroup(group)     -> failwith ("Cannot use template on RouteGroup: " + name)
    | RequestPipeline(_)    -> failwith ("Cannot use template on RequestPipeline: " + name)
    
