module MyTemplates
open System.Collections.Generic
open Routing
open Templating
open TextTemplate
open Template

type TemplateState(physicalTemplateRoot: string) = 
    let angularJsBundle = scriptsBottom
                           [ "/Scripts/angular.js" ]
    let todoAppBundle = scriptsBottom
                         [ "/Scripts/Todos/WebSocket.js"
                           "/Scripts/Todos/TodoService.js"
                           "/Scripts/Todos/ITodoScope.js"
                           "/Scripts/Todos/TodoItem.js"
                           "/Scripts/Todos/TodoController.js"
                           "/Scripts/Todos/App.js" ]
    let scriptBundles = [angularJsBundle;todoAppBundle; ]

    let styleBundles = [styles ["/Content/Styles/base.css"]]

    member self.PhysicalTemplateRoot with get() = 
                                        physicalTemplateRoot

    member self.Templates with get() = 
                            [   fileTextTemplate "todos" 
                                    (physicalTemplateRoot + "/Content/todos.html") 
                                    (actions 
                                        [ replace "{CurrentTime}" (fun ctx -> System.DateTime.Now.ToString());] 
                                        @ scriptBundles
                                        @ styleBundles);
                                fileTextTemplate "websockets"
                                    (physicalTemplateRoot + "/Content/websockets.html")
                                    (actions []);
                            ]


type MyTemplateContainer(templateState: TemplateState) =
    inherit TemplateContainer(templateState.PhysicalTemplateRoot, templateState.Templates) 

