using System;
using System.Reflection;
using System.Threading.Tasks;
using Microsoft.Owin;
using Owin;
using Routing;
using WebLibrary;

[assembly: OwinStartup(typeof(WebApplication.Startup))]

namespace WebApplication
{
    public class Startup
    {
        public void Configuration(IAppBuilder app)
        {
            var codebase = Assembly.GetExecutingAssembly().CodeBase;
            var binPos = codebase.IndexOf("bin");
            var root = codebase.Substring(8, binPos-8);
            
            var router = new MyRouter(root).Init();
            // For more information on how to configure your application, visit http://go.microsoft.com/fwlink/?LinkID=316888
            app.Run(async context =>
            {
                var s = System.Environment.CurrentDirectory;
                var t = context.Request.PathBase;
                await router.ExecuteRequestAsync(context);
                //await context.Response.WriteAsync(" My First OWIN App");
            });
        }
    }
}
