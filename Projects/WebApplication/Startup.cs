using System;
using System.Collections.Generic;
using System.Reflection;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Owin;
using Owin;
using Routing;
using WebLibrary;

[assembly: OwinStartup(typeof(WebApplication.Startup))]

namespace WebApplication
{
    // http://owin.org/extensions/owin-WebSocket-Extension-v0.4.0.htm
    using WebSocketAccept = Action<IDictionary<string, object>, // options
        Func<IDictionary<string, object>, Task>>; // callback
    using WebSocketCloseAsync =
        Func<int /* closeStatus */,
            string /* closeDescription */,
            CancellationToken /* cancel */,
            Task>;
    using WebSocketReceiveAsync =
        Func<ArraySegment<byte> /* data */,
            CancellationToken /* cancel */,
            Task<Tuple<int /* messageType */,
                bool /* endOfMessage */,
                int /* count */>>>;
    using WebSocketSendAsync =
        Func<ArraySegment<byte> /* data */,
            int /* messageType */,
            bool /* endOfMessage */,
            CancellationToken /* cancel */,
            Task>;
    using WebSocketReceiveResult = Tuple<int, // type
        bool, // end of message?
        int>; // count

    public class Startup
    {
        public void Configuration(IAppBuilder app)
        {
            var codebase = Assembly.GetExecutingAssembly().CodeBase;
            var binPos = codebase.IndexOf("bin");
            var root = codebase.Substring(8, binPos-8);

            //app.Use(UpgradeToWebSockets);
            var router = MyRouter.create(root);
            // For more information on how to configure your application, visit http://go.microsoft.com/fwlink/?LinkID=316888
            
            app.Run(async context =>
            {
                await router.ExecuteRequestAsync(context);
                ;
            });

        }
        private Task UpgradeToWebSockets(IOwinContext context, Func<Task> next)
        {
            WebSocketAccept accept = context.Get<WebSocketAccept>("websocket.Accept");
            if (accept == null)
            {
                // Not a websocket request
                return next();
            }

            accept(null, WebSocketEcho);

            return Task.FromResult<object>(null);
        }

        private async Task WebSocketEcho(IDictionary<string, object> websocketContext)
        {
            var sendAsync = (WebSocketSendAsync)websocketContext["websocket.SendAsync"];
            var receiveAsync = (WebSocketReceiveAsync)websocketContext["websocket.ReceiveAsync"];
            var closeAsync = (WebSocketCloseAsync)websocketContext["websocket.CloseAsync"];
            var callCancelled = (CancellationToken)websocketContext["websocket.CallCancelled"];

            byte[] buffer = new byte[1024];
            WebSocketReceiveResult received = await receiveAsync(new ArraySegment<byte>(buffer), callCancelled);

            object status;
            while (!websocketContext.TryGetValue("websocket.ClientCloseStatus", out status) || (int)status == 0)
            {
                // Echo anything we receive
                await sendAsync(new ArraySegment<byte>(buffer, 0, received.Item3), received.Item1, received.Item2, callCancelled);

                received = await receiveAsync(new ArraySegment<byte>(buffer), callCancelled);
            }

            await closeAsync((int)websocketContext["websocket.ClientCloseStatus"], (string)websocketContext["websocket.ClientCloseDescription"], callCancelled);
        }

    }
}
