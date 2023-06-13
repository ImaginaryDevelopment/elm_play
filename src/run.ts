import { Server, Request, Response, Event } from "web-dev-server";

// Create web server instance.
Server.CreateNew()
   // Required.
   .SetDocumentRoot(__dirname)
   // Optional, 8000 by default.
   .SetPort(8000)
   // Optional, '127.0.0.1' by default.
   //.SetHostname('127.0.0.1')
   // Optional, `true` by default to display Errors and directories
   //.SetDevelopment(false)
   // Optional, `null` by default, useful for apache proxy modes.
   //.SetBasePath('/node')
   // Optional, custom place to log any unhandled errors.
   /*.SetErrorHandler(async (
      err: Error,
      code: number,
      req: Request,
      res: Response
   ) => { })*/
   // Optional, to prepend any execution before `web-dev-server` module execution.
   .AddPreHandler(async (
      req: Request,
      res: Response,
      event?: Event
   ) => {
      if (req.GetPath() == '/health') {
         res.SetCode(200).SetBody('1').Send();
        // Do not anything else in `web-dev-server` module for this request:
         event?.PreventDefault();
      }
      /*setTimeout(function () {
         throw new Error("Test error:-)");
      }, 1000);*/
   })
   // Callback param is optional. called after server has been started or after error ocured.
   .Start((success?: boolean, err?: Error) => {
      if (!success) return console.error(err);
      console.log("Server is running.");
   });