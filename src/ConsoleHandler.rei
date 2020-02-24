module Make:
  (LogEvent: LogEvent.T, InitParams: Handler.InitParams) =>
   {
    external errorAsString: Js.Exn.t => string = "%identity";
    let make:
      (
        ~shouldLog: unit => bool=?,
        ~ignoredErrorMessages: array(string)=?,
        unit
      ) =>
      Handler.t(InitParams.t, LogEvent.t);
  };