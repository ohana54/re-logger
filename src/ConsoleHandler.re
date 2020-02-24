module Make = (LogEvent: LogEvent.T, InitParams: Handler.InitParams) => {
  external errorAsString: Js.Exn.t => string = "%identity";

  let make = (~shouldLog=() => false, ~ignoredErrorMessages=[||], ()) => {
    let isErrorMessageIgnored = errorMessage =>
      ignoredErrorMessages->Belt.Array.some(ignoredErrorMessage =>
        errorMessage === ignoredErrorMessage
      );

    let toErrorMessageAndStack = error => {
      let errorMessage =
        Belt.Option.getWithDefault(
          Js.Exn.message(error),
          errorAsString(error),
        );
      let errorStack =
        Belt.Option.getWithDefault(
          Js.Exn.stack(error),
          errorAsString(error),
        );
      (errorMessage, errorStack);
    };

    let logError = ((errorMessage, errorStack)) =>
      if (!isErrorMessageIgnored(errorMessage)) {
        Js.Console.error(errorStack);
      } else {
        ();
      };

    Handler.make(
      ~handleLog=
        logEvent =>
          if (shouldLog()) {
            switch (logEvent) {
            | LogEvent.Error({error}) =>
              error |> toErrorMessageAndStack |> logError
            | LogEvent.Warn({payload}) =>
              payload
              |> LogEvent.WarnPayload.toExn
              |> toErrorMessageAndStack
              |> logError
            | _ => ()
            };
          },
      (),
    );
  };
};
