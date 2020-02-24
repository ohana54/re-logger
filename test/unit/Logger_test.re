open Jest;
open ExtraExpect;

module TracePayload = {
  type t = string;
  let toString = t => t;
};
module InfoPayload = {
  type t = string;
  let toString = t => t;
};
module WarnPayload = {
  type t = string;
  let toString = t => t;
  let toExn = payload => {
    let (e, _, _) = JsError.make(payload);
    e;
  };
};
module AppLogger = Logger.Make(TracePayload, InfoPayload, WarnPayload);

type handlerStrategy('initSpy, 'logSpy, 'initParams, 'logEvent) = {
  handlers: array(Handler.t('initParams, 'logEvent)),
  initSpies: array('initSpy),
  /*
   MockJs.fn(
     option(Logger.initParams) => unit,
     option(Logger.initParams),
     unit,
   ),
   */
  logSpies: array('logSpy) //MockJs.fn(LogEvent.t => unit, LogEvent.t, unit)
};

let raiseUnexpectedVariant = v => {
  Js.Exn.raiseError(
    "Expected a specific variant, but got: " ++ AppLogger.LogEvent.toString(v),
  );
};

let stringToSessionData = s => s |> Js.Json.parseExn |> SessionData.make;

let getLastSpiesCallArgs = spies => {
  spies
  |> Array.map(spy => {
       let calls = spy |> MockJs.calls;
       calls[(calls |> Array.length) - 1];
     });
};

let getSpiesCallArgs = spies => {
  spies |> Array.map(spy => spy |> MockJs.calls);
};

let mapLogSpiesToActualAndExpectedData =
    (~logSpies, ~mapActual: AppLogger.LogEvent.t => 'a, ~expectedData: 'a) => {
  let lastCallArgs = getLastSpiesCallArgs(logSpies);
  let actualData = lastCallArgs |> Array.map(mapActual);
  let expectedDataArray = Array.make(logSpies |> Array.length, expectedData);
  (actualData, expectedDataArray);
};

let mapAllLogSpiesToActualAndExpectedData =
    (
      ~logSpies,
      ~mapActual: AppLogger.LogEvent.t => 'a,
      ~expectedData: array('a),
    ) => {
  let allCallArgs = getSpiesCallArgs(logSpies);
  let actualData = allCallArgs |> Array.map(Array.map(mapActual));
  let expectedDataArray = Array.make(logSpies |> Array.length, expectedData);
  (actualData, expectedDataArray);
};

describe("logger", () => {
  let createHandlerStrategy = () => {
    let initSpy = JestJs.fn(_ => ());
    let initFn = MockJs.fn(initSpy);

    let logSpy = JestJs.fn(_ => ());
    let logFn = MockJs.fn(logSpy);

    let handler = Handler.make(~handleInit=initFn, ~handleLog=logFn, ());
    {handlers: [|handler|], initSpies: [|initSpy|], logSpies: [|logSpy|]};
  };

  let handlersStrategy =
    Belt.Map.String.fromArray([|
      ("single handler", createHandlerStrategy),
      (
        "multiple handlers",
        () => {
          let strategy1 = createHandlerStrategy();
          let strategy2 = createHandlerStrategy();
          {
            handlers: Array.concat([strategy1.handlers, strategy2.handlers]),
            initSpies:
              Array.concat([strategy1.initSpies, strategy2.initSpies]),
            logSpies: Array.concat([strategy1.logSpies, strategy2.logSpies]),
          };
        },
      ),
    |]);

  handlersStrategy->Belt.Map.String.forEach((strategyName, createStrategy) => {
    describe({j|init - $strategyName|j}, () => {
      test("should be called", () => {
        let {handlers, initSpies} = createStrategy();
        AppLogger.make(~handlers)->AppLogger.init(None);
        expectSpiesToHaveBeenCalled(initSpies);
      });

      test("should be called with the init params", () => {
        let {handlers, initSpies} = createStrategy();
        let initParams = Some("init parameter");
        AppLogger.make(~handlers)->AppLogger.init(initParams);
        expectSpiesToHaveBeenCalledWith(initSpies, initParams);
      });
    });

    describe({j|logging functions - $strategyName|j}, () => {
      describe("session data", () => {
        test("should merge multiple providers", () => {
          let message = "info message";
          let sessionData1 =
            {|
                {
                  "some": {
                    "nested": { "data": "session data" }
                  }
                }
              |}
            |> stringToSessionData;
          let sessionData2 =
            {|
                {
                  "some": {
                    "nested2": "data"
                  }
                }
               |}
            |> stringToSessionData;
          let sessionData3 =
            {|
                {
                  "some": {
                    "nested": { "data": "override" }
                  }
                }
               |}
            |> stringToSessionData;
          let expectedSessionData =
            {|
                {
                  "some": {
                    "nested": { "data": "override" },
                    "nested2": "data"
                  }
                }
               |}
            |> stringToSessionData;

          let {handlers, logSpies} = createStrategy();

          let logger = AppLogger.make(~handlers);
          AppLogger.addSessionData(logger, () => sessionData1) |> ignore;
          AppLogger.addSessionData(logger, () => sessionData2) |> ignore;
          AppLogger.addSessionData(logger, () => sessionData3) |> ignore;
          AppLogger.info(logger, message);

          let (actualSessionDatas, expectedSessionDatas) =
            mapLogSpiesToActualAndExpectedData(
              ~logSpies,
              ~mapActual=
                logEvent =>
                  switch (logEvent) {
                  | AppLogger.LogEvent.Info({sessionData}) => sessionData
                  | v => raiseUnexpectedVariant(v)
                  },
              ~expectedData=expectedSessionData,
            );

          Expect.(
            expect(actualSessionDatas) |> toEqual(expectedSessionDatas)
          );
        });

        test("should allow to unregister a provider", () => {
          let message = "info message";
          let sessionData1 =
            {|
                {
                  "some": {
                    "nested": { "data": "session data" }
                  }
                }
               |}
            |> stringToSessionData;
          let sessionData2 =
            {|
                {
                  "some": {
                    "nested2": "data"
                  }
                }
               |}
            |> stringToSessionData;
          let sessionData3 =
            {|
                {
                  "some": {
                    "nested": { "data": "override" }
                  }
                }
               |}
            |> stringToSessionData;
          let expectedSessionData =
            {|
                {
                  "some": {
                    "nested": { "data": "session data" },
                    "nested2": "data"
                  }
                }
               |}
            |> stringToSessionData;

          let {handlers, logSpies} = createStrategy();

          let logger = AppLogger.make(~handlers);
          AppLogger.addSessionData(logger, () => sessionData1) |> ignore;
          AppLogger.addSessionData(logger, () => sessionData2) |> ignore;
          let unregister =
            AppLogger.addSessionData(logger, () => sessionData3);
          unregister();
          AppLogger.info(logger, message);

          let (actualSessionDatas, expectedSessionDatas) =
            mapLogSpiesToActualAndExpectedData(
              ~logSpies,
              ~mapActual=
                logEvent =>
                  switch (logEvent) {
                  | AppLogger.LogEvent.Info({sessionData}) => sessionData
                  | v => raiseUnexpectedVariant(v)
                  },
              ~expectedData=expectedSessionData,
            );

          Expect.(
            expect(actualSessionDatas) |> toEqual(expectedSessionDatas)
          );
        });

        test("should not fail on throwing provider", () => {
          let message = "info message";
          let (_, sessionDataErrorStack, throwSessionDataError) =
            JsError.make("session data error");

          let stack =
            sessionDataErrorStack
            |> Js.Json.stringifyAny
            |> Belt.Option.getWithDefault(_, "no stack trace");

          let sessionData =
            {|
                {
                  "data": "session data"
                }
               |}
            |> stringToSessionData;

          let expectedSessionData =
            {j|
                {
                  "data": "session data",
                  "sessionDataError": $stack
                }
               |j}
            |> stringToSessionData;

          let {handlers, logSpies} = createStrategy();
          let logger = AppLogger.make(~handlers);
          AppLogger.addSessionData(logger, () => sessionData) |> ignore;
          AppLogger.addSessionData(logger, throwSessionDataError) |> ignore;
          AppLogger.info(logger, message);

          let (actualSessionDatas, expectedSessionDatas) =
            mapLogSpiesToActualAndExpectedData(
              ~logSpies,
              ~mapActual=
                logEvent =>
                  switch (logEvent) {
                  | AppLogger.LogEvent.Info({sessionData}) => sessionData
                  | v => raiseUnexpectedVariant(v)
                  },
              ~expectedData=expectedSessionData,
            );

          Expect.(
            expect(actualSessionDatas) |> toEqual(expectedSessionDatas)
          );
        });
      });

      describe("info", () => {
        test("should pass an info payload", () => {
          let {handlers, logSpies} = createStrategy();
          let logger = AppLogger.make(~handlers);
          let message = "some info message";

          AppLogger.info(logger, message);

          let (actualInfoEvents, expectedInfoEvents) =
            mapLogSpiesToActualAndExpectedData(
              ~logSpies,
              ~mapActual=
                logEvent =>
                  switch (logEvent) {
                  | AppLogger.LogEvent.Info({payload}) => payload
                  | v => raiseUnexpectedVariant(v)
                  },
              ~expectedData=message,
            );

          Expect.(expect(actualInfoEvents) |> toEqual(expectedInfoEvents));
        });

        test("should pass session data", () => {
          let {handlers, logSpies} = createStrategy();
          let logger = AppLogger.make(~handlers);
          let sessionData =
            {|
                {
                  "data": "session data"
                }
               |}
            |> stringToSessionData;

          AppLogger.addSessionData(logger, () => sessionData) |> ignore;

          AppLogger.info(logger, "some info message");

          let (actualSessionData, expectedSessionData) =
            mapLogSpiesToActualAndExpectedData(
              ~logSpies,
              ~mapActual=
                logEvent =>
                  switch (logEvent) {
                  | AppLogger.LogEvent.Info({sessionData}) => sessionData
                  | v => raiseUnexpectedVariant(v)
                  },
              ~expectedData=sessionData,
            );

          Expect.(expect(actualSessionData) |> toEqual(expectedSessionData));
        });
      });

      describe("warn", () => {
        test("should pass a warn payload", () => {
          let {handlers, logSpies} = createStrategy();
          let logger = AppLogger.make(~handlers);
          let message = "some warning message";

          AppLogger.warn(logger, message);

          let (actualWarnEvents, expectedWarnEvents) =
            mapLogSpiesToActualAndExpectedData(
              ~logSpies,
              ~mapActual=
                logEvent =>
                  switch (logEvent) {
                  | AppLogger.LogEvent.Warn({payload}) => payload
                  | v => raiseUnexpectedVariant(v)
                  },
              ~expectedData=message,
            );

          Expect.(expect(actualWarnEvents) |> toEqual(expectedWarnEvents));
        });

        test("should pass session data", () => {
          let {handlers, logSpies} = createStrategy();
          let logger = AppLogger.make(~handlers);
          let sessionData =
            {|
                {
                  "data": "session data"
                }
               |}
            |> stringToSessionData;

          AppLogger.addSessionData(logger, () => sessionData) |> ignore;

          AppLogger.warn(logger, "some warning message");

          let (actualSessionData, expectedSessionData) =
            mapLogSpiesToActualAndExpectedData(
              ~logSpies,
              ~mapActual=
                logEvent =>
                  switch (logEvent) {
                  | AppLogger.LogEvent.Warn({sessionData}) => sessionData
                  | v => raiseUnexpectedVariant(v)
                  },
              ~expectedData=sessionData,
            );

          Expect.(expect(actualSessionData) |> toEqual(expectedSessionData));
        });
      });

      describe("error", () => {
        test("should pass an error", () => {
          let {handlers, logSpies} = createStrategy();
          let logger = AppLogger.make(~handlers);
          let (error, _, _) = JsError.make("some error");

          AppLogger.error(logger, error);

          let (actualErrorEvents, expectedErrorEvents) =
            mapLogSpiesToActualAndExpectedData(
              ~logSpies,
              ~mapActual=
                logEvent =>
                  switch (logEvent) {
                  | AppLogger.LogEvent.Error({error}) => error
                  | v => raiseUnexpectedVariant(v)
                  },
              ~expectedData=error,
            );

          Expect.(expect(actualErrorEvents) |> toEqual(expectedErrorEvents));
        });

        test("should pass session data", () => {
          let {handlers, logSpies} = createStrategy();
          let logger = AppLogger.make(~handlers);
          let sessionData =
            {|
                {
                  "data": "session data"
                }
               |}
            |> stringToSessionData;

          AppLogger.addSessionData(logger, () => sessionData) |> ignore;

          AppLogger.warn(logger, "some warning message");

          let (actualSessionData, expectedSessionData) =
            mapLogSpiesToActualAndExpectedData(
              ~logSpies,
              ~mapActual=
                logEvent =>
                  switch (logEvent) {
                  | AppLogger.LogEvent.Warn({sessionData}) => sessionData
                  | v => raiseUnexpectedVariant(v)
                  },
              ~expectedData=sessionData,
            );

          Expect.(expect(actualSessionData) |> toEqual(expectedSessionData));
        });
      });

      describe("trace", () => {
        test("should log a trace event", () => {
          let {handlers, logSpies} = createStrategy();
          let logger = AppLogger.make(~handlers);
          let message = "some trace message";
          AppLogger.trace(logger, message);
          let (actualTraceEvents, expectedTraceEvents) =
            mapLogSpiesToActualAndExpectedData(
              ~logSpies,
              ~mapActual=
                logEvent =>
                  switch (logEvent) {
                  | AppLogger.LogEvent.Trace({payload}) => payload
                  | v => raiseUnexpectedVariant(v)
                  },
              ~expectedData=message,
            );
          Expect.(expect(actualTraceEvents) |> toEqual(expectedTraceEvents));
        });

        test("should pass session data", () => {
          let {handlers, logSpies} = createStrategy();
          let logger = AppLogger.make(~handlers);
          let sessionData =
            {|
                {
                  "data": "session data"
                }
               |}
            |> stringToSessionData;

          AppLogger.addSessionData(logger, () => sessionData) |> ignore;

          AppLogger.trace(logger, "some trace message");

          let (actualSessionData, expectedSessionData) =
            mapLogSpiesToActualAndExpectedData(
              ~logSpies,
              ~mapActual=
                logEvent =>
                  switch (logEvent) {
                  | AppLogger.LogEvent.Trace({sessionData}) => sessionData
                  | v => raiseUnexpectedVariant(v)
                  },
              ~expectedData=sessionData,
            );

          Expect.(expect(actualSessionData) |> toEqual(expectedSessionData));
        });

        test("should have a None position", () => {
          let {handlers, logSpies} = createStrategy();
          let logger = AppLogger.make(~handlers);
          AppLogger.trace(logger, "some trace message");
          let (actualTraceEvents, expectedTraceEvents) =
            mapLogSpiesToActualAndExpectedData(
              ~logSpies,
              ~mapActual=
                logEvent =>
                  switch (logEvent) {
                  | AppLogger.LogEvent.Trace({position}) => position
                  | v => raiseUnexpectedVariant(v)
                  },
              ~expectedData=AppLogger.LogEvent.TracePayload.Position.None,
            );
          Expect.(expect(actualTraceEvents) |> toEqual(expectedTraceEvents));
        });
      });

      describe("traceSync", () => {
        test("should log trace start and end events", () => {
          let {handlers, logSpies} = createStrategy();
          let logger = AppLogger.make(~handlers);
          let message = "some trace message";
          AppLogger.traceSync(logger, message, () => ()) |> ignore;
          let (actualTraceEvents, expectedTraceEvents) =
            mapAllLogSpiesToActualAndExpectedData(
              ~logSpies,
              ~mapActual=
                logEvent =>
                  switch (logEvent) {
                  | AppLogger.LogEvent.Trace({position: Start(_)}) =>
                    AppLogger.LogEvent.TracePayload.Position.Start(0)
                  | AppLogger.LogEvent.Trace({
                      position: End(Belt.Result.Ok({startPayload})),
                    }) =>
                    AppLogger.LogEvent.TracePayload.Position.End(
                      Belt.Result.Ok({
                        traceId: 0,
                        durationMs: 0.0,
                        startPayload,
                      }),
                    )
                  | v => raiseUnexpectedVariant(v)
                  },
              ~expectedData=
                AppLogger.LogEvent.TracePayload.Position.(
                  [|
                    Start(0),
                    End(
                      Belt.Result.Ok({
                        traceId: 0,
                        durationMs: 0.0,
                        startPayload: message,
                      }),
                    ),
                  |]
                ),
            );

          Expect.(expect(actualTraceEvents) |> toEqual(expectedTraceEvents));
        });

        test("should return the wrapped function return value", () => {
          let {handlers} = createStrategy();
          let logger = AppLogger.make(~handlers);
          let message = "some trace message";
          let fnToWrap = () => "return value";
          let returnValue = AppLogger.traceSync(logger, message, fnToWrap);

          Expect.(
            expect(returnValue) |> toEqual(Belt.Result.Ok("return value"))
          );
        });

        describe("on error", () => {
          test("should send an end event with the error", () => {
            let {handlers, logSpies} = createStrategy();
            let logger = AppLogger.make(~handlers);
            let message = "some trace message";
            let (error, _, raiseError) = JsError.make("traceSync error");
            let fnToWrap = () => {
              raiseError();
            };
            AppLogger.traceSync(logger, message, fnToWrap) |> ignore;
            let (actualTraceEvents, expectedTraceEvents) =
              mapAllLogSpiesToActualAndExpectedData(
                ~logSpies,
                ~mapActual=
                  logEvent =>
                    switch (logEvent) {
                    | AppLogger.LogEvent.Trace({position: Start(_)}) =>
                      AppLogger.LogEvent.TracePayload.Position.Start(0)
                    | AppLogger.LogEvent.Trace({
                        position:
                          End(Belt.Result.Error({startPayload, error})),
                      }) =>
                      AppLogger.LogEvent.TracePayload.Position.End(
                        Belt.Result.Error({
                          traceId: 0,
                          durationMs: 0.0,
                          startPayload,
                          error,
                        }),
                      )
                    | v => raiseUnexpectedVariant(v)
                    },
                ~expectedData=
                  AppLogger.LogEvent.TracePayload.Position.(
                    [|
                      Start(0),
                      End(
                        Belt.Result.Error({
                          traceId: 0,
                          durationMs: 0.0,
                          startPayload: message,
                          error,
                        }),
                      ),
                    |]
                  ),
              );

            Expect.(
              expect(actualTraceEvents) |> toEqual(expectedTraceEvents)
            );
          });

          test("should return the error", () => {
            let {handlers} = createStrategy();
            let logger = AppLogger.make(~handlers);
            let message = "some trace message";
            let (error, _, raiseError) = JsError.make("traceSync error");
            let fnToWrap = () => {
              raiseError();
            };
            let returnValue = AppLogger.traceSync(logger, message, fnToWrap);
            Expect.(
              expect(returnValue) |> toEqual(Belt.Result.Error(error))
            );
          });
        });
      });

      describe("traceAsync", () => {
        testPromise("should log trace start and end events", () => {
          let {handlers, logSpies} = createStrategy();
          let logger = AppLogger.make(~handlers);
          let message = "some trace message";
          AppLogger.traceAsync(logger, message, () => Js.Promise.resolve())
          |> Js.Promise.then_(_ => {
               let (actualTraceEvents, expectedTraceEvents) =
                 mapAllLogSpiesToActualAndExpectedData(
                   ~logSpies,
                   ~mapActual=
                     logEvent =>
                       switch (logEvent) {
                       | AppLogger.LogEvent.Trace({position: Start(_)}) =>
                         AppLogger.LogEvent.TracePayload.Position.Start(0)
                       | AppLogger.LogEvent.Trace({
                           position: End(Belt.Result.Ok({startPayload})),
                         }) =>
                         AppLogger.LogEvent.TracePayload.Position.End(
                           Belt.Result.Ok({
                             traceId: 0,
                             durationMs: 0.0,
                             startPayload,
                           }),
                         )
                       | v => raiseUnexpectedVariant(v)
                       },
                   ~expectedData=
                     AppLogger.LogEvent.TracePayload.Position.(
                       [|
                         Start(0),
                         End(
                           Belt.Result.Ok({
                             traceId: 0,
                             durationMs: 0.0,
                             startPayload: message,
                           }),
                         ),
                       |]
                     ),
                 );

               Js.Promise.resolve(
                 Expect.(
                   expect(actualTraceEvents) |> toEqual(expectedTraceEvents)
                 ),
               );
             });
        });

        testPromise("should return the wrapped function return value", () => {
          let {handlers} = createStrategy();
          let logger = AppLogger.make(~handlers);
          let message = "some trace message";
          let fnToWrap = () => Js.Promise.resolve("return value");
          AppLogger.traceAsync(logger, message, fnToWrap)
          |> Js.Promise.then_(result =>
               Js.Promise.resolve(
                 Expect.(
                   expect(result) |> toEqual(Belt.Result.Ok("return value"))
                 ),
               )
             );
        });

        describe("on error", () => {
          testPromise("should send an end event with the error", () => {
            let {handlers, logSpies} = createStrategy();
            let logger = AppLogger.make(~handlers);
            let message = "some trace message";
            let (error, _, _) = JsError.make("traceAsync error");
            let fnToWrap = () => Js.Promise.reject(error |> Obj.magic);

            AppLogger.traceAsync(logger, message, fnToWrap)
            |> Js.Promise.then_(_ => {
                 let (actualTraceEvents, expectedTraceEvents) =
                   mapAllLogSpiesToActualAndExpectedData(
                     ~logSpies,
                     ~mapActual=
                       logEvent =>
                         switch (logEvent) {
                         | AppLogger.LogEvent.Trace({position: Start(_)}) =>
                           AppLogger.LogEvent.TracePayload.Position.Start(0)
                         | AppLogger.LogEvent.Trace({
                             position:
                               End(Belt.Result.Error({startPayload, error})),
                           }) =>
                           AppLogger.LogEvent.TracePayload.Position.End(
                             Belt.Result.Error({
                               traceId: 0,
                               durationMs: 0.0,
                               startPayload,
                               error,
                             }),
                           )
                         | v => raiseUnexpectedVariant(v)
                         },
                     ~expectedData=
                       AppLogger.LogEvent.TracePayload.Position.(
                         [|
                           Start(0),
                           End(
                             Belt.Result.Error({
                               traceId: 0,
                               durationMs: 0.0,
                               startPayload: message,
                               error,
                             }),
                           ),
                         |]
                       ),
                   );

                 Js.Promise.resolve(
                   Expect.(
                     expect(actualTraceEvents)
                     |> toEqual(expectedTraceEvents)
                   ),
                 );
               });
          });

          testPromise("should return the error", () => {
            let {handlers} = createStrategy();
            let logger = AppLogger.make(~handlers);
            let message = "some trace message";
            let (error, _, _) = JsError.make("traceAsync error");
            let fnToWrap = () => Js.Promise.reject(error |> Obj.magic);
            AppLogger.traceAsync(logger, message, fnToWrap)
            |> Js.Promise.then_(result =>
                 Js.Promise.resolve(
                   Expect.(
                     expect(result) |> toEqual(Belt.Result.Error(error))
                   ),
                 )
               );
          });
        });
      });
    });
  });
});
