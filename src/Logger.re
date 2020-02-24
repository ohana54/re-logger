module Make =
       (
         TracePayloadType: LogEvent.TracePayloadType,
         InfoPayloadType: LogEvent.InfoPayloadType,
         WarnPayloadType: LogEvent.WarnPayloadType,
       ) => {
  module LogEvent =
    LogEvent.Make(TracePayloadType, InfoPayloadType, WarnPayloadType);

  type t('a, 'b) = {
    handlers: array(Handler.t('a, 'b)),
    sessionDataCollector: SessionDataCollector.t,
  };

  let invokeHandlers = (logger, logEvent) =>
    logger.handlers |> Array.iter(handler => Handler.log(handler, logEvent));

  let trace = (logger, payload) => {
    LogEvent.(
      invokeHandlers(
        logger,
        Trace({
          payload,
          position: TracePayload.Position.None,
          sessionData: SessionDataCollector.get(logger.sessionDataCollector),
        }),
      )
    );
  };

  let calcEndPositionByResult =
      (~result, ~traceId, ~durationMs, ~startPayload) => {
    switch (result) {
    | Belt.Result.Ok(_) =>
      LogEvent.TracePayload.Position.End(
        Belt.Result.Ok({traceId, durationMs, startPayload}),
      )
    | Belt.Result.Error(error) =>
      LogEvent.TracePayload.Position.End(
        Belt.Result.Error({traceId, durationMs, startPayload, error}),
      )
    };
  };

  let traceSync = (logger, payload, fnToTrace) => {
    let ts = Js.Date.now();
    let traceId = UniqueId.make();

    LogEvent.(
      invokeHandlers(
        logger,
        Trace({
          payload,
          position: TracePayload.Position.Start(traceId),
          sessionData: SessionDataCollector.get(logger.sessionDataCollector),
        }),
      )
    );

    let result = Try.sync(fnToTrace);
    let durationMs = Js.Date.now() -. ts;
    let position =
      calcEndPositionByResult(
        ~result,
        ~traceId,
        ~durationMs,
        ~startPayload=payload,
      );

    LogEvent.(
      invokeHandlers(
        logger,
        Trace({
          payload,
          position,
          sessionData: SessionDataCollector.get(logger.sessionDataCollector),
        }),
      )
    );

    result;
  };

  let traceAsync = (logger, payload, fnToTrace) => {
    let ts = Js.Date.now();
    let traceId = UniqueId.make();

    LogEvent.(
      invokeHandlers(
        logger,
        Trace({
          payload,
          position: TracePayload.Position.Start(traceId),
          sessionData: SessionDataCollector.get(logger.sessionDataCollector),
        }),
      )
    );

    let promise = Try.async(fnToTrace);
    promise
    |> Js.Promise.then_(result => {
         let durationMs = Js.Date.now() -. ts;
         let position =
           calcEndPositionByResult(
             ~result,
             ~traceId,
             ~durationMs,
             ~startPayload=payload,
           );

         LogEvent.(
           invokeHandlers(
             logger,
             Trace({
               payload,
               position,
               sessionData:
                 SessionDataCollector.get(logger.sessionDataCollector),
             }),
           )
         );

         Js.Promise.resolve(result);
       })
    |> ignore;

    promise;
  };

  let info = (logger, payload) => {
    LogEvent.(
      invokeHandlers(
        logger,
        Info({
          payload,
          sessionData: SessionDataCollector.get(logger.sessionDataCollector),
        }),
      )
    );
  };

  let warn = (logger, payload) => {
    LogEvent.(
      invokeHandlers(
        logger,
        Warn({
          payload,
          sessionData: SessionDataCollector.get(logger.sessionDataCollector),
        }),
      )
    );
  };

  let error = (logger, error) => {
    invokeHandlers(
      logger,
      LogEvent.Error({
        LogEvent.ErrorPayload.error,
        sessionData: SessionDataCollector.get(logger.sessionDataCollector),
      }),
    );
  };

  let make = (~handlers) => {
    let sessionDataCollector = SessionDataCollector.make();

    {handlers, sessionDataCollector};
  };

  let init = (logger, initParams) => {
    logger.handlers
    |> Array.iter(handler => Handler.init(handler, initParams));
  };

  let addSessionData = (logger, cb) =>
    SessionDataCollector.registerProvider(logger.sessionDataCollector, cb);
};
