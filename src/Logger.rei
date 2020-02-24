module Make:
  (
    TracePayloadType: LogEvent.TracePayloadType,
    InfoPayloadType: LogEvent.InfoPayloadType,
    WarnPayloadType: LogEvent.WarnPayloadType,
  ) =>
   {
    module LogEvent: {
      type tracePayloadType = TracePayloadType.t;
      module TracePayload: {
        type traceId = int;
        module EndData: {
          type t =
            LogEvent.Make(TracePayloadType, InfoPayloadType, WarnPayloadType).TracePayload.EndData.t
            = {
              traceId,
              durationMs: float,
              startPayload: tracePayloadType,
            };
        };
        module EndErrorData: {
          type t =
            LogEvent.Make(TracePayloadType, InfoPayloadType, WarnPayloadType).TracePayload.EndErrorData.t
            = {
              traceId,
              durationMs: float,
              startPayload: tracePayloadType,
              error: Js.Exn.t,
            };
        };
        module Position: {
          type t =
            LogEvent.Make(TracePayloadType, InfoPayloadType, WarnPayloadType).TracePayload.Position.t
            =
              | None
              | Start(traceId)
              | End(Belt.Result.t(EndData.t, EndErrorData.t));
        };
        type t =
          LogEvent.Make(TracePayloadType, InfoPayloadType, WarnPayloadType).TracePayload.t
          = {
            payload: tracePayloadType,
            position: Position.t,
            sessionData: SessionData.t,
          };
      };
      type infoPayloadType = InfoPayloadType.t;
      module InfoPayload: {
        type t =
          LogEvent.Make(TracePayloadType, InfoPayloadType, WarnPayloadType).InfoPayload.t
          = {
            payload: infoPayloadType,
            sessionData: SessionData.t,
          };
      };
      type warnPayloadType = WarnPayloadType.t;
      module WarnPayload: {
        type t =
          LogEvent.Make(TracePayloadType, InfoPayloadType, WarnPayloadType).WarnPayload.t
          = {
            payload: warnPayloadType,
            sessionData: SessionData.t,
          };
        let toExn: WarnPayloadType.t => Js.Exn.t;
      };
      module ErrorPayload: {
        type error = Js.Exn.t;
        type t =
          LogEvent.Make(TracePayloadType, InfoPayloadType, WarnPayloadType).ErrorPayload.t
          = {
            error,
            sessionData: SessionData.t,
          };
      };
      type t =
        LogEvent.Make(TracePayloadType, InfoPayloadType, WarnPayloadType).t =
          | Trace(TracePayload.t)
          | Info(InfoPayload.t)
          | Warn(WarnPayload.t)
          | Error(ErrorPayload.t);
      let toString: t => string;
    };
    type t('a, 'b);
    let invokeHandlers: (t('a, 'b), 'b) => unit;
    let trace: (t('a, LogEvent.t), LogEvent.tracePayloadType) => unit;
    let traceSync:
      (t('a, LogEvent.t), LogEvent.tracePayloadType, unit => 'b) =>
      Belt.Result.t('b, Js.Exn.t);
    let traceAsync:
      (
        t('a, LogEvent.t),
        LogEvent.tracePayloadType,
        unit => Js.Promise.t('b)
      ) =>
      Js.Promise.t(Belt.Result.t('b, Js.Exn.t));
    let info: (t('a, LogEvent.t), LogEvent.infoPayloadType) => unit;
    let warn: (t('a, LogEvent.t), LogEvent.warnPayloadType) => unit;
    let error: (t('a, LogEvent.t), LogEvent.ErrorPayload.error) => unit;
    let make: (~handlers: array(Handler.t('a, 'b))) => t('a, 'b);
    let init: (t('a, 'b), 'a) => unit;
    let addSessionData:
      (t('a, 'b), SessionDataCollector.sessionDataProvider) =>
      SessionDataCollector.SessionDataCallbacks.unregisterFn;
  };