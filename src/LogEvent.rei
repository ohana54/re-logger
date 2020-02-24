module type InfoPayloadType = {
  type t;
  let toString: t => string;
};
module type WarnPayloadType = {
  type t;
  let toString: t => string;
  let toExn: t => Js.Exn.t;
};
module type TracePayloadType = {
  type t;
  let toString: t => string;
};
module type T = {
  type tracePayloadType;
  module TracePayload: {
    type traceId = int;
    module EndData: {
      type t = {
        traceId,
        durationMs: float,
        startPayload: tracePayloadType,
      };
    };
    module EndErrorData: {
      type t = {
        traceId,
        durationMs: float,
        startPayload: tracePayloadType,
        error: Js.Exn.t,
      };
    };
    module Position: {
      type t =
        | None
        | Start(traceId)
        | End(Belt.Result.t(EndData.t, EndErrorData.t));
    };
    type t = {
      payload: tracePayloadType,
      position: Position.t,
      sessionData: SessionData.t,
    };
  };
  type infoPayloadType;
  module InfoPayload: {
    type t = {
      payload: infoPayloadType,
      sessionData: SessionData.t,
    };
  };
  type warnPayloadType;
  module WarnPayload: {
    type t = {
      payload: warnPayloadType,
      sessionData: SessionData.t,
    };
    let toExn: warnPayloadType => Js.Exn.t;
  };
  module ErrorPayload: {
    type error = Js.Exn.t;
    type t = {
      error,
      sessionData: SessionData.t,
    };
  };
  type t =
    | Trace(TracePayload.t)
    | Info(InfoPayload.t)
    | Warn(WarnPayload.t)
    | Error(ErrorPayload.t);
  let toString: t => string;
};
module Make:
  (
    TracePayloadType: TracePayloadType,
    InfoPayloadType: InfoPayloadType,
    WarnPayloadType: WarnPayloadType,
  ) =>
   {
    type tracePayloadType = TracePayloadType.t;
    module TracePayload: {
      type traceId = int;
      module EndData: {
        type t = {
          traceId,
          durationMs: float,
          startPayload: tracePayloadType,
        };
      };
      module EndErrorData: {
        type t = {
          traceId,
          durationMs: float,
          startPayload: tracePayloadType,
          error: Js.Exn.t,
        };
      };
      module Position: {
        type t =
          | None
          | Start(traceId)
          | End(Belt.Result.t(EndData.t, EndErrorData.t));
      };
      type t = {
        payload: tracePayloadType,
        position: Position.t,
        sessionData: SessionData.t,
      };
    };
    type infoPayloadType = InfoPayloadType.t;
    module InfoPayload: {
      type t = {
        payload: infoPayloadType,
        sessionData: SessionData.t,
      };
    };
    type warnPayloadType = WarnPayloadType.t;
    module WarnPayload: {
      type t = {
        payload: warnPayloadType,
        sessionData: SessionData.t,
      };
      let toExn: warnPayloadType => Js.Exn.t;
    };
    module ErrorPayload: {
      type error = Js.Exn.t;
      type t = {
        error,
        sessionData: SessionData.t,
      };
    };
    type t =
      | Trace(TracePayload.t)
      | Info(InfoPayload.t)
      | Warn(WarnPayload.t)
      | Error(ErrorPayload.t);
    let toString: t => string;
  };