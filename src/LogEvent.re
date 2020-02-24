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

module Make =
       (
         TracePayloadType: TracePayloadType,
         InfoPayloadType: InfoPayloadType,
         WarnPayloadType: WarnPayloadType,
       )

         : (
           T with
             type tracePayloadType = TracePayloadType.t and
             type infoPayloadType = InfoPayloadType.t and
             type warnPayloadType = WarnPayloadType.t
       ) => {
  type tracePayloadType = TracePayloadType.t;
  module TracePayload = {
    type traceId = int;
    module EndData = {
      type t = {
        traceId,
        durationMs: float,
        startPayload: tracePayloadType,
      };

      let toString = ({traceId, durationMs, startPayload}) => {
        let startPayloadString = TracePayloadType.toString(startPayload);
        {j|{
          traceId: $traceId,
          durationMs: $durationMs,
          startPayload: $startPayloadString
        }|j};
      };
    };
    module EndErrorData = {
      type t = {
        traceId,
        durationMs: float,
        startPayload: tracePayloadType,
        error: Js.Exn.t,
      };

      let toString = ({traceId, durationMs, startPayload, error}) => {
        let startPayloadString = TracePayloadType.toString(startPayload);
        {j|{
          traceId: $traceId,
          durationMs: $durationMs,
          startPayload: $startPayloadString,
          error: $error
        }|j};
      };
    };
    module Position = {
      type t =
        | None
        | Start(traceId)
        | End(Belt.Result.t(EndData.t, EndErrorData.t));

      let toString = position =>
        switch (position) {
        | None => "None"
        | Start(traceId) => {j|Start($traceId)|j}
        | End(result) =>
          let result =
            switch (result) {
            | Belt.Result.Ok(endData) =>
              let endDataString = EndData.toString(endData);
              {j|Ok($endDataString)|j};
            | Belt.Result.Error(endErrorData) =>
              let endErrorDataString = EndErrorData.toString(endErrorData);
              {j|Error($endErrorDataString)|j};
            };
          {j|End($result)|j};
        };
    };
    type t = {
      payload: tracePayloadType,
      position: Position.t,
      sessionData: SessionData.t,
    };

    let toString = tracePayload => {
      let payloadTypeString = TracePayloadType.toString(tracePayload.payload);
      let position = Position.toString(tracePayload.position);
      {j|{
        payload: $payloadTypeString,
        position: $position
      }|j};
    };
  };

  type infoPayloadType = InfoPayloadType.t;
  module InfoPayload = {
    type t = {
      payload: infoPayloadType,
      sessionData: SessionData.t,
    };
  };

  type warnPayloadType = WarnPayloadType.t;
  module WarnPayload = {
    type t = {
      payload: warnPayloadType,
      sessionData: SessionData.t,
    };
    let toExn = WarnPayloadType.toExn;
  };

  module ErrorPayload = {
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

  let toString = t =>
    switch (t) {
    | Trace(payload) =>
      let payloadString = TracePayload.toString(payload);
      {j|Trace($payloadString)|j};
    | Info({payload}) =>
      let payloadString = InfoPayloadType.toString(payload);
      {j|Info($payloadString)|j};
    | Warn({payload}) =>
      let payloadString = WarnPayloadType.toString(payload);
      {j|Warn($payloadString)|j};
    | Error({error}) => {j|Error($error)|j}
    };
};
