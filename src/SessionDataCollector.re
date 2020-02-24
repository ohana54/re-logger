type sessionDataProvider = unit => SessionData.t;

module SessionDataCallbacks =
  Registrar.Make({
    type t = sessionDataProvider;
  });
type addCallback = SessionDataCallbacks.listenerFn;
type t = {
  registerCallback: addCallback,
  getCallbacks: unit => SessionDataCallbacks.callbacks,
};

let get = sessionDataCollector => {
  sessionDataCollector.getCallbacks()
  |> Array.map(cb =>
       try(cb()) {
       | Js.Exn.Error(e) => SessionData.fromError(e)
       }
     )
  |> SessionData.getMergedSessionData;
};

let registerProvider = (sessionDataCollector, provider) =>
  sessionDataCollector.registerCallback(provider);

let make = () => {
  let (registerCallback, getCallbacks) = SessionDataCallbacks.create();

  {registerCallback, getCallbacks};
};
