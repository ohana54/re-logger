type sessionDataProvider = unit => SessionData.t;
module SessionDataCallbacks: {
  type unregisterFn = unit => unit;
};
type t;
let get: t => SessionData.t;
let registerProvider:
  (t, sessionDataProvider) => SessionDataCallbacks.unregisterFn;
let make: unit => t;