type t;

let make: Js.Json.t => t

let getMergedSessionData: array(t) => t

let fromError: Js.Exn.t => t