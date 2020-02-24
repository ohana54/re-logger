type t = Js.Json.t;

[@bs.module "deepmerge"]
external mergeAll: array(Js.Json.t) => Js.Json.t = "all";

let make = sessionData => sessionData;

let getMergedSessionData = sessionDataArray => sessionDataArray |> mergeAll;

let fromError = e => {
  let errorStack =
    Belt.Option.getWithDefault(Js.Exn.stack(e), "no error stack")
    |> Js.Json.string;
  Js.Dict.fromArray([|("sessionDataError", errorStack)|]) |> Js.Json.object_;
};
