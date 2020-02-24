external promiseRejectionToExn: Js.Promise.error => Js.Exn.t = "%identity";

let sync = fn =>
  try(Belt.Result.Ok(fn())) {
  | Js.Exn.Error(e) => Belt.Result.Error(e)
  };

let async = fn => {
  fn()
  |> Js.Promise.then_(v => Js.Promise.resolve(Belt.Result.Ok(v)))
  |> Js.Promise.catch(e =>
       Js.Promise.resolve(Belt.Result.Error(e |> promiseRejectionToExn))
     );
};
