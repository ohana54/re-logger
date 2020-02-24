[@bs.new] external makeError: string => Js.Exn.t = "Error";

let make = message => {
  let error = makeError(message);
  let stack = error->Js.Exn.stack->Belt.Option.getWithDefault("no stack");
  (error, stack, () => raise(error |> Obj.magic));
};
