let sync: (unit => 'a) => Belt.Result.t('a, Js.Exn.t);
let async:
  (unit => Js.Promise.t('a)) => Js.Promise.t(Belt.Result.t('a, Js.Exn.t));