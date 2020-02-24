module type RegistrarListener = {type t;};
module Make:
  (RegistrarListener: RegistrarListener) =>
   {
    type unregisterFn = unit => unit;
    type listenerFn = RegistrarListener.t => unregisterFn;
    type callbacks = array(RegistrarListener.t);
    module ListenersMap = Belt.Map.Int;
    let create: unit => (('a, unit) => unit, unit => array('a));
  };