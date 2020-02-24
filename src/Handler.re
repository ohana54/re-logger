module type InitParams = {type t;};

type initFn('a) = 'a => unit;
type logFn('a) = 'a => unit;
type t('a, 'b) = {
  init: option(initFn('a)),
  log: logFn('b),
};

let make = (~handleInit=?, ~handleLog, ()) => {
  init: handleInit,
  log: handleLog,
};

let init = (handler, initParams) =>
  switch (handler.init) {
  | Some(init) => init(initParams)
  | None => ()
  };

let log = (handler, logEvent) => handler.log(logEvent);
