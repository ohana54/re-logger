module type InitParams = { type t; };
type initFn('a) = 'a => unit;
type logFn('a) = 'a => unit;
type t('a, 'b) = { init: option(initFn('a)), log: logFn('b), };
let make:
  (~handleInit: initFn('a)=?, ~handleLog: logFn('b), unit) => t('a, 'b);
let init: (t('a, 'b), 'a) => unit;
let log: (t('a, 'b), 'b) => unit;