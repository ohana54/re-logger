open Jest;
open ExtraExpect;

module TracePayload = {
  type t = unit;
  let toString = _ => "";
};
module InfoPayload = {
  type t = unit;
  let toString = _ => "";
};
module WarnPayload = {
  type t = string;
  let toString = t => t;
  let toExn = payload => {
    let (e, _, _) = JsError.make(payload);
    e;
  };
};
module InitParams = {
  type t = unit;
};
module TestLogger = Logger.Make(TracePayload, InfoPayload, WarnPayload);
module TestConsoleHandler =
  ConsoleHandler.Make(TestLogger.LogEvent, InitParams);

let createConsoleSpy = () => {
  let consoleSpy: MockJs.fn(string => unit, string, unit) = [%raw
    "jest.spyOn(global.console, 'error')"
  ];
  MockJs.mockImplementation(_ => (), consoleSpy) |> ignore;
  consoleSpy;
};

let setup = (~shouldLog=() => true, ~ignoredErrorMessages=[||], ()) => {
  let consoleSpy = createConsoleSpy();
  let consoleHandler =
    TestConsoleHandler.make(~shouldLog, ~ignoredErrorMessages, ());
  let logger = TestLogger.make(~handlers=[|consoleHandler|]);
  (logger, consoleSpy);
};

describe("console handler", () => {
  afterEach(() => JestJs.resetAllMocks());

  describe("on error", () =>
    test("should log the error stack", () => {
      let (logger, consoleSpy) = setup();
      let (e, stack, _) = JsError.make("My error");
      logger->TestLogger.error(e);
      expectSpyToHaveBeenCalledWith(consoleSpy, stack);
    })
  );

  describe("on warn", () =>
    test("should log the error stack", () => {
      let (logger, consoleSpy) = setup();
      logger->TestLogger.warn("Some error");
      expectSpyToHaveBeenCalledWithStringContaining(consoleSpy, "Some error");
    })
  );

  describe("shouldLog", () => {
    test("should log when set to true", () => {
      let (logger, consoleSpy) = setup(~shouldLog=() => true, ());
      let (e, stack, _) = JsError.make("My error");
      logger->TestLogger.error(e);
      expectSpyToHaveBeenCalledWith(consoleSpy, stack);
    });

    test("should not log when set to false", () => {
      let (logger, consoleSpy) = setup(~shouldLog=() => false, ());
      let (e, _, _) = JsError.make("My error");
      logger->TestLogger.error(e);
      expectSpyNotToHaveBeenCalled(consoleSpy);
    });
  });

  describe("error message filtering", () => {
    test("should accept an array of ignored messages", () => {
      let (logger, consoleSpy) =
        setup(~ignoredErrorMessages=[|"error 1", "error 2"|], ());
      let (e1, _, _) = JsError.make("error 1");
      let (e2, _, _) = JsError.make("error 2");
      let (e3, stack3, _) = JsError.make("error 3");
      logger->TestLogger.error(e1);
      logger->TestLogger.error(e2);
      logger->TestLogger.error(e3);
      expectSpyToHaveBeenCalledWith(consoleSpy, stack3);
    });

    test("should work for warnings", () => {
      let (logger, consoleSpy) =
        setup(~ignoredErrorMessages=[|"My error"|], ());
      logger->TestLogger.warn("My error");
      expectSpyNotToHaveBeenCalled(consoleSpy);
    });
  });
});
