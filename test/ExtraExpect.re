open Jest;

let expectSpyToHaveBeenCalledWith = (spy, expectedValue) => {
  Expect.(expect(spy |> MockJs.calls) |> toEqual([|expectedValue|]));
};

let expectSpyToHaveBeenCalledWithStringContaining = (spy, expectedValue) => {
  let actualValue = MockJs.calls(spy)[0];
  Expect.(expect(actualValue) |> toContainString(expectedValue));
};

let expectSpyNotToHaveBeenCalled = spy =>
  Expect.(expect(spy |> MockJs.calls |> Array.length) |> toEqual(0));

let expectSpiesToHaveBeenCalled = spies => {
  let actualCalls =
    spies |> Array.map(spy => spy |> MockJs.calls |> Array.length);
  let expectedCalls = Array.make(spies |> Array.length, 1);
  Expect.(expect(actualCalls) |> toEqual(expectedCalls));
};

let expectSpiesToHaveBeenCalledWith = (spies, param) => {
  let actualCalls = spies |> Array.map(spy => spy |> MockJs.calls);
  let expectedCalls = Array.make(spies |> Array.length, [|param|]);
  Expect.(expect(actualCalls) |> toEqual(expectedCalls));
};
