const path = require("path");

// NOTE:
// This test suite exists to ensure wasm output does not contain multivalue functions.
// If no multivalue functions exist, Grain smart contracts should run without issue.

let worker;
afterAll(async () => {
  await worker?.tearDown();
});

describe("Runs in NEAR sandbox", () => {
  jest.setTimeout(60_000);
  let Worker;
  try {
    Worker = require("near-workspaces").Worker;
  } catch (err) {
    // no-op
  }
  // near-workspaces is optional, so if it's not installed, we can skip tests
  const testIf = !Worker ? test.skip : test;
  testIf("it should not produce wasm multivalue function", async () => {
    worker = await Worker.init();
    const root = worker.rootAccount;
    const contract = await root.createAndDeploy(
      "grain-near-multivalue-test",
      path.join(__dirname, "index.gr.wasm")
    );
    expect(await contract.view("hello")).toEqual("Hello, World!");
  });
});
