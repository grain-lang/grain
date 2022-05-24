const path = require("path");
const { Worker } = require("near-workspaces");

// NOTE:
// This test suite exists to ensure wasm output does not contain multivalue functions.
// If no multivalue functions exist, Grain smart contracts should run without issue.

let worker;

beforeAll(async () => {
  worker = await Worker.init();
});

afterAll(async () => {
  await worker.tearDown();
});

describe("Runs in NEAR sandbox", () => {
  jest.setTimeout(60_000);

  test("it should not produce wasm multivalue function", async () => {
    const root = worker.rootAccount;
    const contract = await root.createAndDeploy(
      "grain-near-multivalue-test",
      path.join(__dirname, "index.gr.wasm")
    );
    expect(await contract.view("hello")).toEqual("Hello, World!");
  });
});
