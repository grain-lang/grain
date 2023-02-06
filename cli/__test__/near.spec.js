const path = require("path");

// NOTE:
// This test suite exists to ensure wasm output does not contain multivalue functions.
// If no multivalue functions exist, Grain smart contracts should run without issue.

let worker;
afterAll(async () => {
  await worker?.tearDown();
});

describe("Runs in NEAR sandbox", () => {
  jest.setTimeout(100_000_000);
  let Worker;
  let SandboxServer;
  try {
    Worker = require("near-workspaces").Worker;
    SandboxServer = require("near-workspaces").SandboxServer;
  } catch (err) {
    // no-op
  }
  // near-workspaces is optional, so if it's not installed, we can skip tests
  const testIf = !Worker ? test.skip : test;
  testIf("it should not produce wasm multivalue function", async () => {
    const port = await SandboxServer.nextPort();
    // NEAR hasn't accepted https://github.com/near/workspaces-js/pull/200
    // so we hack around their lack of Node 18+ support
    const rpcAddr = `http://127.0.0.1:${port}`;
    SandboxServer.prototype.rpcAddr = rpcAddr;
    Worker.prototype.rpcAddr = rpcAddr;
    worker = await Worker.init({
      network: "sandbox",
      rpcAddr,
    });
    const root = worker.rootAccount;
    const contract = await root.devDeploy(
      path.join(__dirname, "index.gr.wasm")
    );
    expect(await contract.view("hello")).toEqual("Hello, World!");
  });
});
