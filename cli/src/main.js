#!/usr/bin/env node --disable-warning=ExperimentalWarning
import { env } from "node:process";

// NOTE: This is a bit of a hack to allow us to run sub commands
// from the compiled executable, without having to extract the compiled commands.

const internalFlag = env.GRAIN_INTERNAL; // This is set by `utils.runCommand`
if (internalFlag == undefined) {
  await import("./cli.js");
} else {
  // NOTE: Calling the fs patch here ensures that the `fs` module is patched before running.
  await import("./fs_patch.cjs");
  // The internal flag is a JSON string containing the information required to run the command.
  const internalConfig = JSON.parse(internalFlag);
  globalThis.process.argv = ["", internalConfig.script, ...internalConfig.args];
  await import(internalConfig.script);
}
