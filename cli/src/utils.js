import { Option } from "commander";
import { execSync } from "node:child_process";
import { env, execPath } from "node:process";
import { fileURLToPath } from "node:url";
// import path from "node:path";
import fs from "fs";

/**
 * A ForwardOption is an option that is forwarded to the underlying ocaml program.
 */
export class ForwardOption extends Option {
  // A ForwardOption is forwarded to the underlying program
  forward = true;
  toFlag(opts) {
    const value = opts[this.attributeName()];
    if (value instanceof Array && value.length > 0) {
      return `${this.long || this.short} ${value.join(",")}`;
    } else if (typeof value === "string" || typeof value === "number") {
      return `${this.long || this.short} ${value}`;
    } else if (
      (this.negate && value === false) ||
      (!this.negate && value === true)
    ) {
      return this.long || this.short;
    }
  }
}

export class ProfileOption extends Option {
  // Like ForwardOption, ProfileOption is forwarded to the underlying program
  // but we convert the flag into a profile flag, i.e. `--release` becomes `--profile=release`
  forward = true;
  toFlag(opts) {
    const attribute = this.attributeName();
    if (opts[attribute]) {
      return `--profile=${attribute}`;
    }
  }
}

/** A parser for converting comma-separated strings into arrays. */
export const listParser = (val) => val.split(",").map((s) => s.trim());
/** A parser for converting strings to integers. */
export const intParser = (val) => Number.parseInt(val, 10);

/**
 * This function is responsible for converting Commander options into CLI flags that can be passed to the underlying ocaml tooling.
 *
 * @param {*} [program]: The commander program instance containing the options to convert
 * @param {*} [options]: The options object containing the values of the options to convert
 * @param {string[]} [positional]: An array of positional arguments to include in the output
 * @returns An array of CLI flags corresponding to the provided options and positional arguments
 */
export const optionsToFlags = (program, options, positional) => {
  const flags = [];
  program.options.forEach((option) => {
    if (!option.forward) return;
    const flag = option.toFlag(options);
    if (flag) flags.push(flag);
  });
  return [...flags, ...positional];
};

/**
 * This function is responsible for determining the default location of the compiled wasm file according to grains conventions.
 *
 * @param {string} file: The file being compiled
 * @param {Object} options: The options passed to the compiler of particular interest are `targetDir` and `release` which determine the target directory and profile respectively
 * @returns {string} The default wasm output location
 */
export const defaultWasmLocation = (file, options) => {
  // TODO: Enable when we release 0.8.0 (alternatively add a check for the grain version)
  // const targetDir = options.targetDir
  //   ? path.resolve(options.targetDir)
  //   : path.resolve("target");
  // const profile = options.release ? "release" : "debug";
  // const basename = path.basename(file).replace(/\.gr$/, ".wasm");
  // return path.join(targetDir, profile, basename);
  return file.replace(/\.gr$/, ".wasm");
};

const exec = (command, args, options) => {
  try {
    execSync(`${command} ${args.join(" ")}`, options);
    return true;
  } catch (err) {
    process.exitCode = err?.status ?? 1;
    return false;
  }
};

/**
 * This function is responsible for running a grain command, either by running the compiled
 * executable if available, or by running the compiled JavaScript file with `node run` if
 * the executable is not available.
 *
 * @param {string} name: The name of the command to run (e.g. "grainc", "grainfmt", etc.)
 * @param {string[]} args: The cli arguments to pass to the command (e.g. ["--release", "src/index.gr"])
 * @returns {boolean} A promise resolving to a boolean indicating whether the command succeeded.
 */
export async function runCommand(name, args) {
  if (!name) throw new Error("No command provided");

  const base = new URL("./artifacts/", import.meta.url);
  const exe = new URL(`${name}.exe`, base);
  const exePath = fileURLToPath(exe);
  const js = new URL(`${name}.bc.cjs`, base);
  const jsPath = fileURLToPath(js);
  // Check if the executable exists. If it does, we can run it directly.
  const isExe = await fs.promises
    .stat(exePath)
    .then(() => true)
    .catch(() => false);

  const success = isExe
    ? exec(exePath, args, { stdio: "inherit" })
    : exec(execPath, [jsPath, ...args], {
        env: {
          ...env,
          // NOTE: This is used to redirect `main.js` to the correct command
          // when running from the compiled executable
          GRAIN_INTERNAL: JSON.stringify({
            script: jsPath,
          }),
        },
        stdio: "inherit",
      });

  return success;
}
