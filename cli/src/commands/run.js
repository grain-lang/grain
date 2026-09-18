// NOTE: destructuring is safe here because we would never be working off the vfs
import { readFile } from "node:fs/promises";
import process from "node:process";

// NOTE: for some reason esm isn't respecting the `--no-warnings` flag package.json so we manually remove the warning listener here to avoid the ExperimentalWarning from being printed to the console
process.removeAllListeners("warning");

const { WASI } = await import("node:wasi");

export const run = async (filename, opts, unprocessedArgs) => {
  const preopens = {};
  opts.dir?.forEach((preopen) => {
    const [guestDir, hostDir = guestDir] = preopen.split("=");
    preopens[guestDir] = hostDir;
  });

  const cliEnv = {};
  opts.env?.forEach((env) => {
    const [name, ...rest] = env.split("=");
    const val = rest.join("=");
    cliEnv[name] = val;
  });

  const wasi = new WASI({
    args: [filename, ...unprocessedArgs],
    env: cliEnv,
    preopens: preopens,
    version: "preview1",
    returnOnExit: false,
  });

  const importObject = { wasi_snapshot_preview1: wasi.wasiImport };

  let bytes;
  try {
    bytes = await readFile(filename);
  } catch (err) {
    console.error(`Unable to read file: ${filename}`);
    process.exitCode = 1;
    return;
  }

  let wasm;
  try {
    wasm = await WebAssembly.compile(new Uint8Array(bytes));
  } catch (err) {
    if (filename.endsWith(".gr")) {
      console.error(
        `The \`grain run\` command is used on compiled \`.wasm\` files.`,
      );
      console.error(
        `To compile and run your \`.gr\` file, use \`grain ${filename}\``,
      );
    } else {
      console.error(`Unable to compile WebAssembly module.`);
      console.error(err.stack);
    }
    process.exitCode = 1;
    return;
  }

  let instance;
  try {
    instance = await WebAssembly.instantiate(wasm, importObject);
  } catch (err) {
    console.error(`Unable to instantiate WebAssembly module.`);
    console.error(err.stack);
    process.exitCode = 1;
    return;
  }

  try {
    wasi.start(instance);
  } catch (err) {
    console.error(err.stack);
    process.exitCode = 1;
    return;
  }
};
// Setup the command
export default (cli, unprocessedArgs) => {
  cli
    .command("run <file:string>")
    .description("run a wasm file via grain's WASI runner")
    .option("--dir <dir...>", "directory to preopen")
    .option("--env <env...>", "WASI environment variables")
    .action(async (file, opts, program) => {
      await run(file, opts, unprocessedArgs);
    });

  return cli;
};
