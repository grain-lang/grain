require("./pkg");

const { readFile } = require("fs/promises");
const { WASI } = require("wasi");
const { argv, env } = require("process");

const preopens = JSON.parse(env.PREOPENS);
const envVars = JSON.parse(env.ENV_VARS);

const wasi = new WASI({
  args: argv.slice(2),
  env: envVars,
  preopens,
  version: "preview1",
  returnOnExit: false,
});

const importObject = { wasi_snapshot_preview1: wasi.wasiImport };

async function run(filename) {
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
    wasm = await WebAssembly.compile(bytes);
  } catch (err) {
    if (filename.endsWith(".gr")) {
      console.error(
        `The \`grain run\` command is used on compiled \`.wasm\` files.`
      );
      console.error(
        `To compile and run your \`.gr\` file, use \`grain ${filename}\``
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
}

run(argv[2]);
