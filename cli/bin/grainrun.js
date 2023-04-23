require("./pkg");

// https://github.com/grain-lang/grain/issues/1816
const v8 = require("v8");
/* From the Node.js docs:
 *
 *   The v8.setFlagsFromString() method can be used to programmatically set V8 command-line flags.
 *   This method should be used with care. Changing settings after the VM has started may result
 *   in unpredictable behavior, including crashes and data loss; or it may simply do nothing.
 *
 * This seems to work for our needs with Node 18, but we should be cautious when updating.
 */
v8.setFlagsFromString("--experimental-wasm-return-call");

const { readFile } = require("fs/promises");
const { WASI } = require("wasi");
const { argv, env } = require("process");

const preopens = JSON.parse(env.PREOPENS);

delete env.PREOPENS;
delete env.NODE_OPTIONS;

const wasi = new WASI({
  args: argv.slice(2),
  env,
  preopens,
});

const importObject = { wasi_snapshot_preview1: wasi.wasiImport };

async function run(filename) {
  try {
    const wasm = await WebAssembly.compile(await readFile(filename));
    const instance = await WebAssembly.instantiate(wasm, importObject);

    wasi.start(instance);
  } catch (err) {
    console.error(err.stack);
    process.exit(1);
  }
}

run(argv[2]);
