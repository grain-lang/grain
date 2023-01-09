require("./pkg");

const { readFile } = require("node:fs/promises");
const { WASI } = require("node:wasi");
const { argv, env } = require("node:process");

const wasi = new WASI({
  args: argv,
  env,
  preopens: {
    "/sandbox": process.cwd(),
  },
});

const importObject = { wasi_snapshot_preview1: wasi.wasiImport };

module.exports = async function run(filename) {
  const wasm = await WebAssembly.compile(await readFile(filename));
  const instance = await WebAssembly.instantiate(wasm, importObject);

  wasi.start(instance);
};
