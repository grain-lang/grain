require("./pkg");

const { readFile } = require("fs/promises");
const { WASI } = require("wasi");
const { argv, env } = require("process");

delete env.NODE_OPTIONS;

const wasi = new WASI({
  args: argv.slice(3),
  env,
  preopens: {
    "/sandbox": process.cwd(),
  },
});

const importObject = { wasi_snapshot_preview1: wasi.wasiImport };

async function run(filename) {
  const wasm = await WebAssembly.compile(await readFile(filename));
  const instance = await WebAssembly.instantiate(wasm, importObject);

  wasi.start(instance);
}

run(argv[2]);
