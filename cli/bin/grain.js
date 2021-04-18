#!/usr/bin/env node

// https://github.com/grain-lang/grain/issues/114
const v8 = require("v8");
/* From the Node.js docs:
 *
 *   The v8.setFlagsFromString() method can be used to programmatically set V8 command-line flags.
 *   This method should be used with care. Changing settings after the VM has started may result
 *   in unpredictable behavior, including crashes and data loss; or it may simply do nothing.
 *
 * This seems to work for our needs with Node 14, but we should be cautious when updating.
 */
v8.setFlagsFromString("--experimental-wasm-bigint");
v8.setFlagsFromString("--experimental-wasm-return-call");

const program = require("commander");
const exec = require("./exec");

// Workaround to defer loading of the grain runtime until the memory settings have been parsed
let actions = {
  get compile() {
    return require("./compile.js");
  },
  get run() {
    return require("./run.js");
  },
  get lsp() {
    return require("./lsp.js");
  },
};

const stdlibPath = require("@grain/stdlib");

function list(val) {
  return val.split(",");
}

function num(val) {
  return Number.parseInt(val, 10);
}

function graincVersion() {
  return exec("--version").toString().trim();
}

program
  .option("-v, --version", "output CLI and compiler versions")
  .on("option:version", () => {
    console.log(`Grain cli ${require("../package.json").version}`);
    console.log(`Grain compiler ${graincVersion()}`);
    process.exit(0);
  })
  .description("Compile and run Grain programs. 🌾")
  .option("-p, --print-output", "print the output of the program")
  .option("-g, --graceful", "return a 0 exit code if the program errors")
  .option(
    "-I, --include-dirs <dirs>",
    "include directories the runtime should find wasm modules",
    list,
    []
  )
  .option("-f, --cflags <cflags>", "pass flags to the Grain compiler")
  .option(
    "-S, --stdlib <path>",
    "override the standard libary with your own",
    stdlibPath
  )
  .option("--limitMemory <size>", "maximum allowed heap size", num, -1)
  .option(
    "--init-memory-pages <size>",
    "number of pages used to initialize the grain runtime"
  )
  .on("option:init-memory-pages", (pages) => {
    // Workaround for the runtime's memory being initialized statically on module load
    process.env.GRAIN_INIT_MEMORY_PAGES = parseInt(pages, 10);
  })
  // The root command that compiles & runs
  .arguments("<file>")
  .action(function (file) {
    actions.run(actions.compile(file, program), program);
  });

program
  .command("compile <file>")
  .description("compile a grain program into wasm")
  .action(function (file) {
    actions.compile(file, program);
  });

program
  .command("lsp <file>")
  .description("check a grain file for LSP")
  .action(function (file) {
    actions.lsp(file, program);
  });

program
  .command("run <file>")
  .description("run a wasm file with the grain runtime")
  .action(function (wasmFile) {
    actions.run(wasmFile, program);
  });

program.parse(process.argv);
