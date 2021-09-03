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
const exec = require("./exec.js");
const compile = require("./compile.js");
const run = require("./run.js");
const lsp = require("./lsp.js");
const doc = require("./doc.js");

const stdlibPath = require("@grain/stdlib");

function list(val) {
  return val.split(",");
}

function num(val) {
  return Number.parseInt(val, 10);
}

function graincVersion() {
  return exec.grainc("--version", program).toString().trim();
}

class GraincOption extends program.Option {
  grainc = true;

  toFlag() {
    const value = program.opts()[this.attributeName()];

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

program.graincOption = function (flags, description, parser, defaultValue) {
  const option = new GraincOption(flags, description);
  if (parser) option.argParser(parser);
  if (typeof defaultValue !== "undefined") option.default(defaultValue);
  return program.addOption(option);
};

program
  .option("-v, --version", "output CLI and compiler versions")
  .on("option:version", () => {
    console.log(`Grain cli ${require("../package.json").version}`);
    console.log(`Grain compiler ${graincVersion()}`);
    process.exit(0);
  })
  .description("Compile and run Grain programs. 🌾")
  .addOption(new program.Option("-p, --print-output").hideHelp())
  .option("-g, --graceful", "return a 0 exit code if the program errors")
  .graincOption(
    "-I, --include-dirs <dirs>",
    "add additional dependency include directories",
    list,
    []
  )
  .graincOption(
    "-S, --stdlib <path>",
    "override the standard libary with your own",
    null,
    stdlibPath
  )
  .graincOption(
    "--initial-memory-pages <size>",
    "initial number of WebAssembly memory pages",
    num
  )
  .graincOption(
    "--maximum-memory-pages <size>",
    "maximum number of WebAssembly memory pages",
    num
  )
  .graincOption(
    "--compilation-mode <mode>",
    "compilation mode (advanced use only)"
  )
  .graincOption(
    "--elide-type-info",
    "don't include runtime type information used by toString/print"
  )
  .graincOption(
    "--experimental-wasm-tail-call",
    "enables tail-call optimization"
  )
  .graincOption("--debug", "compile with debugging information")
  .graincOption("--wat", "additionally produce a WebAssembly Text (.wat) file")
  .graincOption(
    "--hide-locs",
    "hide locations from intermediate trees. Only has an effect with `--verbose`"
  )
  .graincOption("--lsp", "generate lsp errors and warnings only")
  .graincOption("--no-color", "disable colored output")
  .graincOption("--no-gc", "turn off reference counting garbage collection")
  .graincOption(
    "--no-bulk-memory",
    "polyfill WebAssembly bulk memory instructions"
  )
  .graincOption(
    "--wasi-polyfill <filename>",
    "path to custom WASI implementation"
  )
  .graincOption(
    "--use-start-section",
    "replaces the _start export with a start section during linking"
  )
  .graincOption("--no-link", "disable static linking")
  .graincOption(
    "--no-pervasives",
    "don't automatically import the Grain Pervasives module"
  )
  .graincOption("-o <filename>", "output filename")
  .graincOption("-O <level>", "set the optimization level")
  .graincOption(
    "--parser-debug-level <level>",
    "debugging level for parser output"
  )
  .graincOption("--source-map", "generate source maps")
  .graincOption("--strict-sequence", "enable strict sequencing")
  .graincOption(
    "--verbose",
    "print critical information at various stages of compilation"
  )
  // The root command that compiles & runs
  .arguments("<file>")
  .action(function (file) {
    run(compile(file, program), program.opts());
  });

program
  .command("compile <file>")
  .description("compile a grain program into wasm")
  .action(function (file) {
    compile(file, program);
  });

program
  .command("lsp <file>")
  .description("check a grain file for LSP")
  .action(function (file) {
    lsp(file, program);
  });

program
  .command("run <file>")
  .description("run a wasm file with grain's javascript runner")
  .action(function (wasmFile) {
    run(wasmFile, program.opts());
  });

program
  .command("doc <file>")
  .description("generate documentation for a grain file")
  .action(function (file) {
    doc(file, program);
  });

program.parse(process.argv);
