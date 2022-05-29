#!/usr/bin/env node

// https://github.com/grain-lang/grain/issues/114
const v8 = require("v8");
/* From the Node.js docs:
 *
 *   The v8.setFlagsFromString() method can be used to programmatically set V8 command-line flags.
 *   This method should be used with care. Changing settings after the VM has started may result
 *   in unpredictable behavior, including crashes and data loss; or it may simply do nothing.
 *
 * This seems to work for our needs with Node 16, but we should be cautious when updating.
 */
v8.setFlagsFromString("--experimental-wasm-return-call");

const program = require("commander");
const exec = require("./exec.js");
const compile = require("./compile.js");
const run = require("./run.js");
const lsp = require("./lsp.js");
const doc = require("./doc.js");
const format = require("./format.js");

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

class ForwardOption extends program.Option {
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

class ProfileOption extends program.Option {
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

const optionApplicator = (Option) =>
  function (flags, description, parser, defaultValue) {
    const option = new Option(flags, description);
    if (parser) option.argParser(parser);
    if (typeof defaultValue !== "undefined") option.default(defaultValue);
    return this.addOption(option);
  };

// Adds .forwardOption to commands. Similar to Commander's native .option,
// but will forward the flag to the underlying program.
program.Command.prototype.forwardOption = optionApplicator(ForwardOption);

// Adds .profileOption to commands. Similar to Commander's native .option,
// but will convert the flag from the shorthand to the full form.
program.Command.prototype.profileOption = optionApplicator(ProfileOption);

program
  .option("-v, --version", "output CLI and compiler versions")
  .on("option:version", () => {
    console.log(`Grain cli ${require("../package.json").version}`);
    console.log(`Grain compiler ${graincVersion()}`);
    process.exit(0);
  })
  .description("Compile and run Grain programs. 🌾")
  .addOption(new program.Option("-p, --print-output").hideHelp())
  .forwardOption(
    "-I, --include-dirs <dirs>",
    "add additional dependency include directories",
    list,
    []
  )
  .forwardOption(
    "-S, --stdlib <path>",
    "override the standard libary with your own",
    null,
    stdlibPath
  )
  .forwardOption(
    "--initial-memory-pages <size>",
    "initial number of WebAssembly memory pages",
    num
  )
  .forwardOption(
    "--maximum-memory-pages <size>",
    "maximum number of WebAssembly memory pages",
    num
  )
  .forwardOption(
    "--compilation-mode <mode>",
    "compilation mode (advanced use only)"
  )
  .forwardOption(
    "--elide-type-info",
    "don't include runtime type information used by toString/print"
  )
  .profileOption(
    "--release",
    "compile using the release profile (production mode)"
  )
  .forwardOption(
    "--experimental-wasm-tail-call",
    "enables tail-call optimization"
  )
  .forwardOption("--debug", "compile with debugging information")
  .forwardOption("--wat", "additionally produce a WebAssembly Text (.wat) file")
  .forwardOption(
    "--hide-locs",
    "hide locations from intermediate trees. Only has an effect with `--verbose`"
  )
  .forwardOption("--lsp", "generate lsp errors and warnings only")
  .forwardOption("--no-color", "disable colored output")
  .forwardOption("--no-gc", "turn off reference counting garbage collection")
  .forwardOption(
    "--no-bulk-memory",
    "polyfill WebAssembly bulk memory instructions"
  )
  .forwardOption(
    "--wasi-polyfill <filename>",
    "path to custom WASI implementation"
  )
  .forwardOption(
    "--use-start-section",
    "replaces the _start export with a start section during linking"
  )
  .forwardOption("--no-link", "disable static linking")
  .forwardOption(
    "--no-pervasives",
    "don't automatically import the Grain Pervasives module"
  )
  .forwardOption("-o <filename>", "output filename")
  .forwardOption("-O <level>", "set the optimization level")
  .forwardOption(
    "--parser-debug-level <level>",
    "debugging level for parser output"
  )
  .forwardOption(
    "--memory-base <addr>",
    "set the base address for the Grain heap"
  )
  .forwardOption("--source-map", "generate source maps")
  .forwardOption("--strict-sequence", "enable strict sequencing")
  .forwardOption(
    "--verbose",
    "print critical information at various stages of compilation"
  )
  // The root command that compiles & runs
  .arguments("<file>")
  .action(function (file, options, program) {
    run(compile(file, program), options);
  });

program
  .command("compile <file>")
  .description("compile a grain program into wasm")
  .action(function (file) {
    // The compile subcommand inherits all behaviors/options of the
    // top level grain command
    compile(file, program);
  });

program
  .command("run <file>")
  .description("run a wasm file with grain's javascript runner")
  .action(function (wasmFile) {
    // The run subcommand inherits all options of the
    // top level grain command
    run(wasmFile, program.opts());
  });

program
  .command("lsp <file>")
  .description("check a grain file for LSP")
  .action(function (file) {
    // The lsp subcommand inherits all options of the
    // top level grain command
    lsp(file, program);
  });

program
  .command("doc <file|dir>")
  .description("generate documentation for a grain file")
  .forwardOption(
    "--current-version <version>",
    "provide a version to use as current when generating markdown for `@since` and `@history` attributes"
  )
  .action(function (file, options, program) {
    doc(file, program);
  });

program
  .command("format <file|dir>")
  .description("format a grain file")
  .action(function (file, options, program) {
    format(file, program);
  });
program.parse(process.argv);
