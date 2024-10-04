#!/usr/bin/env node

const commander = require("commander");
const exec = require("./exec.js");
const pkgJson = require("../package.json");

const stdlibPath = require("@grain/stdlib");

function list(val) {
  return val.split(",");
}

function num(val) {
  return Number.parseInt(val, 10);
}

class ForwardOption extends commander.Option {
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

class ProfileOption extends commander.Option {
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

class GrainHelp extends commander.Help {
  visibleOptions(cmd) {
    // If we are running `--help` at the root, we want to list options for `compile-and-run`
    if (cmd.name() === "grain") {
      return super.visibleOptions(
        cmd.commands.find((command) => command.name() === "compile-and-run")
      );
    }
    return super.visibleOptions(cmd);
  }
}

const optionApplicator = (Option) =>
  function (flags, description, parser, defaultValue) {
    const option = new Option(flags, description);
    if (parser) option.argParser(parser);
    if (typeof defaultValue !== "undefined") option.default(defaultValue);
    return this.addOption(option);
  };

class GrainCommand extends commander.Command {
  // Adds .forwardOption to commands. Similar to Commander's native .option,
  // but will forward the flag to the underlying program.
  forwardOption = optionApplicator(ForwardOption);

  // Adds .profileOption to commands. Similar to Commander's native .option,
  // but will convert the flag from the shorthand to the full form.
  profileOption = optionApplicator(ProfileOption);

  createHelp() {
    return new GrainHelp();
  }

  createCommand(name) {
    const cmd = new GrainCommand(name);
    // Add global options to command
    cmd.forwardOption(
      "-I, --include-dirs <dirs>",
      "add additional dependency include directories",
      list,
      []
    );
    cmd.forwardOption(
      "-S, --stdlib <path>",
      "override the standard library with your own",
      null,
      stdlibPath
    );
    cmd.forwardOption(
      "--initial-memory-pages <size>",
      "initial number of WebAssembly memory pages",
      num
    );
    cmd.forwardOption(
      "--maximum-memory-pages <size>",
      "maximum number of WebAssembly memory pages",
      num
    );
    cmd.forwardOption("--import-memory", "import the memory from `env.memory`");
    cmd.option("--dir <dir...>", "directory to preopen");
    cmd.option("--env <env...>", "WASI environment variables");
    cmd.forwardOption(
      "--elide-type-info",
      "don't include runtime type information used by toString/print"
    );
    cmd.profileOption(
      "--release",
      "compile using the release profile (production mode)"
    );
    cmd.forwardOption("--no-wasm-tail-call", "disables tail-call optimization");
    cmd.forwardOption("--debug", "compile with debugging information");
    cmd.forwardOption(
      "--wat",
      "additionally produce a WebAssembly Text (.wat) file"
    );
    cmd.forwardOption(
      "--hide-locs",
      "hide locations from intermediate trees. Only has an effect with `--verbose`"
    );
    cmd.forwardOption("--no-color", "disable colored output");
    cmd.forwardOption(
      "--no-gc",
      "turn off reference counting garbage collection"
    );
    cmd.forwardOption(
      "--no-bulk-memory",
      "polyfill WebAssembly bulk memory instructions"
    );
    cmd.forwardOption(
      "--wasi-polyfill <filename>",
      "path to custom WASI implementation"
    );
    cmd.forwardOption(
      "--no-pervasives",
      "don't automatically import the Grain Pervasives module"
    );
    cmd.forwardOption(
      "--memory-base <addr>",
      "set the base address for the Grain heap"
    );
    cmd.forwardOption("--source-map", "generate source maps");
    cmd.forwardOption("--strict-sequence", "enable strict sequencing");
    cmd.forwardOption(
      "--verbose",
      "print critical information at various stages of compilation"
    );
    return cmd;
  }
}

let endOptsI = process.argv.findIndex((x) => x === "--");
if (endOptsI === -1) {
  endOptsI = Infinity;
}
const argsToProcess = process.argv.slice(0, endOptsI);
const unprocessedArgs = process.argv.slice(endOptsI + 1);

const program = new GrainCommand();

program
  .description("Compile and run Grain programs. 🌾")
  // Show the default usage without "compile-and-run"
  .usage("[options] <file>")
  .addHelpCommand(false)
  // The default command that compiles & runs
  .command("compile-and-run <file>", { isDefault: true, hidden: true })
  // `--version` should only be available on the default command
  .version(pkgJson.version, "-v, --version", "output the current version")
  .forwardOption("-o <filename>", "output filename")
  .action(function (file, options, program) {
    const success = exec.grainc(file, options, program);
    if (success) {
      const outFile = options.o ?? file.replace(/\.gr$/, ".gr.wasm");
      exec.grainrun(unprocessedArgs, outFile, options, program);
    }
  });

program
  .command("compile <file>")
  .description("compile a grain program into wasm")
  .forwardOption("-o <filename>", "output filename")
  .forwardOption(
    "--use-start-section",
    "replaces the _start export with a start section during linking"
  )
  .forwardOption("--no-link", "disable static linking")
  .action(exec.grainc);

program
  .command("run <file>")
  .description("run a wasm file via grain's WASI runner")
  .action((...args) => exec.grainrun(unprocessedArgs, ...args));

program
  .command("lsp")
  .description("start the Grain LSP server")
  .action(exec.grainlsp);

program
  .command("doc <file|dir>")
  .description("generate documentation for a grain file")
  .forwardOption(
    "--current-version <version>",
    "provide a version to use as current when generating markdown for `@since` and `@history` attributes"
  )
  .forwardOption("-o <file|dir>", "output file or directory")
  .action(exec.graindoc);

program
  .command("format <file|dir>")
  .description("format a grain file")
  .forwardOption("-o <file|dir>", "output file or directory")
  .action(exec.grainformat);

program.parse(argsToProcess);
