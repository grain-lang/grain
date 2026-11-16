import { ForwardOption, optionsToFlags, runCommand } from "../utils.js";

export const compile = async (file, opts, program) => {
  return await runCommand("grainc", optionsToFlags(program, opts, [file]));
};

export default (cli, unprocessedArgs) => {
  cli
    .command("compile <file>")
    .description("compile a grain program into wasm")
    .addOption(new ForwardOption("-o <filename>", "output filename"))
    .addOption(
      new ForwardOption(
        "--single-file",
        "compile a single file without compiling dependencies",
      ),
    )
    .addOption(
      new ForwardOption(
        "--use-start-section",
        "replaces the _start export with a start section during linking",
      ),
    )
    .addOption(new ForwardOption("--no-link", "disable static linking"))
    .action(async (file, opts, program) => {
      await compile(file, opts, program);
    });
  return cli;
};
