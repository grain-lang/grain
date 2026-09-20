import { ForwardOption, optionsToFlags, runCommand } from "../utils.js";

export const doc = async (file, opts, program) => {
  return await runCommand("graindoc", optionsToFlags(program, opts, [file]));
};

export default (cli, unprocessedArgs) => {
  cli
    .command("doc <file|dir>")
    .description("generate documentation for a grain file")
    .addOption(
      new ForwardOption(
        "--current-version <version>",
        "provide a version to use as current when generating markdown for `@since` and `@history` attributes",
      ),
    )
    .addOption(new ForwardOption("-o <file|dir>", "output file or directory"))
    .action(async (file, opts, program) => {
      await doc(file, opts, program);
    });
  return cli;
};
