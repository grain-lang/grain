import { ForwardOption, optionsToFlags, runCommand } from "../utils.js";

export const format = async (file, opts, program) => {
  return await runCommand("grainformat", optionsToFlags(program, opts, [file]));
};

export default (cli, unprocessedArgs) => {
  cli
    .command("format <file|dir>")
    .description("format a grain file")
    .addOption(new ForwardOption("-o <file|dir>", "output file or directory"))
    .action(async (file, opts, program) => {
      await format(file, opts, program);
    });
  return cli;
};
