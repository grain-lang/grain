import { optionsToFlags, runCommand } from "../utils.js";

export const lsp = async (opts, program) => {
  return await runCommand("grainlsp", optionsToFlags(program, opts, []));
};

export default (cli, unprocessedArgs) => {
  cli
    .command("lsp")
    .description("start the Grain LSP server")
    .action(async (opts, program) => {
      await lsp(opts, program);
    });
  return cli;
};
