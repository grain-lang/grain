// A wrapper around grainc_js.bc.js that prepares the `pkg` env
const program = require("commander");
const preparePkg = require("./pkg");

program.option("--stdlib <path>").allowUnknownOption().parse(process.argv);

const { stdlib } = program.opts();

preparePkg();

require("./grainc_js.bc.js");
