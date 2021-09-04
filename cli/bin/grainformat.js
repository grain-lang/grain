// A wrapper around grainformat_js.bc.js that prepares the `pkg` env
const preparePkg = require("./pkg");

preparePkg();

require("./grainformat_js.bc.js");
