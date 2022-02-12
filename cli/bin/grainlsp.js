// A wrapper around grainlsp_js.bc.js that prepares the `pkg` env
const preparePkg = require("./pkg");

preparePkg();

require("./grainlsp_js.bc.js");
