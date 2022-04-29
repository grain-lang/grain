// A wrapper around grainlsp.bc.js that prepares the `pkg` env
const preparePkg = require("./pkg");

preparePkg();

require("./grainlsp.bc.js");