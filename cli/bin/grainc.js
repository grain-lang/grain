// A wrapper around grainc_js.bc.js that prepares the `pkg` env
const preparePkg = require("./pkg");

preparePkg();

require("./grainc_js.bc.js");
