const exec = require("./exec");

module.exports = function (program) {
  exec.grainlsp(program, { stdio: "inherit" });
};
