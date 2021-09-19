const exec = require("./exec");

module.exports = function (file, program) {
  exec.grainformat(file, program, { stdio: "inherit" });
};
