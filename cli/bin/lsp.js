const exec = require("./exec");

module.exports = function (program) {

  const options = program.opts();
 
  exec.grainlsp(program, { stdio: "inherit" });
};
