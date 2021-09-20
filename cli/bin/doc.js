const exec = require("./exec");

module.exports = (file, program) => {
  exec.graindoc(file, program, { stdio: "inherit" });
};
