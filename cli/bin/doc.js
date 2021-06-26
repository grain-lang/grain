const exec = require("./exec");

module.exports = (file, program) => {
  try {
    exec.graindoc(file, program, { stdio: "inherit" });
    process.exit();
  } catch (e) {
    if (program.opts().graceful) {
      process.exit();
    } else {
      process.exit(1);
    }
  }
};
