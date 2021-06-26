const exec = require("./exec");

module.exports = (file, program) => {
  const options = program.opts();
  try {
    exec.grainc(file, program, { stdio: "inherit" });
    if (options.o) {
      return options.o;
    } else {
      return file.replace(/\.gr$/, ".gr.wasm");
    }
  } catch (e) {
    if (options.graceful) {
      process.exit();
    } else {
      process.exit(1);
    }
  }
};
