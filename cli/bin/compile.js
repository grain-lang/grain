const exec = require("./exec");

module.exports = (file, program) => {
  const options = program.opts();
  exec.grainc(file, program, { stdio: "inherit" });
  if (options.o) {
    return options.o;
  } else {
    return file.replace(/\.gr$/, ".gr.wasm");
  }
};
