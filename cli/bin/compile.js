const exec = require("./exec");

module.exports = (file, options) => {
  try {
    exec(file, options, { stdio: "inherit" });
    return file.replace(/\.gr$/, ".gr.wasm");
  } catch (e) {
    if (options.graceful) {
      process.exit();
    } else {
      process.exit(1);
    }
  }
};
