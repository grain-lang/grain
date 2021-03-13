const exec = require("./exec");

module.exports = (file, options) => {
  try {
    // TODO: Should we console.log the output (stdout) of the exec?
    exec(file, options);
    return file.replace(/\.gr$/, ".gr.wasm");
  } catch (e) {
    console.log(e.stdout.toString());
    // TODO: Do we also want to print the stderr?
    // console.log(e.stderr.toString());
    if (options.graceful) {
      process.exit();
    } else {
      process.exit(1);
    }
  }
};
