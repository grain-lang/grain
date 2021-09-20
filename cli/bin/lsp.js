const exec = require("./exec");

// call the compiler, passing stdio through so the compiler gets the source code on stdin
// and we get the compiler output in stdout
// we still take the file name so we have it available

module.exports = (file, program) => {
  exec.grainc(`--lsp ${file}`, program, { stdio: "inherit" });
};
