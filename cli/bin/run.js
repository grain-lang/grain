const path = require("path");
const { execSync } = require("child_process");
require("./pkg");

module.exports = async function run(filename, options) {
  execSync(`${process.execPath} ${path.join(__dirname, 'runner.mjs')} ${filename}`, {
    stdio: "inherit",
    env: {
      __WASI_DIR: JSON.stringify(options.dir ?? []),
      NODE_OPTIONS: [
        `--experimental-loader=${path.join(__dirname, 'hook.mjs')}`,
        "--no-warnings",
      ].join(" "),
    },
  });
};
