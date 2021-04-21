const path = require("path");
const { execSync } = require("child_process");
const fs = require("fs");

function getGrainc() {
  const grainc = path.join(__dirname, "grainc.exe");

  // TODO: Maybe make an installable path & check it?
  if (process.pkg || !fs.existsSync(grainc)) {
    const node = process.execPath;
    const grainc_js = path.join(__dirname, "grainc.js");
    return `${node} ${grainc_js}`;
  }

  return `${grainc}`;
}

const grainc = getGrainc();

function exec(commandOrFile = "", program, execOpts = { stdio: "pipe" }) {
  const flags = [];
  program.options.forEach((option) => {
    if (!option.grainc) return;
    const flag = option.toFlag();
    if (flag) flags.push(flag);
  });

  return execSync(`${grainc} ${flags.join(" ")} ${commandOrFile}`, execOpts);
}

module.exports = exec;
