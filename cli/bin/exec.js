const path = require("path");
const { execSync } = require("child_process");
const fs = require("fs");

function getGrainc() {
  const grainc = path.join(__dirname, "grainc.exe");

  // TODO: Maybe make an installable path & check it?
  if (process.pkg || !fs.existsSync(grainc)) {
    const node = process.execPath;
    const grainc_js = path.join(__dirname, "grainc.js");
    return `"${node}" ${grainc_js}`;
  }

  return `${grainc}`;
}

const grainc = getGrainc();

function execGrainc(commandOrFile = "", program, execOpts = { stdio: "pipe" }) {
  const flags = [];
  const options = program.opts();
  program.options.forEach((option) => {
    if (!option.forward) return;
    const flag = option.toFlag(options);
    if (flag) flags.push(flag);
  });

  return execSync(`${grainc} ${flags.join(" ")} ${commandOrFile}`, execOpts);
}

function getGraindoc() {
  const graindoc = path.join(__dirname, "graindoc.exe");

  // TODO: Maybe make an installable path & check it?
  if (process.pkg || !fs.existsSync(graindoc)) {
    const node = process.execPath;
    const graindoc_js = path.join(__dirname, "graindoc.js");
    return `"${node}" ${graindoc_js}`;
  }

  return `${graindoc}`;
}

const graindoc = getGraindoc();

function execGraindoc(
  commandOrFile = "",
  program,
  execOpts = { stdio: "pipe" }
) {
  const flags = [];
  // Inherit compiler flags passed to the parent
  const options = program.parent.options.concat(program.options);
  const opts = { ...program.parent.opts(), ...program.opts() };
  options.forEach((option) => {
    if (!option.forward) return;
    const flag = option.toFlag(opts);
    if (flag) flags.push(flag);
  });

  return execSync(`${graindoc} ${flags.join(" ")} ${commandOrFile}`, execOpts);
}

function getGrainformat() {
  const grainformat = path.join(__dirname, "grainformat.exe");

  // TODO: Maybe make an installable path & check it?
  if (process.pkg || !fs.existsSync(grainformat)) {
    const node = process.execPath;
    const grainformat_js = path.join(__dirname, "grainformat.js");
    return `"${node}" ${grainformat_js}`;
  }

  return `${grainformat}`;
}

const grainformat = getGrainformat();

function execGrainformat(
  commandOrFile = "",
  program,
  execOpts = { stdio: "pipe" }
) {
  const flags = [];
  // Inherit compiler flags passed to the parent
  const options = program.parent.options.concat(program.options);
  const opts = { ...program.parent.opts(), ...program.opts() };
  options.forEach((option) => {
    if (!option.forward) return;
    const flag = option.toFlag(opts);
    if (flag) flags.push(flag);
  });

  return execSync(
    `${grainformat} ${flags.join(" ")} ${commandOrFile}`,
    execOpts
  );
}

module.exports = {
  grainc: execGrainc,
  graindoc: execGraindoc,
  grainformat: execGrainformat,
};
