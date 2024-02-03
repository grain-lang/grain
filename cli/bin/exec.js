const path = require("path");
const { execSync } = require("child_process");
const fs = require("fs");

function exec(command, execOpts) {
  try {
    execSync(command, execOpts);
    return true;
  } catch (err) {
    process.exitCode = err.status;
    return false;
  }
}

function flagsFromOptions(program, options) {
  const flags = [];
  program.options.forEach((option) => {
    if (!option.forward) return;
    const flag = option.toFlag(options);
    if (flag) flags.push(flag);
  });
  return flags;
}

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

function execGrainc(
  commandOrFile = "",
  options,
  program,
  execOpts = { stdio: "inherit" }
) {
  const flags = flagsFromOptions(program, options);

  return exec(`${grainc} ${flags.join(" ")} ${commandOrFile}`, execOpts);
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
  options,
  program,
  execOpts = { stdio: "inherit" }
) {
  const flags = flagsFromOptions(program, options);

  return exec(`${graindoc} ${flags.join(" ")} ${commandOrFile}`, execOpts);
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
  options,
  program,
  execOpts = { stdio: "inherit" }
) {
  const flags = flagsFromOptions(program, options);

  return exec(`${grainformat} ${flags.join(" ")} ${commandOrFile}`, execOpts);
}

function getGrainlsp() {
  const grainlsp = path.join(__dirname, "grainlsp.exe");

  // TODO: Maybe make an installable path & check it?
  if (process.pkg || !fs.existsSync(grainlsp)) {
    const node = process.execPath;
    const grainlsp_js = path.join(__dirname, "grainlsp.js");
    return `"${node}" ${grainlsp_js}`;
  }

  return `${grainlsp}`;
}

const grainlsp = getGrainlsp();

function execGrainlsp(options, program, execOpts = { stdio: "inherit" }) {
  const flags = flagsFromOptions(program, options);

  return exec(`${grainlsp} ${flags.join(" ")}`, execOpts);
}

function getGrainrun() {
  const node = process.execPath;
  const grainlsp_js = path.join(__dirname, "grainrun.js");
  return `"${node}" ${grainlsp_js}`;
}

const grainrun = getGrainrun();

function execGrainrun(
  unprocessedArgs,
  file,
  options,
  program,
  execOpts = { stdio: "inherit" }
) {
  const preopens = {};
  options.dir?.forEach((preopen) => {
    const [guestDir, hostDir = guestDir] = preopen.split("=");
    preopens[guestDir] = hostDir;
  });

  const cliEnv = {};
  options.env?.forEach((env) => {
    const [name, ...rest] = env.split("=");
    const val = rest.join("=");
    cliEnv[name] = val;
  });

  const env = {
    ENV_VARS: JSON.stringify(cliEnv),
    PREOPENS: JSON.stringify(preopens),
    NODE_OPTIONS: `--experimental-wasi-unstable-preview1 --no-warnings`,
  };

  try {
    exec(`${grainrun} ${file} ${unprocessedArgs.join(" ")}`, {
      ...execOpts,
      env,
    });
  } catch (e) {
    process.exit(e.status);
  }
}

module.exports = {
  grainc: execGrainc,
  graindoc: execGraindoc,
  grainformat: execGrainformat,
  grainlsp: execGrainlsp,
  grainrun: execGrainrun,
};
