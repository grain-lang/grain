#!/usr/bin/env node

const v8flags = require('v8flags');
const flaggedRespawn = require('flagged-respawn');

v8flags(onFlags);

function onFlags(err, flags) {
  if (err) {
    console.error('Encountered error when retrieving v8 flags: ', err);
    process.exit(1);
  }

  // https://github.com/grain-lang/grain/issues/114
  flaggedRespawn(flags, process.argv, ['--experimental-wasm-bigint'], onRespawn);
}

function onRespawn(ready, proc, argv) {
  if (!ready) {
    // Respawning
    return;
  }

  const { execSync } = require('child_process');
  const path = require('path');

  let program = require('commander');
  let compile = require('./compile.js');
  let run = require('./run.js');
  let lsp = require('./lsp.js');

  let pervasivesPath = require.resolve('@grain/stdlib');
  let stdlibPath = path.dirname(pervasivesPath);

  function list(val) {
    return val.split(',');
  }

  function num(val) {
    return Number.parseInt(val, 10);
  }

  function graincVersion() {
    const grainc = path.join(__dirname, 'grainc.exe');

    return execSync(`${grainc} --version`).toString().trim();
  }

  program
    .option('-v, --version', 'output CLI and compiler versions')
    .on('option:version', () => {
      console.log(`Grain cli ${require('../package.json').version}`)
      console.log(`Grain compiler ${graincVersion()}`)
      process.exit(0)
    })
    .description('Compile and run Grain programs. 🌾')
    .option('-p, --print-output', 'print the output of the program')
    .option('-g, --graceful', 'return a 0 exit code if the program errors')
    .option('-I, --include-dirs <dirs>', 'include directories the runtime should find wasm modules', list, [])
    .option('-f, --cflags <cflags>', 'pass flags to the Grain compiler')
    .option('-S, --stdlib <path>', 'override the standard libary with your own', stdlibPath)
    .option('--limitMemory <size>', 'maximum allowed heap size', num, -1)
    // The root command that compiles & runs
    .arguments('<file>')
    .action(function (file) {
      run(compile(file, program), program);
    })

  program
    .command('compile <file>')
    .description('compile a grain program into wasm')
    .action(function (file) {
      compile(file, program);
    });

  program
    .command('lsp <file>')
    .description('check a grain file for LSP')
    .action(function (file) {
      lsp(file, program);
    });

  program
    .command('run <file>')
    .description('run a wasm file with the grain runtime')
    .action(function (wasmFile) {
      run(wasmFile, program);
    });

  program.parse(argv);
}
