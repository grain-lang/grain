#!/usr/bin/env node

let program = require('commander');
let compile = require('./compile.js');
let run = require('./run.js');

let givenFile

function list(val) {
  return val.split(',');
}

program
  .version('Grain compiler 0.0.0\nGrain cli 0.0.0', '-v, --version')
  .description('Compile and run Grain programs. 🌾')
  .arguments('<file>')
  .option('-w, --wasm', 'run a wasm file')
  .option('-p, --print-output', 'print the output of the program')
  .option('-g, --graceful', 'return a 0 exit code if the program errors')
  .option('-I, --include-dirs <dirs>', 'include directories the runtime should find wasm modules', list, [])
  .option('-f, --cflags <cflags>', 'pass flags to the Grain compiler')
  .action((file) => {
    givenFile = file
    
    let wasmFile;
    if (program.wasm) {
      wasmFile = file;
    } else {
      wasmFile = compile(file, program);
    }

    run(wasmFile, program);
  });

program.parse(process.argv);

// If no file is given, print the help message and exit
if (typeof givenFile === 'undefined') {
  program.outputHelp()
  process.exit(-1)
}
