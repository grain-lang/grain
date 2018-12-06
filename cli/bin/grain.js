#!/usr/bin/env node

let program = require('commander');
let compile = require('./compile.js');
let run = require('./run.js');

let givenFile

program
  .version('Grain compiler 0.0.0\nGrain cli 0.0.0', '-v, --version')
  .description('Compile and run Grain programs. 🌾')
  .arguments('<file>')
  .option('-w, --wasm', 'run a wasm file')
  .option('-p, --print-output', 'print the output of the program')
  .option('-f, --cflags <cflags>', 'pass flags to the Grain compiler')
  .action((file) => {
    givenFile = file
    
    let wasmFile;
    if (program.wasm) {
      wasmFile = file;
    } else {
      wasmFile = compile(file);
    }

    run(wasmFile, program.printOutput);
  });

program.parse(process.argv);

if (typeof givenFile === 'undefined') {
  program.outputHelp()
  process.exit(-1)
}
