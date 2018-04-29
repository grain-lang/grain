#!node --harmony

let program = require('commander');
let compile = require('./compile.js');
let run = require('./run.js');

let file;

program
  .version('Grain compiler 0.0.0\nGrain cli 0.0.0', '-v, --version')
  .arguments('<file>')
  .action((givenFile) => {
    file = givenFile || '';
  });

program.on('--help', () => {
  console.log('\n\n  File can be either a Grain (.gr) or WebAssembly (.wasm) file.\n');
});
  
program.parse(process.argv);

let wasmFile;
if (file.endsWith('gr')) {
  wasmFile = compile(file);
} else if (file.endsWith('wasm')) {
  wasmFile = file;
} else {
  program.help();
}

run(wasmFile);