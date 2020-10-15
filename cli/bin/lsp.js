const path = require('path');
const { execSync } = require('child_process');

const grainc = path.join(__dirname, 'grainc.exe');

// call the compiler, passing stdio through so the compiler gets the source code on stdin
// and we get the compiler output in stdout
// we still take the file name so we have it available

module.exports = (file, options) => {
  try {
    execSync(`${grainc} --stdlib=${options.stdlib} ${options.cflags ? options.cflags : ''} -g --lsp ${file}`, { stdio: 'inherit' });
    process.exit()
  } catch (e) {
    console.log(e);
    if (options.graceful) {
      process.exit()
    } else {
      process.exit(1)
    }
  }
}
