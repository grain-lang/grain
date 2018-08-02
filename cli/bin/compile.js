const { execSync } = require('child_process');
const locatorPath = require('./locator-path');

module.exports = (file) => {
  try {
    execSync(`grainc -I ${locatorPath} ${file}`);
    return file.replace(/\.gr$/, '.wasm')
  } catch (e) {
    console.log(e.stdout.toString());
    process.exit(-1);
  }
}
