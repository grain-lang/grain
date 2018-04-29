const { execSync } = require('child_process');

module.exports = (file) => {
  try {
    execSync(`grainc -I _build/install/default/lib/grain/stdlib ${file}`);
    return file.replace(/\.gr$/, '.wasm')
  } catch (e) {
    console.log(e.stdout.toString());
    process.exit(-1);
  }
}
