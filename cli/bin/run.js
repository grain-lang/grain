const { execSync } = require('child_process');

let runtime = require('../../runtime/dist/grain-runtime.js');
let grainRoot = execSync('grain-root').toString('utf8').trim();
let locator = runtime.defaultFileLocator(`${grainRoot}/lib/grain/stdlib`);
let GrainRunner = runtime.buildGrainRunner(locator);

module.exports = async function run(path, printOutput) {
  let result = await GrainRunner.runFile(path);
  if (printOutput) {
    console.log(result);
  }
}
