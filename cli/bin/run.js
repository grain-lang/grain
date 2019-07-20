const path = require('path')
const { execSync } = require('child_process');

let runtime = require('../../runtime/dist/grain-runtime.js');
let grainRoot = execSync('grain-root').toString('utf8').trim();

module.exports = async function run(filename, options) {
  try {
    let base_path = path.dirname(filename)
    let includeDirs = [base_path].concat(options.includeDirs, `${grainRoot}/lib/grain/stdlib`)
    let locator = runtime.defaultFileLocator(includeDirs);
    let GrainRunner = runtime.buildGrainRunner(locator);
    let result = await GrainRunner.runFileUnboxed(filename);
    if (options.printOutput) {
      console.log(runtime.grainToString(GrainRunner, result));
    }
  }
  catch (e) {
    console.error(e)
    if (options.graceful) {
      process.exit()
    } else {
      process.exit(-1)
    }
  }
}
