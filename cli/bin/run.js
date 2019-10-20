const path = require('path')
const { execSync } = require('child_process');

let runtime = require('../../runtime/dist/grain-runtime.js');
let grainRoot = execSync('grain-root').toString('utf8').trim();
let stdlibPath = `${grainRoot}/lib/grain/stdlib`;

module.exports = async function run(filename, options) {
  try {
    let basePath = path.dirname(filename)
    let includeDirs = [basePath, ...options.includeDirs, stdlibPath];
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
