const path = require('path')
const runtime = require('@grain/runtime/dist/grain-runtime.js')

module.exports = async function run(filename, options) {
  try {
    let basePath = path.dirname(filename)
    let includeDirs = [basePath, ...options.includeDirs, options.stdlib];
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
