const path = require('path')
const runtime = require('@grain/runtime/dist/grain-runtime.js')

module.exports = async function run(filename, options) {
  try {
    let basePath = path.dirname(filename)
    let includeDirs = [basePath, ...options.includeDirs, options.stdlib];
    let locator = runtime.defaultFileLocator(includeDirs);
    let GrainRunner = runtime.buildGrainRunner(locator, {limitMemory: options.limitMemory});
    if (options.printOutput) {
      let result = await GrainRunner.runFileUnboxed(filename, true);
      console.log(runtime.grainToString(GrainRunner, result));
    } else {
      await GrainRunner.runFile(filename);
    }
    // Will only do anything if memory.js's TRACE_MEMORY is enabled
    runtime.dumpMemoryStats();
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
