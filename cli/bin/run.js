const path = require("path");
const runner = require("@grain/js-runner");
require("./pkg");

module.exports = async function run(filename, options) {
  let basePath = path.dirname(filename);
  let includeDirs = [basePath, ...options.includeDirs, options.stdlib];
  let locator = runner.defaultFileLocator(includeDirs);
  let GrainRunner = runner.buildGrainRunner(locator, {
    initialMemoryPages: options.initialMemoryPages,
    maximumMemoryPages: options.maximumMemoryPages,
  });
  if (options.printOutput) {
    let result = await GrainRunner.runFileUnboxed(filename);
    await GrainRunner.ensureStringModule();
    console.log(GrainRunner.grainValueToString(result));
  } else {
    await GrainRunner.runFile(filename);
  }
};
