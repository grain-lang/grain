const path = require("path");
const runner = require("@grain/js-runner");
const preparePkg = require("./pkg");

module.exports = async function run(filename, options) {
  preparePkg();

  try {
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
  } catch (e) {
    console.error(e);
    if (options.graceful) {
      process.exit();
    } else {
      process.exit(-1);
    }
  }
};
