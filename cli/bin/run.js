const { execSync } = require('child_process');

let runtime = require('../../runtime/dist/grain-runtime.js');
let grainRoot = execSync('grain-root').toString('utf8').trim();
let locator = runtime.defaultFileLocator(`${grainRoot}/lib/grain/stdlib`);
let GrainRunner = runtime.buildGrainRunner(locator);

module.exports = async function run(path, options) {
  try {
    let result = await GrainRunner.runFileUnboxed(path);
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
