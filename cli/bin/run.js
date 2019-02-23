const { execSync } = require('child_process');

let runtime = require('../../runtime/dist/grain-runtime.js');
let grainRoot = execSync('grain-root').toString('utf8').trim();

module.exports = async function run(path, options) {
  try {
    let locator = runtime.defaultFileLocator(options.includeDirs.concat(`${grainRoot}/lib/grain/stdlib`));
    let GrainRunner = runtime.buildGrainRunner(locator);
    let result = await GrainRunner.runFile(path);
    if (options.printOutput) {
      console.log(result);
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
