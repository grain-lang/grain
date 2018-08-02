let runtime = require('../../runtime/dist/grain-runtime.js');
let locatorPath = require('./locator-path');
let locator = runtime.defaultFileLocator(locatorPath);
let GrainRunner = runtime.buildGrainRunner(locator);

module.exports = async function run(path) {
    let res = await GrainRunner.runFile(path);
    //console.log(`result: ${res}`);
}
