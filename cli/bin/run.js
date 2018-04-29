let runtime = require('../../runtime/dist/grain-runtime.js');
let locator = runtime.defaultFileLocator('_build/install/default/lib/grain/stdlib');
let GrainRunner = runtime.buildGrainRunner(locator);

module.exports = async function run(path) {
    let res = await GrainRunner.runFile(path);
    //console.log(`result: ${res}`);
}
