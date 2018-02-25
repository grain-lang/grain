let GrainRunner = require('../../runtime/dist/grain-runtime.js').GrainNodeRunner;

module.exports = async function run(path) {
    await GrainRunner(path);
}