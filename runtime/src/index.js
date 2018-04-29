import {buildGrainRunner, defaultURLLocator} from './runtime';
if (window.GrainRunner) {
  throw new Error('[Grain] Only one instance of the Grain runtime is allowed!');
}
let GrainRunner = buildGrainRunner(defaultURLLocator(window.GrainStdlibRoot));
window.GrainRunner = GrainRunner;
export default GrainRunner;
