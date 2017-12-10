import GrainRunner from './runtime';
if (window.GrainRunner) {
  throw new Error('[Grain] Only one instance of the Grain runtime is allowed!');
}
window.GrainRunner = GrainRunner;
export default GrainRunner;
