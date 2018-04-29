import {buildGrainRunner, defaultURLLocator} from './runtime';

let GrainRunner = buildGrainRunner(defaultURLLocator(window.GrainStdlibRoot));
export default GrainRunner;
