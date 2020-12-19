import { managedMemory, grainModule } from '../runtime';
import { throwGrainError } from '../errors/errors';
import { grainToJSVal, JSToGrainVal } from '../utils/utils';
import { GRAIN_ERR_ARITY_MISMATCH } from '../errors/error-codes';

export class GrainClosure {
  constructor(loc, runtime) {
    const view = managedMemory.view;

    this.loc = loc;
    this.runtime = runtime;
    this.arity = view[loc];
    this.ptr = view[loc + 1];
    this.closureSize = view[loc + 2];
    this.closureElts = view.slice(loc + 3, loc + 3 + this.closureSize);
    this.func = grainModule.instance.exports["GRAIN$LAM_" + this.ptr];
  }

  jsFunc(...args) {
    if (args.length != this.arity) {
      throwGrainError(GRAIN_ERR_ARITY_MISMATCH, this.arity, args.length, this.runtime);
    } else {
      let grainVals = args.map(x => JSToGrainVal(x, this.runtime));
      grainVals.unshift(this.loc * 4);
      // [TODO]: We should feed in a runtime here.
      return grainToJSVal(null, this.func(...grainVals));
    }
  }
};

export function printClosure(c) {
  const view = managedMemory.view;

  c /= 4;
  let arity = view[c];
  let idx = view[c + 1];
  let closureSize = view[c + 2];
  let closureElts = [];

  // [TODO]: Reenable when this gets ported to AS
  // for (var i = 0; i < closureSize; ++i) {
  //   closureElts.push(print(view[c + i + 3]));
  // }
  console.log(`<closure@${c}: idx=${idx}, arity=${arity}, size=${closureSize}>`);
  console.log(view.slice(0, 32));
  return c;
}
