import { view, grainModule } from '../runtime';
import { throwGrainError } from '../errors/errors';
import { grainToJSVal, JSToGrainVal } from '../utils/utils';
import { GRAIN_ERR_ARITY_MISMATCH } from '../errors/error-codes';

import print from '../lib/print';

export class GrainClosure {
  constructor(loc) {
    this.loc = loc;
    this.arity = view[loc];
    this.ptr = view[loc + 1];
    this.closureSize = view[loc + 2];
    this.closureElts = view.slice(loc + 3, loc + 3 + this.closureSize);
    this.func = grainModule.instance.exports["GRAIN$LAM_" + this.ptr];
  }

  jsFunc(...args) {
    if (args.length != this.arity) {
      throwGrainError(GRAIN_ERR_ARITY_MISMATCH, this.arity, args.length);
    } else {
      let grainVals = args.map(JSToGrainVal);
      grainVals.unshift(this.loc * 4);
      return grainToJSVal(this.func(...grainVals));
    }
  }
};

export function printClosure(c) {
  c /= 4;
  let arity = view[c];
  let idx = view[c + 1];
  let closureSize = view[c + 2];
  let closureElts = [];

  for (var i = 0; i < closureSize; ++i) {
    closureElts.push(print(view[c + i + 3]));
  }
  console.log(`<closure@${c}: idx=${idx}, arity=${arity}, size=${closureSize}: ${closureElts}>`);
  console.log(view.slice(0, 32));
  return c;
}
