import { view, grainModule } from '../runtime';
import { throwGrainError } from '../errors/errors';
import { GRAIN_ERR_ARITY_MISMATCH } from '../errors/error-codes';

export const GrainClosure = function(loc) {
  this.loc = loc;
  this.arity = view[loc];
  this.ptr = view[loc + 1];
  this.closureSize = view[loc + 2];
  this.closureElts = view.slice(loc + 3, loc + 3 + this.closureSize);
  this.func = grainModule.instance.exports["GRAIN$LAM_" + this.ptr];
};

GrainClosure.prototype.call = function() {
  if (arguments.length != this.arity) {
    throwGrainError(GRAIN_ERR_ARITY_MISMATCH, this.arity, arguments.length);
    return undefined;
  } else {
    let grainVals = Array.prototype.map.call(arguments, JSToGrainVal);
    grainVals.unshift(this.loc * 4);
    return grainToJSVal(this.func.apply(this.func, grainVals));
  }
};

export function printClosure(c) {
  c /= 4;
  let arity = view[c];
  let idx = view[c + 1];
  let closureSize = view[c + 2];
  let closureElts = [];

  for (var i = 0; i < closureSize; ++i) {
    closureElts.push(printNumber(view[c + i + 3]));
  }
  console.log(`<closure@${c}: idx=${idx}, arity=${arity}, size=${closureSize}: ${closureElts}>`);
  console.log(view.slice(0, 32));
  return c;
}
