import { grainToJSVal, JSToGrainVal } from "../utils/utils";

export class GrainClosure {
  constructor(loc, runner) {
    const view = runner.managedMemory.view;

    this.loc = loc;
    this.runner = runner;
    this.arity = view[loc + 1];
    this.ptr = view[loc + 2];
    this.closureSize = view[loc + 3];
    this.closureElts = view.slice(loc + 4, loc + 4 + this.closureSize);
    this.func = runner.table.get(this.ptr);
  }

  jsFunc(...args) {
    let grainVals = args.map((x) => JSToGrainVal(x, this.runner));
    grainVals.unshift(this.loc * 4);
    return grainToJSVal(this.runner, this.func(...grainVals));
  }
}

export function printClosure(c) {
  const view = runner.managedMemory.view;

  c /= 4;
  let arity = view[c];
  let idx = view[c + 1];
  let closureSize = view[c + 2];
  let closureElts = [];

  // [TODO]: Reenable when this gets ported to AS
  // for (var i = 0; i < closureSize; ++i) {
  //   closureElts.push(print(view[c + i + 3]));
  // }
  console.log(
    `<closure@${c}: idx=${idx}, arity=${arity}, size=${closureSize}>`
  );
  console.log(view.slice(0, 32));
  return c;
}
