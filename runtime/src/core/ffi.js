import { grainToJSVal, JSToGrainVal } from "../utils/utils";

export const wrapJSFunc = (fn) => (...args) => {
  const result = fn(...args.map(arg => grainToJSVal(null, arg)));

  return JSToGrainVal(result);
};

export const wrapJSModule = (mod) =>
  Object.keys(mod).reduce((wrapped, key) => ({
    ...wrapped,

    [key]: typeof mod[key] === 'function'
      ? wrapJSFunc(mod[key])
      : mod[key]
  }), {});