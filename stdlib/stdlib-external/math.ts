import {
    coerceFloat64
} from './numbers';

import {
    newFloat64
} from './ascutils/dataStructures'

/** 
 * Returns the square root of a given number.
 * 
 * @param {u32} x - The input number
 */
export function mathSqrt(x: u32): u32 {
    let xval = coerceFloat64(x);
    return newFloat64(sqrt<f64>(xval));
}