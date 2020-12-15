import {
    coerceFloat64,
    reducedInteger
} from './numbers';

import {
    newFloat64
} from './ascutils/dataStructures'

/** 
 * Returns the square root of a given number.
 * 
 * @param   {u32} x - The input number.
 * @returns {u32}
 */
export function mathSqrt(x: u32): u32 {
    let xval = coerceFloat64(x);
    return newFloat64(sqrt<f64>(xval));
}

/** 
 * Returns the least integer greater than the given number.
 * 
 * @param   {u32} x - The input number.
 * @returns {u32}
 */
export function mathCeil(x: u32): u32 {
    let xval = coerceFloat64(x);
    return reducedInteger(<i64>ceil(xval));
}

/** 
 * Returns the greatest integer less than the given number.
 * 
 * @param   {u32} x - The input number.
 * @returns {u32}
 */
export function mathFloor(x: u32): u32 {
    let xval = coerceFloat64(x);
    return reducedInteger(<i64>floor(xval));
}

/** 
 * Returns the integer part of a given number by removing any fractional digits.
 * 
 * @param   {u32} x - The input number.
 * @returns {u32}
 */
export function mathTrunc(x: u32): u32 {
    let xval = coerceFloat64(x);
    return reducedInteger(<i64>trunc(xval));
}

/** 
 * Returns the absolute value of the given number.
 * 
 * @param   {u32} x - The input number.
 * @returns {u32}
 */
export function mathAbs(x: u32): u32 {
    let xval = coerceFloat64(x);
    return newFloat64(<f64>abs(xval));
}

/** 
 * Returns the minimum of 2 numbers.
 * 
 * @param   {u32} x - The first input.
 * @param   {u32} y - The second input.
 * @returns {u32}
 */
export function mathMin(x: u32, y: u32): u32 {
    let xval = coerceFloat64(x);
    let yval = coerceFloat64(y);
    return newFloat64(<f64>min(xval, yval))
}