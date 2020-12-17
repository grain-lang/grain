import { coerceFloat64, reducedInteger } from './numbers';
import { newFloat64 } from './ascutils/dataStructures';
import { throwError } from './ascutils/grainRuntime';
import { GRAIN_ERR_NOT_NONNEG } from './ascutils/errors';

/**
 * Returns the square root of a given number.
 * 
 * @param   {u32} x - The input number.
 * @returns {u32} The square root of a given number.
 */
export function mathSqrt(x: u32): u32 {
    let xval = coerceFloat64(x);
    if (xval < 0) {
        throwError(GRAIN_ERR_NOT_NONNEG, newFloat64(xval), 0);
    }
    return newFloat64(sqrt<f64>(xval));
}

/**
 * Returns the least integer greater than the given number.
 * 
 * @param   {u32} x - The input number.
 * @returns {u32} The least integer greater than the given number.
 */
export function mathCeil(x: u32): u32 {
    let xval = coerceFloat64(x);
    return reducedInteger(<i64>ceil(xval));
}

/**
 * Returns the greatest integer less than the given number.
 * 
 * @param   {u32} x - The input number.
 * @returns {u32} The greatest integer less than the given number.
 */
export function mathFloor(x: u32): u32 {
    let xval = coerceFloat64(x);
    return reducedInteger(<i64>floor(xval));
}

/**
 * Returns the integer part of a given number by removing any fractional digits.
 * 
 * @param   {u32} x - The input number.
 * @returns {u32} The integer part of a given number by removing any fractional digits.
 */
export function mathTrunc(x: u32): u32 {
    let xval = coerceFloat64(x);
    return reducedInteger(<i64>trunc(xval));
}

/**
 * Returns the absolute value of the given number.
 * 
 * @param   {u32} x - The input number.
 * @returns {u32} The absolute value of the given number.
 */
export function mathAbs(x: u32): u32 {
    let xval = coerceFloat64(x);
    return newFloat64(<f64>abs(xval));
}

/**
 * Returns the minimum of 2 numbers.
 * 
 * @param   {u32} x - The first input number.
 * @param   {u32} y - The second input number.
 * @returns {u32} The minimum of 2 numbers
 */
export function mathMin(x: u32, y: u32): u32 {
    let xval = coerceFloat64(x);
    let yval = coerceFloat64(y);
    return newFloat64(<f64>min(xval, yval))
}

/**
 * Returns the maximum of 2 numbers.
 * 
 * @param   {u32} x - The first input number.
 * @param   {u32} y - The second input number.
 * @returns {u32} The maximum of 2 numbers 
 */
export function mathMax(x: u32, y: u32): u32 {
    let xval = coerceFloat64(x);
    let yval = coerceFloat64(y);
    return newFloat64(<f64>min(xval, yval))
}

/**
 * Returns the negative of the given number.
 * 
 * @param   {u32} x - The input number.
 * @returns {u32} The negative of the given number. 
 */
export function mathNeg(x: u32): u32 {
    let xval = coerceFloat64(x);
    return newFloat64(-xval)
}

/**
 * Returns the number after rounding it to the nearest decimal.
 * 
 * @param   {u32} x - The input number.
 * @returns {u32} The number after rounding it to the nearest decimal. 
 */
export function mathRound(x: u32): u32 {
    let xval = coerceFloat64(x);
    return newFloat64(<f64>Math.round(xval));
}