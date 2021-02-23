import { malloc } from './grainRuntime'
import { allocateString, singleByteString } from './dataStructures'
import { CharCode } from './charCodes'

/*
 * This file is a modified version of AssemblyScript's std/assembly/util/number.ts
 * Original file under Apache 2.0 License by AssemblyScript authors:
 *
 * https://github.com/AssemblyScript/assemblyscript/blob/d7ad4821a974d2491a0115cb35c85c649b34e7f0/LICENSE
 */

// @ts-ignore: decorator
//@inline
export const MAX_DOUBLE_LENGTH = 28;

// @ts-ignore: decorator
let _POWERS10: usize = -1

function get_POWERS10(): usize {
  if (_POWERS10 == -1) {
    _POWERS10 = <usize>(malloc(10 * 4))
    store<u32>(_POWERS10, 1, 0 * 4)
    store<u32>(_POWERS10, 10, 1 * 4)
    store<u32>(_POWERS10, 100, 2 * 4)
    store<u32>(_POWERS10, 1000, 3 * 4)
    store<u32>(_POWERS10, 10000, 4 * 4)
    store<u32>(_POWERS10, 100000, 5 * 4)
    store<u32>(_POWERS10, 1000000, 6 * 4)
    store<u32>(_POWERS10, 10000000, 7 * 4)
    store<u32>(_POWERS10, 100000000, 8 * 4)
    store<u32>(_POWERS10, 1000000000, 9 * 4)
  }
  return _POWERS10
}

/*
  Lookup table for pairwise char codes in range [0-99]
  "00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
  "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
  "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
  "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
  "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
  "50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
  "60", "61", "62", "63", "64", "65", "66", "67", "68", "69",
  "70", "71", "72", "73", "74", "75", "76", "77", "78", "79",
  "80", "81", "82", "83", "84", "85", "86", "87", "88", "89",
  "90", "91", "92", "93", "94", "95", "96", "97", "98", "99"
*/
// @ts-ignore: decorator
let _DIGITS: usize = -1

function get_DIGITS(): usize {
  if (_DIGITS == -1) {
    _DIGITS = <usize>(malloc(100 * 2))
    store<u16>(_DIGITS, 0x3030, 0 * 2)
    store<u16>(_DIGITS, 0x3130, 1 * 2)
    store<u16>(_DIGITS, 0x3230, 2 * 2)
    store<u16>(_DIGITS, 0x3330, 3 * 2)
    store<u16>(_DIGITS, 0x3430, 4 * 2)
    store<u16>(_DIGITS, 0x3530, 5 * 2)
    store<u16>(_DIGITS, 0x3630, 6 * 2)
    store<u16>(_DIGITS, 0x3730, 7 * 2)
    store<u16>(_DIGITS, 0x3830, 8 * 2)
    store<u16>(_DIGITS, 0x3930, 9 * 2)
    store<u16>(_DIGITS, 0x3031, 10 * 2)
    store<u16>(_DIGITS, 0x3131, 11 * 2)
    store<u16>(_DIGITS, 0x3231, 12 * 2)
    store<u16>(_DIGITS, 0x3331, 13 * 2)
    store<u16>(_DIGITS, 0x3431, 14 * 2)
    store<u16>(_DIGITS, 0x3531, 15 * 2)
    store<u16>(_DIGITS, 0x3631, 16 * 2)
    store<u16>(_DIGITS, 0x3731, 17 * 2)
    store<u16>(_DIGITS, 0x3831, 18 * 2)
    store<u16>(_DIGITS, 0x3931, 19 * 2)
    store<u16>(_DIGITS, 0x3032, 20 * 2)
    store<u16>(_DIGITS, 0x3132, 21 * 2)
    store<u16>(_DIGITS, 0x3232, 22 * 2)
    store<u16>(_DIGITS, 0x3332, 23 * 2)
    store<u16>(_DIGITS, 0x3432, 24 * 2)
    store<u16>(_DIGITS, 0x3532, 25 * 2)
    store<u16>(_DIGITS, 0x3632, 26 * 2)
    store<u16>(_DIGITS, 0x3732, 27 * 2)
    store<u16>(_DIGITS, 0x3832, 28 * 2)
    store<u16>(_DIGITS, 0x3932, 29 * 2)
    store<u16>(_DIGITS, 0x3033, 30 * 2)
    store<u16>(_DIGITS, 0x3133, 31 * 2)
    store<u16>(_DIGITS, 0x3233, 32 * 2)
    store<u16>(_DIGITS, 0x3333, 33 * 2)
    store<u16>(_DIGITS, 0x3433, 34 * 2)
    store<u16>(_DIGITS, 0x3533, 35 * 2)
    store<u16>(_DIGITS, 0x3633, 36 * 2)
    store<u16>(_DIGITS, 0x3733, 37 * 2)
    store<u16>(_DIGITS, 0x3833, 38 * 2)
    store<u16>(_DIGITS, 0x3933, 39 * 2)
    store<u16>(_DIGITS, 0x3034, 40 * 2)
    store<u16>(_DIGITS, 0x3134, 41 * 2)
    store<u16>(_DIGITS, 0x3234, 42 * 2)
    store<u16>(_DIGITS, 0x3334, 43 * 2)
    store<u16>(_DIGITS, 0x3434, 44 * 2)
    store<u16>(_DIGITS, 0x3534, 45 * 2)
    store<u16>(_DIGITS, 0x3634, 46 * 2)
    store<u16>(_DIGITS, 0x3734, 47 * 2)
    store<u16>(_DIGITS, 0x3834, 48 * 2)
    store<u16>(_DIGITS, 0x3934, 49 * 2)
    store<u16>(_DIGITS, 0x3035, 50 * 2)
    store<u16>(_DIGITS, 0x3135, 51 * 2)
    store<u16>(_DIGITS, 0x3235, 52 * 2)
    store<u16>(_DIGITS, 0x3335, 53 * 2)
    store<u16>(_DIGITS, 0x3435, 54 * 2)
    store<u16>(_DIGITS, 0x3535, 55 * 2)
    store<u16>(_DIGITS, 0x3635, 56 * 2)
    store<u16>(_DIGITS, 0x3735, 57 * 2)
    store<u16>(_DIGITS, 0x3835, 58 * 2)
    store<u16>(_DIGITS, 0x3935, 59 * 2)
    store<u16>(_DIGITS, 0x3036, 60 * 2)
    store<u16>(_DIGITS, 0x3136, 61 * 2)
    store<u16>(_DIGITS, 0x3236, 62 * 2)
    store<u16>(_DIGITS, 0x3336, 63 * 2)
    store<u16>(_DIGITS, 0x3436, 64 * 2)
    store<u16>(_DIGITS, 0x3536, 65 * 2)
    store<u16>(_DIGITS, 0x3636, 66 * 2)
    store<u16>(_DIGITS, 0x3736, 67 * 2)
    store<u16>(_DIGITS, 0x3836, 68 * 2)
    store<u16>(_DIGITS, 0x3936, 69 * 2)
    store<u16>(_DIGITS, 0x3037, 70 * 2)
    store<u16>(_DIGITS, 0x3137, 71 * 2)
    store<u16>(_DIGITS, 0x3237, 72 * 2)
    store<u16>(_DIGITS, 0x3337, 73 * 2)
    store<u16>(_DIGITS, 0x3437, 74 * 2)
    store<u16>(_DIGITS, 0x3537, 75 * 2)
    store<u16>(_DIGITS, 0x3637, 76 * 2)
    store<u16>(_DIGITS, 0x3737, 77 * 2)
    store<u16>(_DIGITS, 0x3837, 78 * 2)
    store<u16>(_DIGITS, 0x3937, 79 * 2)
    store<u16>(_DIGITS, 0x3038, 80 * 2)
    store<u16>(_DIGITS, 0x3138, 81 * 2)
    store<u16>(_DIGITS, 0x3238, 82 * 2)
    store<u16>(_DIGITS, 0x3338, 83 * 2)
    store<u16>(_DIGITS, 0x3438, 84 * 2)
    store<u16>(_DIGITS, 0x3538, 85 * 2)
    store<u16>(_DIGITS, 0x3638, 86 * 2)
    store<u16>(_DIGITS, 0x3738, 87 * 2)
    store<u16>(_DIGITS, 0x3838, 88 * 2)
    store<u16>(_DIGITS, 0x3938, 89 * 2)
    store<u16>(_DIGITS, 0x3039, 90 * 2)
    store<u16>(_DIGITS, 0x3139, 91 * 2)
    store<u16>(_DIGITS, 0x3239, 92 * 2)
    store<u16>(_DIGITS, 0x3339, 93 * 2)
    store<u16>(_DIGITS, 0x3439, 94 * 2)
    store<u16>(_DIGITS, 0x3539, 95 * 2)
    store<u16>(_DIGITS, 0x3639, 96 * 2)
    store<u16>(_DIGITS, 0x3739, 97 * 2)
    store<u16>(_DIGITS, 0x3839, 98 * 2)
    store<u16>(_DIGITS, 0x3939, 99 * 2)
  }
  return _DIGITS
}

// Lookup table for pairwise char codes in range [0x00-0xFF]
// @ts-ignore: decorator
let _HEX_DIGITS: usize = -1

function get_HEX_DIGITS(): usize {
  if (_HEX_DIGITS == -1) {
    _HEX_DIGITS = <usize>(malloc(256 << 1))
    store<u16>(_HEX_DIGITS, 0x3030, 0 * 2) // 00
    store<u16>(_HEX_DIGITS, 0x3130, 1 * 2) // 01
    store<u16>(_HEX_DIGITS, 0x3230, 2 * 2)
    store<u16>(_HEX_DIGITS, 0x3330, 3 * 2)
    store<u16>(_HEX_DIGITS, 0x3430, 4 * 2)
    store<u16>(_HEX_DIGITS, 0x3530, 5 * 2)
    store<u16>(_HEX_DIGITS, 0x3630, 6 * 2)
    store<u16>(_HEX_DIGITS, 0x3730, 7 * 2)
    store<u16>(_HEX_DIGITS, 0x3830, 8 * 2)
    store<u16>(_HEX_DIGITS, 0x3930, 9 * 2) // 09
    store<u16>(_HEX_DIGITS, 0x6130, 10 * 2) // 0a
    store<u16>(_HEX_DIGITS, 0x6230, 11 * 2)
    store<u16>(_HEX_DIGITS, 0x6330, 12 * 2)
    store<u16>(_HEX_DIGITS, 0x6430, 13 * 2)
    store<u16>(_HEX_DIGITS, 0x6530, 14 * 2)
    store<u16>(_HEX_DIGITS, 0x6630, 15 * 2) // 0f

    store<u16>(_HEX_DIGITS, 0x3031, 16 * 2) // 10
    store<u16>(_HEX_DIGITS, 0x3131, 17 * 2)
    store<u16>(_HEX_DIGITS, 0x3231, 18 * 2)
    store<u16>(_HEX_DIGITS, 0x3331, 19 * 2)
    store<u16>(_HEX_DIGITS, 0x3431, 20 * 2)
    store<u16>(_HEX_DIGITS, 0x3531, 21 * 2)
    store<u16>(_HEX_DIGITS, 0x3631, 22 * 2)
    store<u16>(_HEX_DIGITS, 0x3731, 23 * 2)
    store<u16>(_HEX_DIGITS, 0x3831, 24 * 2)
    store<u16>(_HEX_DIGITS, 0x3931, 25 * 2)
    store<u16>(_HEX_DIGITS, 0x6131, 26 * 2)
    store<u16>(_HEX_DIGITS, 0x6231, 27 * 2)
    store<u16>(_HEX_DIGITS, 0x6331, 28 * 2)
    store<u16>(_HEX_DIGITS, 0x6431, 29 * 2)
    store<u16>(_HEX_DIGITS, 0x6531, 30 * 2)
    store<u16>(_HEX_DIGITS, 0x6631, 31 * 2) // 1f

    store<u16>(_HEX_DIGITS, 0x3032, 32 * 2) // 20
    store<u16>(_HEX_DIGITS, 0x3132, 33 * 2)
    store<u16>(_HEX_DIGITS, 0x3232, 34 * 2)
    store<u16>(_HEX_DIGITS, 0x3332, 35 * 2)
    store<u16>(_HEX_DIGITS, 0x3432, 36 * 2)
    store<u16>(_HEX_DIGITS, 0x3532, 37 * 2)
    store<u16>(_HEX_DIGITS, 0x3632, 38 * 2)
    store<u16>(_HEX_DIGITS, 0x3732, 39 * 2)
    store<u16>(_HEX_DIGITS, 0x3832, 40 * 2)
    store<u16>(_HEX_DIGITS, 0x3932, 41 * 2)
    store<u16>(_HEX_DIGITS, 0x6132, 42 * 2)
    store<u16>(_HEX_DIGITS, 0x6232, 43 * 2)
    store<u16>(_HEX_DIGITS, 0x6332, 44 * 2)
    store<u16>(_HEX_DIGITS, 0x6432, 45 * 2)
    store<u16>(_HEX_DIGITS, 0x6532, 46 * 2)
    store<u16>(_HEX_DIGITS, 0x6632, 47 * 2) // 2f

    store<u16>(_HEX_DIGITS, 0x3033, 48 * 2) // 30
    store<u16>(_HEX_DIGITS, 0x3133, 49 * 2)
    store<u16>(_HEX_DIGITS, 0x3233, 50 * 2)
    store<u16>(_HEX_DIGITS, 0x3333, 51 * 2)
    store<u16>(_HEX_DIGITS, 0x3433, 52 * 2)
    store<u16>(_HEX_DIGITS, 0x3533, 53 * 2)
    store<u16>(_HEX_DIGITS, 0x3633, 54 * 2)
    store<u16>(_HEX_DIGITS, 0x3733, 55 * 2)
    store<u16>(_HEX_DIGITS, 0x3833, 56 * 2)
    store<u16>(_HEX_DIGITS, 0x3933, 57 * 2)
    store<u16>(_HEX_DIGITS, 0x6133, 58 * 2)
    store<u16>(_HEX_DIGITS, 0x6233, 59 * 2)
    store<u16>(_HEX_DIGITS, 0x6333, 60 * 2)
    store<u16>(_HEX_DIGITS, 0x6433, 61 * 2)
    store<u16>(_HEX_DIGITS, 0x6533, 62 * 2)
    store<u16>(_HEX_DIGITS, 0x6633, 63 * 2) // 3f

    store<u16>(_HEX_DIGITS, 0x3034, 64 * 2) // 40
    store<u16>(_HEX_DIGITS, 0x3134, 65 * 2)
    store<u16>(_HEX_DIGITS, 0x3234, 66 * 2)
    store<u16>(_HEX_DIGITS, 0x3334, 67 * 2)
    store<u16>(_HEX_DIGITS, 0x3434, 68 * 2)
    store<u16>(_HEX_DIGITS, 0x3534, 69 * 2)
    store<u16>(_HEX_DIGITS, 0x3634, 70 * 2)
    store<u16>(_HEX_DIGITS, 0x3734, 71 * 2)
    store<u16>(_HEX_DIGITS, 0x3834, 72 * 2)
    store<u16>(_HEX_DIGITS, 0x3934, 73 * 2)
    store<u16>(_HEX_DIGITS, 0x6134, 74 * 2)
    store<u16>(_HEX_DIGITS, 0x6234, 75 * 2)
    store<u16>(_HEX_DIGITS, 0x6334, 76 * 2)
    store<u16>(_HEX_DIGITS, 0x6434, 77 * 2)
    store<u16>(_HEX_DIGITS, 0x6534, 78 * 2)
    store<u16>(_HEX_DIGITS, 0x6634, 79 * 2) // 4f

    store<u16>(_HEX_DIGITS, 0x3035, 80 * 2) // 50
    store<u16>(_HEX_DIGITS, 0x3135, 81 * 2)
    store<u16>(_HEX_DIGITS, 0x3235, 82 * 2)
    store<u16>(_HEX_DIGITS, 0x3335, 83 * 2)
    store<u16>(_HEX_DIGITS, 0x3435, 84 * 2)
    store<u16>(_HEX_DIGITS, 0x3535, 85 * 2)
    store<u16>(_HEX_DIGITS, 0x3635, 86 * 2)
    store<u16>(_HEX_DIGITS, 0x3735, 87 * 2)
    store<u16>(_HEX_DIGITS, 0x3835, 88 * 2)
    store<u16>(_HEX_DIGITS, 0x3935, 89 * 2)
    store<u16>(_HEX_DIGITS, 0x6135, 90 * 2)
    store<u16>(_HEX_DIGITS, 0x6235, 91 * 2)
    store<u16>(_HEX_DIGITS, 0x6335, 92 * 2)
    store<u16>(_HEX_DIGITS, 0x6435, 93 * 2)
    store<u16>(_HEX_DIGITS, 0x6535, 94 * 2)
    store<u16>(_HEX_DIGITS, 0x6635, 95 * 2) // 5f

    store<u16>(_HEX_DIGITS, 0x3036, 96 * 2) // 60
    store<u16>(_HEX_DIGITS, 0x3136, 97 * 2)
    store<u16>(_HEX_DIGITS, 0x3236, 98 * 2)
    store<u16>(_HEX_DIGITS, 0x3336, 99 * 2)
    store<u16>(_HEX_DIGITS, 0x3436, 100 * 2)
    store<u16>(_HEX_DIGITS, 0x3536, 101 * 2)
    store<u16>(_HEX_DIGITS, 0x3636, 102 * 2)
    store<u16>(_HEX_DIGITS, 0x3736, 103 * 2)
    store<u16>(_HEX_DIGITS, 0x3836, 104 * 2)
    store<u16>(_HEX_DIGITS, 0x3936, 105 * 2)
    store<u16>(_HEX_DIGITS, 0x6136, 106 * 2)
    store<u16>(_HEX_DIGITS, 0x6236, 107 * 2)
    store<u16>(_HEX_DIGITS, 0x6336, 108 * 2)
    store<u16>(_HEX_DIGITS, 0x6436, 109 * 2)
    store<u16>(_HEX_DIGITS, 0x6536, 110 * 2)
    store<u16>(_HEX_DIGITS, 0x6636, 111 * 2) // 6f

    store<u16>(_HEX_DIGITS, 0x3037, 112 * 2) // 70
    store<u16>(_HEX_DIGITS, 0x3137, 113 * 2)
    store<u16>(_HEX_DIGITS, 0x3237, 114 * 2)
    store<u16>(_HEX_DIGITS, 0x3337, 115 * 2)
    store<u16>(_HEX_DIGITS, 0x3437, 116 * 2)
    store<u16>(_HEX_DIGITS, 0x3537, 117 * 2)
    store<u16>(_HEX_DIGITS, 0x3637, 118 * 2)
    store<u16>(_HEX_DIGITS, 0x3737, 119 * 2)
    store<u16>(_HEX_DIGITS, 0x3837, 120 * 2)
    store<u16>(_HEX_DIGITS, 0x3937, 121 * 2)
    store<u16>(_HEX_DIGITS, 0x6137, 122 * 2)
    store<u16>(_HEX_DIGITS, 0x6237, 123 * 2)
    store<u16>(_HEX_DIGITS, 0x6337, 124 * 2)
    store<u16>(_HEX_DIGITS, 0x6437, 125 * 2)
    store<u16>(_HEX_DIGITS, 0x6537, 126 * 2)
    store<u16>(_HEX_DIGITS, 0x6637, 127 * 2) // 7f

    store<u16>(_HEX_DIGITS, 0x3038, 128 * 2) // 80
    store<u16>(_HEX_DIGITS, 0x3138, 129 * 2)
    store<u16>(_HEX_DIGITS, 0x3238, 130 * 2)
    store<u16>(_HEX_DIGITS, 0x3338, 131 * 2)
    store<u16>(_HEX_DIGITS, 0x3438, 132 * 2)
    store<u16>(_HEX_DIGITS, 0x3538, 133 * 2)
    store<u16>(_HEX_DIGITS, 0x3638, 134 * 2)
    store<u16>(_HEX_DIGITS, 0x3738, 135 * 2)
    store<u16>(_HEX_DIGITS, 0x3838, 136 * 2)
    store<u16>(_HEX_DIGITS, 0x3938, 137 * 2)
    store<u16>(_HEX_DIGITS, 0x6138, 138 * 2)
    store<u16>(_HEX_DIGITS, 0x6238, 139 * 2)
    store<u16>(_HEX_DIGITS, 0x6338, 140 * 2)
    store<u16>(_HEX_DIGITS, 0x6438, 141 * 2)
    store<u16>(_HEX_DIGITS, 0x6538, 142 * 2)
    store<u16>(_HEX_DIGITS, 0x6638, 143 * 2) // 8f

    store<u16>(_HEX_DIGITS, 0x3039, 144 * 2) // 90
    store<u16>(_HEX_DIGITS, 0x3139, 145 * 2)
    store<u16>(_HEX_DIGITS, 0x3239, 146 * 2)
    store<u16>(_HEX_DIGITS, 0x3339, 147 * 2)
    store<u16>(_HEX_DIGITS, 0x3439, 148 * 2)
    store<u16>(_HEX_DIGITS, 0x3539, 149 * 2)
    store<u16>(_HEX_DIGITS, 0x3639, 150 * 2)
    store<u16>(_HEX_DIGITS, 0x3739, 151 * 2)
    store<u16>(_HEX_DIGITS, 0x3839, 152 * 2)
    store<u16>(_HEX_DIGITS, 0x3939, 153 * 2)
    store<u16>(_HEX_DIGITS, 0x6139, 154 * 2)
    store<u16>(_HEX_DIGITS, 0x6239, 155 * 2)
    store<u16>(_HEX_DIGITS, 0x6339, 156 * 2)
    store<u16>(_HEX_DIGITS, 0x6439, 157 * 2)
    store<u16>(_HEX_DIGITS, 0x6539, 158 * 2)
    store<u16>(_HEX_DIGITS, 0x6639, 159 * 2) // 9f

    store<u16>(_HEX_DIGITS, 0x3061, 160 * 2) // a0
    store<u16>(_HEX_DIGITS, 0x3161, 161 * 2)
    store<u16>(_HEX_DIGITS, 0x3261, 162 * 2)
    store<u16>(_HEX_DIGITS, 0x3361, 163 * 2)
    store<u16>(_HEX_DIGITS, 0x3461, 164 * 2)
    store<u16>(_HEX_DIGITS, 0x3561, 165 * 2)
    store<u16>(_HEX_DIGITS, 0x3661, 166 * 2)
    store<u16>(_HEX_DIGITS, 0x3761, 167 * 2)
    store<u16>(_HEX_DIGITS, 0x3861, 168 * 2)
    store<u16>(_HEX_DIGITS, 0x3961, 169 * 2)
    store<u16>(_HEX_DIGITS, 0x6161, 170 * 2)
    store<u16>(_HEX_DIGITS, 0x6261, 171 * 2)
    store<u16>(_HEX_DIGITS, 0x6361, 172 * 2)
    store<u16>(_HEX_DIGITS, 0x6461, 173 * 2)
    store<u16>(_HEX_DIGITS, 0x6561, 174 * 2)
    store<u16>(_HEX_DIGITS, 0x6661, 175 * 2) // af

    store<u16>(_HEX_DIGITS, 0x3062, 176 * 2) // b0
    store<u16>(_HEX_DIGITS, 0x3162, 177 * 2)
    store<u16>(_HEX_DIGITS, 0x3262, 178 * 2)
    store<u16>(_HEX_DIGITS, 0x3362, 179 * 2)
    store<u16>(_HEX_DIGITS, 0x3462, 180 * 2)
    store<u16>(_HEX_DIGITS, 0x3562, 181 * 2)
    store<u16>(_HEX_DIGITS, 0x3662, 182 * 2)
    store<u16>(_HEX_DIGITS, 0x3762, 183 * 2)
    store<u16>(_HEX_DIGITS, 0x3862, 184 * 2)
    store<u16>(_HEX_DIGITS, 0x3962, 185 * 2)
    store<u16>(_HEX_DIGITS, 0x6162, 186 * 2)
    store<u16>(_HEX_DIGITS, 0x6262, 187 * 2)
    store<u16>(_HEX_DIGITS, 0x6362, 188 * 2)
    store<u16>(_HEX_DIGITS, 0x6462, 189 * 2)
    store<u16>(_HEX_DIGITS, 0x6562, 190 * 2)
    store<u16>(_HEX_DIGITS, 0x6662, 191 * 2) // bf

    store<u16>(_HEX_DIGITS, 0x3063, 192 * 2) // c0
    store<u16>(_HEX_DIGITS, 0x3163, 193 * 2)
    store<u16>(_HEX_DIGITS, 0x3263, 194 * 2)
    store<u16>(_HEX_DIGITS, 0x3363, 195 * 2)
    store<u16>(_HEX_DIGITS, 0x3463, 196 * 2)
    store<u16>(_HEX_DIGITS, 0x3563, 197 * 2)
    store<u16>(_HEX_DIGITS, 0x3663, 198 * 2)
    store<u16>(_HEX_DIGITS, 0x3763, 199 * 2)
    store<u16>(_HEX_DIGITS, 0x3863, 200 * 2)
    store<u16>(_HEX_DIGITS, 0x3963, 201 * 2)
    store<u16>(_HEX_DIGITS, 0x6163, 202 * 2)
    store<u16>(_HEX_DIGITS, 0x6263, 203 * 2)
    store<u16>(_HEX_DIGITS, 0x6363, 204 * 2)
    store<u16>(_HEX_DIGITS, 0x6463, 205 * 2)
    store<u16>(_HEX_DIGITS, 0x6563, 206 * 2)
    store<u16>(_HEX_DIGITS, 0x6663, 207 * 2) // cf

    store<u16>(_HEX_DIGITS, 0x3064, 208 * 2) // d0
    store<u16>(_HEX_DIGITS, 0x3164, 209 * 2)
    store<u16>(_HEX_DIGITS, 0x3264, 210 * 2)
    store<u16>(_HEX_DIGITS, 0x3364, 211 * 2)
    store<u16>(_HEX_DIGITS, 0x3464, 212 * 2)
    store<u16>(_HEX_DIGITS, 0x3564, 213 * 2)
    store<u16>(_HEX_DIGITS, 0x3664, 214 * 2)
    store<u16>(_HEX_DIGITS, 0x3764, 215 * 2)
    store<u16>(_HEX_DIGITS, 0x3864, 216 * 2)
    store<u16>(_HEX_DIGITS, 0x3964, 217 * 2)
    store<u16>(_HEX_DIGITS, 0x6164, 218 * 2)
    store<u16>(_HEX_DIGITS, 0x6264, 219 * 2)
    store<u16>(_HEX_DIGITS, 0x6364, 220 * 2)
    store<u16>(_HEX_DIGITS, 0x6464, 221 * 2)
    store<u16>(_HEX_DIGITS, 0x6564, 222 * 2)
    store<u16>(_HEX_DIGITS, 0x6664, 223 * 2) // df

    store<u16>(_HEX_DIGITS, 0x3065, 224 * 2) // e0
    store<u16>(_HEX_DIGITS, 0x3165, 225 * 2)
    store<u16>(_HEX_DIGITS, 0x3265, 226 * 2)
    store<u16>(_HEX_DIGITS, 0x3365, 227 * 2)
    store<u16>(_HEX_DIGITS, 0x3465, 228 * 2)
    store<u16>(_HEX_DIGITS, 0x3565, 229 * 2)
    store<u16>(_HEX_DIGITS, 0x3665, 230 * 2)
    store<u16>(_HEX_DIGITS, 0x3765, 231 * 2)
    store<u16>(_HEX_DIGITS, 0x3865, 232 * 2)
    store<u16>(_HEX_DIGITS, 0x3965, 233 * 2)
    store<u16>(_HEX_DIGITS, 0x6165, 234 * 2)
    store<u16>(_HEX_DIGITS, 0x6265, 235 * 2)
    store<u16>(_HEX_DIGITS, 0x6365, 236 * 2)
    store<u16>(_HEX_DIGITS, 0x6465, 237 * 2)
    store<u16>(_HEX_DIGITS, 0x6565, 238 * 2)
    store<u16>(_HEX_DIGITS, 0x6665, 239 * 2) // ef

    store<u16>(_HEX_DIGITS, 0x3066, 240 * 2) // f0
    store<u16>(_HEX_DIGITS, 0x3166, 241 * 2)
    store<u16>(_HEX_DIGITS, 0x3266, 242 * 2)
    store<u16>(_HEX_DIGITS, 0x3366, 243 * 2)
    store<u16>(_HEX_DIGITS, 0x3466, 244 * 2)
    store<u16>(_HEX_DIGITS, 0x3566, 245 * 2)
    store<u16>(_HEX_DIGITS, 0x3666, 246 * 2)
    store<u16>(_HEX_DIGITS, 0x3766, 247 * 2)
    store<u16>(_HEX_DIGITS, 0x3866, 248 * 2)
    store<u16>(_HEX_DIGITS, 0x3966, 249 * 2)
    store<u16>(_HEX_DIGITS, 0x6166, 250 * 2)
    store<u16>(_HEX_DIGITS, 0x6266, 251 * 2)
    store<u16>(_HEX_DIGITS, 0x6366, 252 * 2)
    store<u16>(_HEX_DIGITS, 0x6466, 253 * 2)
    store<u16>(_HEX_DIGITS, 0x6566, 254 * 2)
    store<u16>(_HEX_DIGITS, 0x6666, 255 * 2) // ff
  }
  return _HEX_DIGITS
}

// @ts-ignore: decorator
let _ANY_DIGITS: usize = -1;

function get_ANY_DIGITS(): usize {
  if (_ANY_DIGITS == -1) {
    _ANY_DIGITS = <usize>(malloc(36))
    store<u8>(_ANY_DIGITS, CharCode._0)
    store<u8>(_ANY_DIGITS, CharCode._1, 1)
    store<u8>(_ANY_DIGITS, CharCode._2, 2)
    store<u8>(_ANY_DIGITS, CharCode._3, 3)
    store<u8>(_ANY_DIGITS, CharCode._4, 4)
    store<u8>(_ANY_DIGITS, CharCode._5, 5)
    store<u8>(_ANY_DIGITS, CharCode._6, 6)
    store<u8>(_ANY_DIGITS, CharCode._7, 7)
    store<u8>(_ANY_DIGITS, CharCode._8, 8)
    store<u8>(_ANY_DIGITS, CharCode._9, 9)
    store<u8>(_ANY_DIGITS, CharCode.a, 10)
    store<u8>(_ANY_DIGITS, CharCode.b, 11)
    store<u8>(_ANY_DIGITS, CharCode.c, 12)
    store<u8>(_ANY_DIGITS, CharCode.d, 13)
    store<u8>(_ANY_DIGITS, CharCode.e, 14)
    store<u8>(_ANY_DIGITS, CharCode.f, 15)
    store<u8>(_ANY_DIGITS, CharCode.g, 16)
    store<u8>(_ANY_DIGITS, CharCode.h, 17)
    store<u8>(_ANY_DIGITS, CharCode.i, 18)
    store<u8>(_ANY_DIGITS, CharCode.j, 19)
    store<u8>(_ANY_DIGITS, CharCode.k, 20)
    store<u8>(_ANY_DIGITS, CharCode.l, 21)
    store<u8>(_ANY_DIGITS, CharCode.m, 22)
    store<u8>(_ANY_DIGITS, CharCode.n, 23)
    store<u8>(_ANY_DIGITS, CharCode.o, 24)
    store<u8>(_ANY_DIGITS, CharCode.p, 25)
    store<u8>(_ANY_DIGITS, CharCode.q, 26)
    store<u8>(_ANY_DIGITS, CharCode.r, 27)
    store<u8>(_ANY_DIGITS, CharCode.s, 28)
    store<u8>(_ANY_DIGITS, CharCode.t, 29)
    store<u8>(_ANY_DIGITS, CharCode.u, 30)
    store<u8>(_ANY_DIGITS, CharCode.v, 31)
    store<u8>(_ANY_DIGITS, CharCode.w, 32)
    store<u8>(_ANY_DIGITS, CharCode.x, 33)
    store<u8>(_ANY_DIGITS, CharCode.y, 34)
    store<u8>(_ANY_DIGITS, CharCode.z, 35)
  }
  return _ANY_DIGITS
}

// @ts-ignore: decorator
let _EXP_POWERS: usize = -1

function get_EXP_POWERS(): usize {
  if (_EXP_POWERS == -1) {
    _EXP_POWERS = <usize>(malloc(87 * 2))
    store<i16>(_EXP_POWERS, -1220, 0 * 2)
    store<i16>(_EXP_POWERS, -1193, 1 * 2)
    store<i16>(_EXP_POWERS, -1166, 2 * 2)
    store<i16>(_EXP_POWERS, -1140, 3 * 2)
    store<i16>(_EXP_POWERS, -1113, 4 * 2)
    store<i16>(_EXP_POWERS, -1087, 5 * 2)
    store<i16>(_EXP_POWERS, -1060, 6 * 2)
    store<i16>(_EXP_POWERS, -1034, 7 * 2)
    store<i16>(_EXP_POWERS, -1007, 8 * 2)
    store<i16>(_EXP_POWERS, -980, 9 * 2)
    store<i16>(_EXP_POWERS, -954, 10 * 2)
    store<i16>(_EXP_POWERS, -927, 11 * 2)
    store<i16>(_EXP_POWERS, -901, 12 * 2)
    store<i16>(_EXP_POWERS, -874, 13 * 2)
    store<i16>(_EXP_POWERS, -847, 14 * 2)
    store<i16>(_EXP_POWERS, -821, 15 * 2)
    store<i16>(_EXP_POWERS, -794, 16 * 2)
    store<i16>(_EXP_POWERS, -768, 17 * 2)
    store<i16>(_EXP_POWERS, -741, 18 * 2)
    store<i16>(_EXP_POWERS, -715, 19 * 2)
    store<i16>(_EXP_POWERS, -688, 20 * 2)
    store<i16>(_EXP_POWERS, -661, 21 * 2)
    store<i16>(_EXP_POWERS, -635, 22 * 2)
    store<i16>(_EXP_POWERS, -608, 23 * 2)
    store<i16>(_EXP_POWERS, -582, 24 * 2)
    store<i16>(_EXP_POWERS, -555, 25 * 2)
    store<i16>(_EXP_POWERS, -529, 26 * 2)
    store<i16>(_EXP_POWERS, -502, 27 * 2)
    store<i16>(_EXP_POWERS, -475, 28 * 2)
    store<i16>(_EXP_POWERS, -449, 29 * 2)
    store<i16>(_EXP_POWERS, -422, 30 * 2)
    store<i16>(_EXP_POWERS, -396, 31 * 2)
    store<i16>(_EXP_POWERS, -369, 32 * 2)
    store<i16>(_EXP_POWERS, -343, 33 * 2)
    store<i16>(_EXP_POWERS, -316, 34 * 2)
    store<i16>(_EXP_POWERS, -289, 35 * 2)
    store<i16>(_EXP_POWERS, -263, 36 * 2)
    store<i16>(_EXP_POWERS, -236, 37 * 2)
    store<i16>(_EXP_POWERS, -210, 38 * 2)
    store<i16>(_EXP_POWERS, -183, 39 * 2)
    store<i16>(_EXP_POWERS, -157, 40 * 2)
    store<i16>(_EXP_POWERS, -130, 41 * 2)
    store<i16>(_EXP_POWERS, -103, 42 * 2)
    store<i16>(_EXP_POWERS, -77, 43 * 2)
    store<i16>(_EXP_POWERS, -50, 44 * 2)
    store<i16>(_EXP_POWERS, -24, 45 * 2)
    store<i16>(_EXP_POWERS, 3, 46 * 2)
    store<i16>(_EXP_POWERS, 30, 47 * 2)
    store<i16>(_EXP_POWERS, 56, 48 * 2)
    store<i16>(_EXP_POWERS, 83, 49 * 2)
    store<i16>(_EXP_POWERS, 109, 50 * 2)
    store<i16>(_EXP_POWERS, 136, 51 * 2)
    store<i16>(_EXP_POWERS, 162, 52 * 2)
    store<i16>(_EXP_POWERS, 189, 53 * 2)
    store<i16>(_EXP_POWERS, 216, 54 * 2)
    store<i16>(_EXP_POWERS, 242, 55 * 2)
    store<i16>(_EXP_POWERS, 269, 56 * 2)
    store<i16>(_EXP_POWERS, 295, 57 * 2)
    store<i16>(_EXP_POWERS, 322, 58 * 2)
    store<i16>(_EXP_POWERS, 348, 59 * 2)
    store<i16>(_EXP_POWERS, 375, 60 * 2)
    store<i16>(_EXP_POWERS, 402, 61 * 2)
    store<i16>(_EXP_POWERS, 428, 62 * 2)
    store<i16>(_EXP_POWERS, 455, 63 * 2)
    store<i16>(_EXP_POWERS, 481, 64 * 2)
    store<i16>(_EXP_POWERS, 508, 65 * 2)
    store<i16>(_EXP_POWERS, 534, 66 * 2)
    store<i16>(_EXP_POWERS, 561, 67 * 2)
    store<i16>(_EXP_POWERS, 588, 68 * 2)
    store<i16>(_EXP_POWERS, 614, 69 * 2)
    store<i16>(_EXP_POWERS, 641, 70 * 2)
    store<i16>(_EXP_POWERS, 667, 71 * 2)
    store<i16>(_EXP_POWERS, 694, 72 * 2)
    store<i16>(_EXP_POWERS, 720, 73 * 2)
    store<i16>(_EXP_POWERS, 747, 74 * 2)
    store<i16>(_EXP_POWERS, 774, 75 * 2)
    store<i16>(_EXP_POWERS, 800, 76 * 2)
    store<i16>(_EXP_POWERS, 827, 77 * 2)
    store<i16>(_EXP_POWERS, 853, 78 * 2)
    store<i16>(_EXP_POWERS, 880, 79 * 2)
    store<i16>(_EXP_POWERS, 907, 80 * 2)
    store<i16>(_EXP_POWERS, 933, 81 * 2)
    store<i16>(_EXP_POWERS, 960, 82 * 2)
    store<i16>(_EXP_POWERS, 986, 83 * 2)
    store<i16>(_EXP_POWERS, 1013, 84 * 2)
    store<i16>(_EXP_POWERS, 1039, 85 * 2)
    store<i16>(_EXP_POWERS, 1066, 86 * 2)
  }
  return _EXP_POWERS
}

// 1e-348, 1e-340, ..., 1e340
// @ts-ignore: decorator
let _FRC_POWERS: usize = -1

function get_FRC_POWERS(): usize {
  if (_FRC_POWERS == -1) {
    _FRC_POWERS = <usize>(malloc(87 * 8))
    store<u64>(_FRC_POWERS, 0xFA8FD5A0081C0288, 0 * 8)
    store<u64>(_FRC_POWERS, 0xBAAEE17FA23EBF76, 1 * 8)
    store<u64>(_FRC_POWERS, 0x8B16FB203055AC76, 2 * 8)
    store<u64>(_FRC_POWERS, 0xCF42894A5DCE35EA, 3 * 8)
    store<u64>(_FRC_POWERS, 0x9A6BB0AA55653B2D, 4 * 8)
    store<u64>(_FRC_POWERS, 0xE61ACF033D1A45DF, 5 * 8)
    store<u64>(_FRC_POWERS, 0xAB70FE17C79AC6CA, 6 * 8)
    store<u64>(_FRC_POWERS, 0xFF77B1FCBEBCDC4F, 7 * 8)
    store<u64>(_FRC_POWERS, 0xBE5691EF416BD60C, 8 * 8)
    store<u64>(_FRC_POWERS, 0x8DD01FAD907FFC3C, 9 * 8)
    store<u64>(_FRC_POWERS, 0xD3515C2831559A83, 10 * 8)
    store<u64>(_FRC_POWERS, 0x9D71AC8FADA6C9B5, 11 * 8)
    store<u64>(_FRC_POWERS, 0xEA9C227723EE8BCB, 12 * 8)
    store<u64>(_FRC_POWERS, 0xAECC49914078536D, 13 * 8)
    store<u64>(_FRC_POWERS, 0x823C12795DB6CE57, 14 * 8)
    store<u64>(_FRC_POWERS, 0xC21094364DFB5637, 15 * 8)
    store<u64>(_FRC_POWERS, 0x9096EA6F3848984F, 16 * 8)
    store<u64>(_FRC_POWERS, 0xD77485CB25823AC7, 17 * 8)
    store<u64>(_FRC_POWERS, 0xA086CFCD97BF97F4, 18 * 8)
    store<u64>(_FRC_POWERS, 0xEF340A98172AACE5, 19 * 8)
    store<u64>(_FRC_POWERS, 0xB23867FB2A35B28E, 20 * 8)
    store<u64>(_FRC_POWERS, 0x84C8D4DFD2C63F3B, 21 * 8)
    store<u64>(_FRC_POWERS, 0xC5DD44271AD3CDBA, 22 * 8)
    store<u64>(_FRC_POWERS, 0x936B9FCEBB25C996, 23 * 8)
    store<u64>(_FRC_POWERS, 0xDBAC6C247D62A584, 24 * 8)
    store<u64>(_FRC_POWERS, 0xA3AB66580D5FDAF6, 25 * 8)
    store<u64>(_FRC_POWERS, 0xF3E2F893DEC3F126, 26 * 8)
    store<u64>(_FRC_POWERS, 0xB5B5ADA8AAFF80B8, 27 * 8)
    store<u64>(_FRC_POWERS, 0x87625F056C7C4A8B, 28 * 8)
    store<u64>(_FRC_POWERS, 0xC9BCFF6034C13053, 29 * 8)
    store<u64>(_FRC_POWERS, 0x964E858C91BA2655, 30 * 8)
    store<u64>(_FRC_POWERS, 0xDFF9772470297EBD, 31 * 8)
    store<u64>(_FRC_POWERS, 0xA6DFBD9FB8E5B88F, 32 * 8)
    store<u64>(_FRC_POWERS, 0xF8A95FCF88747D94, 33 * 8)
    store<u64>(_FRC_POWERS, 0xB94470938FA89BCF, 34 * 8)
    store<u64>(_FRC_POWERS, 0x8A08F0F8BF0F156B, 35 * 8)
    store<u64>(_FRC_POWERS, 0xCDB02555653131B6, 36 * 8)
    store<u64>(_FRC_POWERS, 0x993FE2C6D07B7FAC, 37 * 8)
    store<u64>(_FRC_POWERS, 0xE45C10C42A2B3B06, 38 * 8)
    store<u64>(_FRC_POWERS, 0xAA242499697392D3, 39 * 8)
    store<u64>(_FRC_POWERS, 0xFD87B5F28300CA0E, 40 * 8)
    store<u64>(_FRC_POWERS, 0xBCE5086492111AEB, 41 * 8)
    store<u64>(_FRC_POWERS, 0x8CBCCC096F5088CC, 42 * 8)
    store<u64>(_FRC_POWERS, 0xD1B71758E219652C, 43 * 8)
    store<u64>(_FRC_POWERS, 0x9C40000000000000, 44 * 8)
    store<u64>(_FRC_POWERS, 0xE8D4A51000000000, 45 * 8)
    store<u64>(_FRC_POWERS, 0xAD78EBC5AC620000, 46 * 8)
    store<u64>(_FRC_POWERS, 0x813F3978F8940984, 47 * 8)
    store<u64>(_FRC_POWERS, 0xC097CE7BC90715B3, 48 * 8)
    store<u64>(_FRC_POWERS, 0x8F7E32CE7BEA5C70, 49 * 8)
    store<u64>(_FRC_POWERS, 0xD5D238A4ABE98068, 50 * 8)
    store<u64>(_FRC_POWERS, 0x9F4F2726179A2245, 51 * 8)
    store<u64>(_FRC_POWERS, 0xED63A231D4C4FB27, 52 * 8)
    store<u64>(_FRC_POWERS, 0xB0DE65388CC8ADA8, 53 * 8)
    store<u64>(_FRC_POWERS, 0x83C7088E1AAB65DB, 54 * 8)
    store<u64>(_FRC_POWERS, 0xC45D1DF942711D9A, 55 * 8)
    store<u64>(_FRC_POWERS, 0x924D692CA61BE758, 56 * 8)
    store<u64>(_FRC_POWERS, 0xDA01EE641A708DEA, 57 * 8)
    store<u64>(_FRC_POWERS, 0xA26DA3999AEF774A, 58 * 8)
    store<u64>(_FRC_POWERS, 0xF209787BB47D6B85, 59 * 8)
    store<u64>(_FRC_POWERS, 0xB454E4A179DD1877, 60 * 8)
    store<u64>(_FRC_POWERS, 0x865B86925B9BC5C2, 61 * 8)
    store<u64>(_FRC_POWERS, 0xC83553C5C8965D3D, 62 * 8)
    store<u64>(_FRC_POWERS, 0x952AB45CFA97A0B3, 63 * 8)
    store<u64>(_FRC_POWERS, 0xDE469FBD99A05FE3, 64 * 8)
    store<u64>(_FRC_POWERS, 0xA59BC234DB398C25, 65 * 8)
    store<u64>(_FRC_POWERS, 0xF6C69A72A3989F5C, 66 * 8)
    store<u64>(_FRC_POWERS, 0xB7DCBF5354E9BECE, 67 * 8)
    store<u64>(_FRC_POWERS, 0x88FCF317F22241E2, 68 * 8)
    store<u64>(_FRC_POWERS, 0xCC20CE9BD35C78A5, 69 * 8)
    store<u64>(_FRC_POWERS, 0x98165AF37B2153DF, 70 * 8)
    store<u64>(_FRC_POWERS, 0xE2A0B5DC971F303A, 71 * 8)
    store<u64>(_FRC_POWERS, 0xA8D9D1535CE3B396, 72 * 8)
    store<u64>(_FRC_POWERS, 0xFB9B7CD9A4A7443C, 73 * 8)
    store<u64>(_FRC_POWERS, 0xBB764C4CA7A44410, 74 * 8)
    store<u64>(_FRC_POWERS, 0x8BAB8EEFB6409C1A, 75 * 8)
    store<u64>(_FRC_POWERS, 0xD01FEF10A657842C, 76 * 8)
    store<u64>(_FRC_POWERS, 0x9B10A4E5E9913129, 77 * 8)
    store<u64>(_FRC_POWERS, 0xE7109BFBA19C0C9D, 78 * 8)
    store<u64>(_FRC_POWERS, 0xAC2820D9623BF429, 79 * 8)
    store<u64>(_FRC_POWERS, 0x80444B5E7AA7CF85, 80 * 8)
    store<u64>(_FRC_POWERS, 0xBF21E44003ACDD2D, 81 * 8)
    store<u64>(_FRC_POWERS, 0x8E679C2F5E44FF8F, 82 * 8)
    store<u64>(_FRC_POWERS, 0xD433179D9C8CB841, 83 * 8)
    store<u64>(_FRC_POWERS, 0x9E19DB92B4E31BA9, 84 * 8)
    store<u64>(_FRC_POWERS, 0xEB96BF6EBADF77D9, 85 * 8)
    store<u64>(_FRC_POWERS, 0xAF87023B9BF0EE6B, 86 * 8)
  }
  return _FRC_POWERS
}

// @ts-ignore: decorator
@inline
export function isPowerOf2<T extends number>(value: T): bool {
  return popcnt<T>(value) == 1;
}

// Count number of decimals for u32 values
// In our case input value always non-zero so we can simplify some parts
export function decimalCount32(value: u32): u32 {
  if (value < 100000) {
    if (value < 100) {
      return 1 + u32(value >= 10);
    } else {
      return 3 + u32(value >= 10000) + u32(value >= 1000);
    }
  } else {
    if (value < 10000000) {
      return 6 + u32(value >= 1000000);
    } else {
      return 8 + u32(value >= 1000000000) + u32(value >= 100000000);
    }
  }
}

// Count number of decimals for u64 values
// In our case input value always greater than 2^32-1 so we can skip some parts
export function decimalCount64High(value: u64): u32 {
  if (value < 1000000000000000) {
    if (value < 1000000000000) {
      return 10 + u32(value >= 100000000000) + u32(value >= 10000000000);
    } else {
      return 13 + u32(value >= 100000000000000) + u32(value >= 10000000000000);
    }
  } else {
    if (value < 100000000000000000) {
      return 16 + u32(value >= 10000000000000000);
    } else {
      return 18 + u32(value >= 10000000000000000000) + u32(value >= 1000000000000000000);
    }
  }
}

function ulog_base(num: u64, base: i32): u32 {
  if (isPowerOf2(base)) {
    return (63 - <u32>clz(num)) / (31 - <u32>clz(base)) + 1;
  }
  var b64 = u64(base), b = b64, e: u32 = 1;
  while (num >= b) {
    num /= b;
    b *= b;
    e <<= 1;
  }
  while (num >= 1) {
    num /= b64;
    e++;
  }
  return e - 1;
}

function utoa32_dec_lut(buffer: usize, num: u32, offset: usize): void {
  while (num >= 10000) {
    // in most VMs i32/u32 div and modulo by constant can be shared and simplificate
    let t = num / 10000;
    let r = num % 10000;
    num = t;

    let d1 = r / 100;
    let d2 = r % 100;

    let digits1 = <u32>load<u16>(get_DIGITS() + (<usize>d1 << alignof<u16>()));
    let digits2 = <u32>load<u16>(get_DIGITS() + (<usize>d2 << alignof<u16>()));

    offset -= 4;
    store<u32>(buffer + offset, digits1 | (digits2 << 16));
  }

  if (num >= 100) {
    let t  = num / 100;
    let d1 = num % 100;
    num = t;
    offset -= 2;
    let digits = load<u16>(get_DIGITS() + (<usize>d1 << alignof<u16>()));
    store<u16>(buffer + offset, digits);
  }

  if (num >= 10) {
    offset -= 2;
    let digits = load<u16>(get_DIGITS() + (<usize>num << alignof<u16>()));
    store<u16>(buffer + offset, digits);
  } else {
    offset -= 1;
    let digit = CharCode._0 + num;
    store<u8>(buffer + offset, digit);
  }
}

function utoa64_dec_lut(buffer: usize, num: u64, offset: usize): void {
  while (num >= 100000000) {
    let t = num / 100000000;
    let r = <usize>(num - t * 100000000);
    num = t;

    let b = r / 10000;
    let c = r % 10000;

    let b1 = b / 100;
    let b2 = b % 100;
    let c1 = c / 100;
    let c2 = c % 100;

    let digits1 = <u64>load<u16>(get_DIGITS() + (<usize>c1 << alignof<u16>()));
    let digits2 = <u64>load<u16>(get_DIGITS() + (<usize>c2 << alignof<u16>()));

    offset -= 4;
    store<u32>(buffer + offset, digits1 | (digits2 << 16));

    digits1 = <u32>load<u16>(get_DIGITS() + (<usize>b1 << alignof<u16>()));
    digits2 = <u32>load<u16>(get_DIGITS() + (<usize>b2 << alignof<u16>()));

    offset -= 4;
    store<u32>(buffer + offset, digits1 | (digits2 << 16));
  }

  utoa32_dec_lut(buffer, <u32>num, offset);
}

function utoa_hex_lut(buffer: usize, num: u64, offset: usize): void {
  const lut = get_HEX_DIGITS()
  while (offset >= 2) {
    offset -= 2;
    store<u16>(
      buffer + offset,
      load<u16>(lut + ((<usize>num & 0xFF) << alignof<u16>()))
    );
    num >>= 8;
  }
  if (offset & 1) {
    store<u8>(buffer, load<u8>(lut + (<usize>num << 5)));
  }
}

function utoa_dec_simple<T extends number>(buffer: usize, num: T, offset: usize): void {
  do {
    let t = num / 10;
    let r = <u32>(num % 10);
    num = changetype<T>(t);
    offset--;
    store<u8>(buffer + offset, CharCode._0 + r);
  } while (num);
}

function utoa_hex_simple<T extends number>(buffer: usize, num: T, offset: usize): void {
  do {
    let d = num & 0x0F | CharCode._0;
    d += select<T>(<T>0x27, <T>0, d > <T>CharCode._9);
    offset--;
    store<u8>(buffer + offset, d);
    // @ts-ignore: type
    num >>= 4;
  } while (num);
}

// @ts-ignore: decorator
@inline
export function utoa32_dec_core(buffer: usize, num: u32, offset: usize): void {
  if (ASC_SHRINK_LEVEL >= 1) {
    utoa_dec_simple<u32>(buffer, num, offset);
  } else {
    utoa32_dec_lut(buffer, num, offset);
  }
}

// @ts-ignore: decorator
@inline
function utoa32_hex_core(buffer: usize, num: u32, offset: usize): void {
  if (ASC_SHRINK_LEVEL >= 1) {
    utoa_hex_simple<u32>(buffer, num, offset);
  } else {
    utoa_hex_lut(buffer, num, offset);
  }
}

// @ts-ignore: decorator
@inline
function utoa64_dec_core(buffer: usize, num: u64, offset: usize): void {
  if (ASC_SHRINK_LEVEL >= 1) {
    utoa_dec_simple<u64>(buffer, num, offset);
  } else {
    utoa64_dec_lut(buffer, num, offset);
  }
}

// @ts-ignore: decorator
@inline
function utoa64_hex_core(buffer: usize, num: u64, offset: usize): void {
  if (ASC_SHRINK_LEVEL >= 1) {
    utoa_hex_simple<u64>(buffer, num, offset);
  } else {
    utoa_hex_lut(buffer, num, offset);
  }
}

function utoa64_any_core(buffer: usize, num: u64, offset: usize, radix: i32): void {
  const lut = get_ANY_DIGITS();
  var base = u64(radix);
  if ((radix & (radix - 1)) == 0) { // for radix which pow of two
    let shift = u64(ctz(radix) & 7);
    let mask = base - 1;
    do {
      offset--;
      store<u8>(buffer + offset, load<u8>(lut + (usize(num & mask))));
      num >>= shift;
    } while (num);
  } else {
    do {
      offset--;
      let q = num / base;
      store<u8>(buffer + offset, load<u8>(lut + (usize(num - q * base))));
      num = q;
    } while (num);
  }
}

export function utoa32(value: u32, radix: i32): u32 {
  if (radix < 2 || radix > 36) {
    //throw new RangeError("toString() radix argument must be between 2 and 36");
    throw new RangeError()
  }
  if (!value) return singleByteString(CharCode._0);
  var out: u32 = 0;

  if (radix == 10) {
    let decimals = decimalCount32(value);
    out = allocateString(decimals);
    utoa32_dec_core(out + 8, value, decimals);
  } else if (radix == 16) {
    let decimals = (31 - clz(value) >> 2) + 1;
    out = allocateString(decimals);
    utoa32_hex_core(out + 8, value, decimals);
  } else {
    let decimals = ulog_base(value, radix);
    out = allocateString(decimals);
    utoa64_any_core(out + 8, value, decimals, radix);
  }
  return out; // retains
}

export function itoa32(value: i32, radix: i32): u32 {
  if (radix < 2 || radix > 36) {
    //throw new RangeError("toString() radix argument must be between 2 and 36");
    throw new RangeError()
  }
  if (!value) return singleByteString(CharCode._0);

  var sign = value >>> 31;
  if (sign) value = -value;
  var out: u32 = 0;

  if (radix == 10) {
    let decimals = decimalCount32(value) + sign;
    out = allocateString(decimals);
    utoa32_dec_core(<usize>(out + 8), value, decimals);
  } else if (radix == 16) {
    let decimals = (31 - clz(value) >> 2) + 1 + sign;
    out = allocateString(decimals);
    utoa32_hex_core(<usize>(out + 8), value, decimals);
  } else {
    let val32 = u32(value);
    let decimals = ulog_base(val32, radix) + sign;
    out = allocateString(decimals);
    utoa64_any_core(<usize>(out + 8), val32, decimals, radix);
  }
  if (sign) store<u8>(<usize>(out + 8), CharCode.MINUS);
  return out; // retains
}

export function utoa64(value: u64, radix: i32): u32 {
  if (radix < 2 || radix > 36) {
    //throw new RangeError("toString() radix argument must be between 2 and 36");
    throw new RangeError()
  }
  if (!value) return singleByteString(CharCode._0);
  var out: u32 = 0;

  if (radix == 10) {
    if (value <= u32.MAX_VALUE) {
      let val32    = <u32>value;
      let decimals = decimalCount32(val32);
      out = allocateString(decimals);
      utoa32_dec_core(out + 8, val32, decimals);
    } else {
      let decimals = decimalCount64High(value);
      out = allocateString(decimals);
      utoa64_dec_core(out + 8, value, decimals);
    }
  } else if (radix == 16) {
    let decimals = (63 - u32(clz(value)) >> 2) + 1;
    out = allocateString(decimals);
    utoa64_hex_core(out + 8, value, decimals);
  } else {
    let decimals = ulog_base(value, radix);
    out = allocateString(decimals);
    utoa64_any_core(out + 8, value, decimals, radix);
  }
  return out; // retains
}

export function itoa64(value: i64, radix: i32): u32 {
  if (radix < 2 || radix > 36) {
    //throw new RangeError("toString() radix argument must be between 2 and 36");
    throw new RangeError()
  }
  if (!value) return singleByteString(CharCode._0);

  var sign = u32(value >>> 63);
  if (sign) value = -value;
  var out: u32 = 0;

  if (radix == 10) {
    if (<u64>value <= <u64>u32.MAX_VALUE) {
      let val32    = <u32>value;
      let decimals = decimalCount32(val32) + sign;
      out = allocateString(decimals);
      utoa32_dec_core(out + 8, val32, decimals);
    } else {
      let decimals = decimalCount64High(value) + sign;
      out = allocateString(decimals);
      utoa64_dec_core(out + 8, value, decimals);
    }
  } else if (radix == 16) {
    let decimals = (63 - u32(clz(value)) >> 2) + 1 + sign;
    out = allocateString(decimals);
    utoa64_hex_core(out + 8, value, decimals);
  } else {
    let decimals = ulog_base(value, radix) + sign;
    out = allocateString(decimals);
    utoa64_any_core(out + 8, value, decimals, radix);
  }
  if (sign) store<u8>(out + 8, CharCode.MINUS);
  return out; // retains
}

// @ts-ignore: decorator
@lazy var _K: i32 = 0;

// // @ts-ignore: decorator
// @lazy
// var _frc: u64 = 0;

// @ts-ignore: decorator
@lazy var _exp: i32 = 0;

// @ts-ignore: decorator
@lazy var _frc_minus: u64 = 0;

// @ts-ignore: decorator
@lazy var _frc_plus:  u64 = 0;

// @ts-ignore: decorator
@lazy var _frc_pow: u64 = 0;

// @ts-ignore: decorator
@lazy var _exp_pow: i32 = 0;

// @ts-ignore: decorator
@inline
function umul64f(u: u64, v: u64): u64 {
  var u0 = u & 0xFFFFFFFF;
  var v0 = v & 0xFFFFFFFF;

  var u1 = u >> 32;
  var v1 = v >> 32;

  var l = u0 * v0;
  var t = u1 * v0 + (l >> 32);
  var w = u0 * v1 + (t & 0xFFFFFFFF);

  w += 0x7FFFFFFF; // rounding

  t >>= 32;
  w >>= 32;

  return u1 * v1 + t + w;
}

// @ts-ignore: decorator
@inline
function umul64e(e1: i32, e2: i32): i32 {
  return e1 + e2 + 64; // where 64 is significand size
}

// @ts-ignore: decorator
@inline
function normalizedBoundaries(f: u64, e: i32): void {
  var frc = (f << 1) + 1;
  var exp = e - 1;
  var off = <i32>clz<u64>(frc);
  frc <<= off;
  exp  -= off;

  var m = 1 + i32(f == 0x0010000000000000);

  _frc_plus  = frc;
  _frc_minus = ((f << m) - 1) << e - m - exp;
  _exp = exp;
}

// @ts-ignore: decorator
@inline
function grisuRound(buffer: usize, len: i32, delta: u64, rest: u64, ten_kappa: u64, wp_w: u64): void {
  var lastp = buffer + len - 1;
  var digit = load<u8>(lastp);
  while (
    rest < wp_w &&
    delta - rest >= ten_kappa && (
      rest + ten_kappa < wp_w ||
      wp_w - rest > rest + ten_kappa - wp_w
    )
  ) {
    --digit;
    rest += ten_kappa;
  }
  store<u8>(lastp, digit);
}

// @ts-ignore: decorator
@inline
function getCachedPower(minExp: i32): void {
  const c = reinterpret<f64>(0x3FD34413509F79FE); // 1 / lg(10) = 0.30102999566398114
  var dk = (-61 - minExp) * c + 347;	            // dk must be positive, so can do ceiling in positive
  var k = <i32>dk;
  k += i32(k != dk); // conversion with ceil

  var index = (k >> 3) + 1;
  _K = 348 - (index << 3);	// decimal exponent no need lookup table
  _frc_pow = load<u64>(get_FRC_POWERS() + (<usize>index << alignof<u64>()));
  _exp_pow = load<i16>(get_EXP_POWERS() + (<usize>index << alignof<i16>()));
}

// @ts-ignore: decorator
@inline
function grisu2(value: f64, buffer: usize, sign: i32): i32 {

  // frexp routine
  var uv  = reinterpret<u64>(value);
  var exp = i32((uv & 0x7FF0000000000000) >>> 52);
  var sid = uv & 0x000FFFFFFFFFFFFF;
  var frc = (u64(exp != 0) << 52) + sid;
  exp = select<i32>(exp, 1, exp) - (0x3FF + 52);

  normalizedBoundaries(frc, exp);
  getCachedPower(_exp);

  // normalize
  var off = <i32>clz<u64>(frc);
  frc <<= off;
  exp  -= off;

  var frc_pow = _frc_pow;
  var exp_pow = _exp_pow;

  var w_frc = umul64f(frc, frc_pow);
  var w_exp = umul64e(exp, exp_pow);

  var wp_frc = umul64f(_frc_plus, frc_pow) - 1;
  var wp_exp = umul64e(_exp, exp_pow);

  var wm_frc = umul64f(_frc_minus, frc_pow) + 1;
  var delta  = wp_frc - wm_frc;

  return genDigits(buffer, w_frc, w_exp, wp_frc, wp_exp, delta, sign);
}

function genDigits(buffer: usize, w_frc: u64, w_exp: i32, mp_frc: u64, mp_exp: i32, delta: u64, sign: i32): i32 {
  var one_exp = -mp_exp;
  var one_frc = (<u64>1) << one_exp;
  var mask    = one_frc - 1;

  var wp_w_frc = mp_frc - w_frc;

  var p1 = u32(mp_frc >> one_exp);
  var p2 = mp_frc & mask;

  var kappa = <i32>decimalCount32(p1);
  var len = sign;

  while (kappa > 0) {
    let d: u32;
    switch (kappa) {
      case 10: { d = p1 / 1000000000; p1 %= 1000000000; break; }
      case  9: { d = p1 /  100000000; p1 %=  100000000; break; }
      case  8: { d = p1 /   10000000; p1 %=   10000000; break; }
      case  7: { d = p1 /    1000000; p1 %=    1000000; break; }
      case  6: { d = p1 /     100000; p1 %=     100000; break; }
      case  5: { d = p1 /      10000; p1 %=      10000; break; }
      case  4: { d = p1 /       1000; p1 %=       1000; break; }
      case  3: { d = p1 /        100; p1 %=        100; break; }
      case  2: { d = p1 /         10; p1 %=         10; break; }
      case  1: { d = p1;              p1 =           0; break; }
      default: { d = 0; break; }
    }

    if (d | len) store<u8>(buffer + (len++), CharCode._0 + <u8>d);

    --kappa;
    let tmp = ((<u64>p1) << one_exp) + p2;
    if (tmp <= delta) {
      _K += kappa;
      grisuRound(buffer, len, delta, tmp, <u64>load<u32>(get_POWERS10() + (<usize>kappa << alignof<u32>())) << one_exp, wp_w_frc);
      return len;
    }
  }

  while (true) {
    p2    *= 10;
    delta *= 10;

    let d = p2 >> one_exp;
    if (d | len) store<u8>(buffer + (len++), CharCode._0 + <u8>d);

    p2 &= mask;
    --kappa;
    if (p2 < delta) {
      _K += kappa;
      wp_w_frc *= <u64>load<u32>(get_POWERS10() + (<usize>-kappa << alignof<u32>()));
      grisuRound(buffer, len, delta, p2, one_frc, wp_w_frc);
      return len;
    }
  }
}

// @ts-ignore: decorator
@inline
function genExponent(buffer: usize, k: i32): i32 {
  var sign = k < 0;
  if (sign) k = -k;
  var decimals = decimalCount32(k) + 1;
  utoa32_dec_core(buffer, k, decimals);
  store<u8>(buffer, <u8>select<u32>(CharCode.MINUS, CharCode.PLUS, sign));
  return decimals;
}

function prettify(buffer: usize, length: i32, k: i32): i32 {
  if (!k) {
    store<u16>(buffer + length, CharCode.DOT | (CharCode._0 << 8));
    return length + 2;
  }

  var kk = length + k;
  if (length <= kk && kk <= 21) {
    // 1234e7 -> 12340000000
    for (let i = length; i < kk; ++i) {
      store<u8>(buffer + i, CharCode._0);
    }
    store<u16>(buffer + kk, CharCode.DOT | (CharCode._0 << 8));
    return kk + 2;
  } else if (kk > 0 && kk <= 21) {
    // 1234e-2 -> 12.34
    let ptr = buffer + kk;
    memory.copy(
      ptr + 1,
      ptr,
      -k
    );
    store<u8>(buffer + kk, CharCode.DOT);
    return length + 1;
  } else if (-6 < kk && kk <= 0) {
    // 1234e-6 -> 0.001234
    let offset = 2 - kk;
    memory.copy(
      buffer + offset,
      buffer,
      length
    );
    store<u16>(buffer, CharCode._0 | (CharCode.DOT << 8));
    for (let i = 2; i < offset; ++i) {
      store<u8>(buffer + i, CharCode._0);
    }
    return length + offset;
  } else if (length == 1) {
    // 1e30
    store<u8>(buffer, CharCode.e, 1);
    length = genExponent(buffer + 2, kk - 1);
    return length + 2;
  } else {
    let len = length;
    memory.copy(
      buffer + 2,
      buffer + 1,
      len - 1
    );
    store<u8>(buffer,       CharCode.DOT, 1);
    store<u8>(buffer + len, CharCode.e,   1);
    length += genExponent(buffer + len + 2, kk - 1);
    return length + 2;
  }
}

function dtoa_core(buffer: usize, value: f64): i32 {
  var sign = i32(value < 0);
  if (sign) {
    value = -value;
    store<u8>(buffer, CharCode.MINUS);
  }
  // assert(value > 0 && value <= 1.7976931348623157e308);
  var len = grisu2(value, buffer, sign);
  len = prettify(buffer + sign, len - sign, _K);
  return len + sign;
}

// @ts-ignore: decorator
let _dtoa_buf: usize = -1

function get_dtoa_buf(): usize {
  if (_dtoa_buf == -1) {
    _dtoa_buf = <usize>(malloc(MAX_DOUBLE_LENGTH))
  }
  return _dtoa_buf
}

export function dtoa(value: f64): u32 {
  if (value == 0) {
    let ret = allocateString(3)
    store<u8>(ret, CharCode._0, 8)
    store<u8>(ret, CharCode.DOT, 8 + 1)
    store<u8>(ret, CharCode._0, 8 + 2)
    return ret
  } else if (!isFinite(value)) {
    if (isNaN(value)) {
      let ret = allocateString(3)
      store<u8>(ret, CharCode.N, 8)
      store<u8>(ret, CharCode.a, 8 + 1)
      store<u8>(ret, CharCode.N, 8 + 2)
      return ret
    } else if (value < 0) {
      let ret = allocateString(9)
      store<u8>(ret, CharCode.MINUS, 8)
      store<u8>(ret, CharCode.I, 8 + 1)
      store<u8>(ret, CharCode.n, 8 + 2)
      store<u8>(ret, CharCode.f, 8 + 3)
      store<u8>(ret, CharCode.i, 8 + 4)
      store<u8>(ret, CharCode.n, 8 + 5)
      store<u8>(ret, CharCode.i, 8 + 6)
      store<u8>(ret, CharCode.t, 8 + 7)
      store<u8>(ret, CharCode.y, 8 + 8)
      return ret
    } else {
      let ret = allocateString(8)
      store<u8>(ret, CharCode.I, 8)
      store<u8>(ret, CharCode.n, 8 + 1)
      store<u8>(ret, CharCode.f, 8 + 2)
      store<u8>(ret, CharCode.i, 8 + 3)
      store<u8>(ret, CharCode.n, 8 + 4)
      store<u8>(ret, CharCode.i, 8 + 5)
      store<u8>(ret, CharCode.t, 8 + 6)
      store<u8>(ret, CharCode.y, 8 + 7)
      return ret
    }
  }
  var size = dtoa_core(get_dtoa_buf(), value);
  var result = allocateString(size);
  memory.copy(result + 8, get_dtoa_buf(), size)
  return result;
}


export function dtoa_buffered(buffer: usize, value: f64): u32 {
  if (value == 0) {
    store<u8>(buffer, CharCode._0);
    store<u8>(buffer, CharCode.DOT, 1);
    store<u8>(buffer, CharCode._0,  2);
    return 3;
  }
  if (!isFinite(value)) {
    if (isNaN(value)) {
      store<u8>(buffer, CharCode.N);
      store<u8>(buffer, CharCode.a, 1);
      store<u8>(buffer, CharCode.N, 2);
      return 3;
    } else {
      let sign = value < 0;
      if (sign) {
        store<u8>(buffer, CharCode.MINUS); // -
        buffer += 1;
      }
      store<u32>(buffer, 0x69666E49, 0); // ifnI
      store<u32>(buffer, 0x7974696E, 4); // ytin
      return 8 + u32(sign);
    }
  }
  return dtoa_core(buffer, value);
}
