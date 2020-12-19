import { decRef, malloc } from './grainRuntime'
import { allocateString, singleByteString, stringSize } from './dataStructures'
import { GRAIN_GENERIC_HEAP_TAG_TYPE } from './tags';

/*
 * This file is a modified version of AssemblyScript's std/assembly/util/number.ts
 * Original file under Apache 2.0 License by AssemblyScript authors:
 *
 * https://github.com/AssemblyScript/assemblyscript/blob/d7ad4821a974d2491a0115cb35c85c649b34e7f0/LICENSE
 */

 // [TODO] find a new home for this that isn't in numberUtils
// @ts-ignore: decorator
@inline
export const enum CharCode {
  NEWLINE = 0xA,
  SPACE = 0x20,
  QUOTE = 0x22,
  LPAREN = 0x28,
  RPAREN = 0x29,
  STAR = 0x2A,
  PLUS = 0x2B,
  COMMA = 0x2C,
  MINUS = 0x2D,
  DOT = 0x2E,
  SLASH = 0x2F,
  _0 = 0x30,
  _1 = 0x31,
  _2 = 0x32,
  _3 = 0x33,
  _4 = 0x34,
  _5 = 0x35,
  _6 = 0x36,
  _7 = 0x37,
  _8 = 0x38,
  _9 = 0x39,
  COLON = 0x3A,
  SEMICOLON = 0x3B,
  LANGLE = 0x3C, // "<"
  RANGLE = 0x3E, // ">"
  A = 0x41,
  B = 0x42,
  C = 0x43,
  D = 0x44,
  E = 0x45,
  F = 0x46,
  G = 0x47,
  H = 0x48,
  I = 0x49,
  J = 0x4A,
  K = 0x4B,
  L = 0x4C,
  M = 0x4D,
  N = 0x4E,
  O = 0x4F,
  P = 0x50,
  Q = 0x51,
  R = 0x52,
  S = 0x53,
  T = 0x54,
  U = 0x55,
  V = 0x56,
  W = 0x57,
  X = 0x58,
  Z = 0x5A,
  LBRACK = 0x5B, // "["
  RBRACK = 0x5D, // "]"
  a = 0x61,
  b = 0x62,
  c = 0x63,
  d = 0x64,
  e = 0x65,
  f = 0x66,
  g = 0x67,
  h = 0x68,
  i = 0x69,
  j = 0x6A,
  k = 0x6B,
  l = 0x6C,
  m = 0x6D,
  n = 0x6E,
  o = 0x6F,
  p = 0x70,
  q = 0x71,
  r = 0x72,
  s = 0x73,
  t = 0x74,
  u = 0x75,
  v = 0x76,
  w = 0x77,
  x = 0x78,
  y = 0x79,
  z = 0x7A,
  LBRACE = 0x7B,
  PIPE = 0x7C,
  RBRACE = 0x7D,
}

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
    _DIGITS = <usize>(malloc(100 * 4))
    store<u32>(_DIGITS, 0x00300030, 0 * 4)
    store<u32>(_DIGITS, 0x00310030, 1 * 4)
    store<u32>(_DIGITS, 0x00320030, 2 * 4)
    store<u32>(_DIGITS, 0x00330030, 3 * 4)
    store<u32>(_DIGITS, 0x00340030, 4 * 4)
    store<u32>(_DIGITS, 0x00350030, 5 * 4)
    store<u32>(_DIGITS, 0x00360030, 6 * 4)
    store<u32>(_DIGITS, 0x00370030, 7 * 4)
    store<u32>(_DIGITS, 0x00380030, 8 * 4)
    store<u32>(_DIGITS, 0x00390030, 9 * 4)
    store<u32>(_DIGITS, 0x00300031, 10 * 4)
    store<u32>(_DIGITS, 0x00310031, 11 * 4)
    store<u32>(_DIGITS, 0x00320031, 12 * 4)
    store<u32>(_DIGITS, 0x00330031, 13 * 4)
    store<u32>(_DIGITS, 0x00340031, 14 * 4)
    store<u32>(_DIGITS, 0x00350031, 15 * 4)
    store<u32>(_DIGITS, 0x00360031, 16 * 4)
    store<u32>(_DIGITS, 0x00370031, 17 * 4)
    store<u32>(_DIGITS, 0x00380031, 18 * 4)
    store<u32>(_DIGITS, 0x00390031, 19 * 4)
    store<u32>(_DIGITS, 0x00300032, 20 * 4)
    store<u32>(_DIGITS, 0x00310032, 21 * 4)
    store<u32>(_DIGITS, 0x00320032, 22 * 4)
    store<u32>(_DIGITS, 0x00330032, 23 * 4)
    store<u32>(_DIGITS, 0x00340032, 24 * 4)
    store<u32>(_DIGITS, 0x00350032, 25 * 4)
    store<u32>(_DIGITS, 0x00360032, 26 * 4)
    store<u32>(_DIGITS, 0x00370032, 27 * 4)
    store<u32>(_DIGITS, 0x00380032, 28 * 4)
    store<u32>(_DIGITS, 0x00390032, 29 * 4)
    store<u32>(_DIGITS, 0x00300033, 30 * 4)
    store<u32>(_DIGITS, 0x00310033, 31 * 4)
    store<u32>(_DIGITS, 0x00320033, 32 * 4)
    store<u32>(_DIGITS, 0x00330033, 33 * 4)
    store<u32>(_DIGITS, 0x00340033, 34 * 4)
    store<u32>(_DIGITS, 0x00350033, 35 * 4)
    store<u32>(_DIGITS, 0x00360033, 36 * 4)
    store<u32>(_DIGITS, 0x00370033, 37 * 4)
    store<u32>(_DIGITS, 0x00380033, 38 * 4)
    store<u32>(_DIGITS, 0x00390033, 39 * 4)
    store<u32>(_DIGITS, 0x00300034, 40 * 4)
    store<u32>(_DIGITS, 0x00310034, 41 * 4)
    store<u32>(_DIGITS, 0x00320034, 42 * 4)
    store<u32>(_DIGITS, 0x00330034, 43 * 4)
    store<u32>(_DIGITS, 0x00340034, 44 * 4)
    store<u32>(_DIGITS, 0x00350034, 45 * 4)
    store<u32>(_DIGITS, 0x00360034, 46 * 4)
    store<u32>(_DIGITS, 0x00370034, 47 * 4)
    store<u32>(_DIGITS, 0x00380034, 48 * 4)
    store<u32>(_DIGITS, 0x00390034, 49 * 4)
    store<u32>(_DIGITS, 0x00300035, 50 * 4)
    store<u32>(_DIGITS, 0x00310035, 51 * 4)
    store<u32>(_DIGITS, 0x00320035, 52 * 4)
    store<u32>(_DIGITS, 0x00330035, 53 * 4)
    store<u32>(_DIGITS, 0x00340035, 54 * 4)
    store<u32>(_DIGITS, 0x00350035, 55 * 4)
    store<u32>(_DIGITS, 0x00360035, 56 * 4)
    store<u32>(_DIGITS, 0x00370035, 57 * 4)
    store<u32>(_DIGITS, 0x00380035, 58 * 4)
    store<u32>(_DIGITS, 0x00390035, 59 * 4)
    store<u32>(_DIGITS, 0x00300036, 60 * 4)
    store<u32>(_DIGITS, 0x00310036, 61 * 4)
    store<u32>(_DIGITS, 0x00320036, 62 * 4)
    store<u32>(_DIGITS, 0x00330036, 63 * 4)
    store<u32>(_DIGITS, 0x00340036, 64 * 4)
    store<u32>(_DIGITS, 0x00350036, 65 * 4)
    store<u32>(_DIGITS, 0x00360036, 66 * 4)
    store<u32>(_DIGITS, 0x00370036, 67 * 4)
    store<u32>(_DIGITS, 0x00380036, 68 * 4)
    store<u32>(_DIGITS, 0x00390036, 69 * 4)
    store<u32>(_DIGITS, 0x00300037, 70 * 4)
    store<u32>(_DIGITS, 0x00310037, 71 * 4)
    store<u32>(_DIGITS, 0x00320037, 72 * 4)
    store<u32>(_DIGITS, 0x00330037, 73 * 4)
    store<u32>(_DIGITS, 0x00340037, 74 * 4)
    store<u32>(_DIGITS, 0x00350037, 75 * 4)
    store<u32>(_DIGITS, 0x00360037, 76 * 4)
    store<u32>(_DIGITS, 0x00370037, 77 * 4)
    store<u32>(_DIGITS, 0x00380037, 78 * 4)
    store<u32>(_DIGITS, 0x00390037, 79 * 4)
    store<u32>(_DIGITS, 0x00300038, 80 * 4)
    store<u32>(_DIGITS, 0x00310038, 81 * 4)
    store<u32>(_DIGITS, 0x00320038, 82 * 4)
    store<u32>(_DIGITS, 0x00330038, 83 * 4)
    store<u32>(_DIGITS, 0x00340038, 84 * 4)
    store<u32>(_DIGITS, 0x00350038, 85 * 4)
    store<u32>(_DIGITS, 0x00360038, 86 * 4)
    store<u32>(_DIGITS, 0x00370038, 87 * 4)
    store<u32>(_DIGITS, 0x00380038, 88 * 4)
    store<u32>(_DIGITS, 0x00390038, 89 * 4)
    store<u32>(_DIGITS, 0x00300039, 90 * 4)
    store<u32>(_DIGITS, 0x00310039, 91 * 4)
    store<u32>(_DIGITS, 0x00320039, 92 * 4)
    store<u32>(_DIGITS, 0x00330039, 93 * 4)
    store<u32>(_DIGITS, 0x00340039, 94 * 4)
    store<u32>(_DIGITS, 0x00350039, 95 * 4)
    store<u32>(_DIGITS, 0x00360039, 96 * 4)
    store<u32>(_DIGITS, 0x00370039, 97 * 4)
    store<u32>(_DIGITS, 0x00380039, 98 * 4)
    store<u32>(_DIGITS, 0x00390039, 99 * 4)
  }
  return _DIGITS
}

// Lookup table for pairwise char codes in range [0x00-0xFF]
// @ts-ignore: decorator
let _HEX_DIGITS: usize = -1

function get_HEX_DIGITS(): usize {
  if (_HEX_DIGITS == -1) {
    _HEX_DIGITS = <usize>(malloc(256 << 2))
    store<u32>(_HEX_DIGITS, 0x00300030, 0 * 4) // 00
    store<u32>(_HEX_DIGITS, 0x00310030, 1 * 4) // 01
    store<u32>(_HEX_DIGITS, 0x00320030, 2 * 4)
    store<u32>(_HEX_DIGITS, 0x00330030, 3 * 4)
    store<u32>(_HEX_DIGITS, 0x00340030, 4 * 4)
    store<u32>(_HEX_DIGITS, 0x00350030, 5 * 4)
    store<u32>(_HEX_DIGITS, 0x00360030, 6 * 4)
    store<u32>(_HEX_DIGITS, 0x00370030, 7 * 4)
    store<u32>(_HEX_DIGITS, 0x00380030, 8 * 4)
    store<u32>(_HEX_DIGITS, 0x00390030, 9 * 4) // 09
    store<u32>(_HEX_DIGITS, 0x00610030, 10 * 4) // 0a
    store<u32>(_HEX_DIGITS, 0x00620030, 11 * 4)
    store<u32>(_HEX_DIGITS, 0x00630030, 12 * 4)
    store<u32>(_HEX_DIGITS, 0x00640030, 13 * 4)
    store<u32>(_HEX_DIGITS, 0x00650030, 14 * 4)
    store<u32>(_HEX_DIGITS, 0x00660030, 15 * 4) // 0f

    store<u32>(_HEX_DIGITS, 0x00300031, 16 * 4) // 10
    store<u32>(_HEX_DIGITS, 0x00310031, 17 * 4)
    store<u32>(_HEX_DIGITS, 0x00320031, 18 * 4)
    store<u32>(_HEX_DIGITS, 0x00330031, 19 * 4)
    store<u32>(_HEX_DIGITS, 0x00340031, 20 * 4)
    store<u32>(_HEX_DIGITS, 0x00350031, 21 * 4)
    store<u32>(_HEX_DIGITS, 0x00360031, 22 * 4)
    store<u32>(_HEX_DIGITS, 0x00370031, 23 * 4)
    store<u32>(_HEX_DIGITS, 0x00380031, 24 * 4)
    store<u32>(_HEX_DIGITS, 0x00390031, 25 * 4)
    store<u32>(_HEX_DIGITS, 0x00610031, 26 * 4)
    store<u32>(_HEX_DIGITS, 0x00620031, 27 * 4)
    store<u32>(_HEX_DIGITS, 0x00630031, 28 * 4)
    store<u32>(_HEX_DIGITS, 0x00640031, 29 * 4)
    store<u32>(_HEX_DIGITS, 0x00650031, 30 * 4)
    store<u32>(_HEX_DIGITS, 0x00660031, 31 * 4) // 1f

    store<u32>(_HEX_DIGITS, 0x00300032, 32 * 4) // 20
    store<u32>(_HEX_DIGITS, 0x00310032, 33 * 4)
    store<u32>(_HEX_DIGITS, 0x00320032, 34 * 4)
    store<u32>(_HEX_DIGITS, 0x00330032, 35 * 4)
    store<u32>(_HEX_DIGITS, 0x00340032, 36 * 4)
    store<u32>(_HEX_DIGITS, 0x00350032, 37 * 4)
    store<u32>(_HEX_DIGITS, 0x00360032, 38 * 4)
    store<u32>(_HEX_DIGITS, 0x00370032, 39 * 4)
    store<u32>(_HEX_DIGITS, 0x00380032, 40 * 4)
    store<u32>(_HEX_DIGITS, 0x00390032, 41 * 4)
    store<u32>(_HEX_DIGITS, 0x00610032, 42 * 4)
    store<u32>(_HEX_DIGITS, 0x00620032, 43 * 4)
    store<u32>(_HEX_DIGITS, 0x00630032, 44 * 4)
    store<u32>(_HEX_DIGITS, 0x00640032, 45 * 4)
    store<u32>(_HEX_DIGITS, 0x00650032, 46 * 4)
    store<u32>(_HEX_DIGITS, 0x00660032, 47 * 4) // 2f

    store<u32>(_HEX_DIGITS, 0x00300033, 48 * 4) // 30
    store<u32>(_HEX_DIGITS, 0x00310033, 49 * 4)
    store<u32>(_HEX_DIGITS, 0x00320033, 50 * 4)
    store<u32>(_HEX_DIGITS, 0x00330033, 51 * 4)
    store<u32>(_HEX_DIGITS, 0x00340033, 52 * 4)
    store<u32>(_HEX_DIGITS, 0x00350033, 53 * 4)
    store<u32>(_HEX_DIGITS, 0x00360033, 54 * 4)
    store<u32>(_HEX_DIGITS, 0x00370033, 55 * 4)
    store<u32>(_HEX_DIGITS, 0x00380033, 56 * 4)
    store<u32>(_HEX_DIGITS, 0x00390033, 57 * 4)
    store<u32>(_HEX_DIGITS, 0x00610033, 58 * 4)
    store<u32>(_HEX_DIGITS, 0x00620033, 59 * 4)
    store<u32>(_HEX_DIGITS, 0x00630033, 60 * 4)
    store<u32>(_HEX_DIGITS, 0x00640033, 61 * 4)
    store<u32>(_HEX_DIGITS, 0x00650033, 62 * 4)
    store<u32>(_HEX_DIGITS, 0x00660033, 63 * 4) // 3f

    store<u32>(_HEX_DIGITS, 0x00300034, 64 * 4) // 40
    store<u32>(_HEX_DIGITS, 0x00310034, 65 * 4)
    store<u32>(_HEX_DIGITS, 0x00320034, 66 * 4)
    store<u32>(_HEX_DIGITS, 0x00330034, 67 * 4)
    store<u32>(_HEX_DIGITS, 0x00340034, 68 * 4)
    store<u32>(_HEX_DIGITS, 0x00350034, 69 * 4)
    store<u32>(_HEX_DIGITS, 0x00360034, 70 * 4)
    store<u32>(_HEX_DIGITS, 0x00370034, 71 * 4)
    store<u32>(_HEX_DIGITS, 0x00380034, 72 * 4)
    store<u32>(_HEX_DIGITS, 0x00390034, 73 * 4)
    store<u32>(_HEX_DIGITS, 0x00610034, 74 * 4)
    store<u32>(_HEX_DIGITS, 0x00620034, 75 * 4)
    store<u32>(_HEX_DIGITS, 0x00630034, 76 * 4)
    store<u32>(_HEX_DIGITS, 0x00640034, 77 * 4)
    store<u32>(_HEX_DIGITS, 0x00650034, 78 * 4)
    store<u32>(_HEX_DIGITS, 0x00660034, 79 * 4) // 4f

    store<u32>(_HEX_DIGITS, 0x00300035, 80 * 4) // 50
    store<u32>(_HEX_DIGITS, 0x00310035, 81 * 4)
    store<u32>(_HEX_DIGITS, 0x00320035, 82 * 4)
    store<u32>(_HEX_DIGITS, 0x00330035, 83 * 4)
    store<u32>(_HEX_DIGITS, 0x00340035, 84 * 4)
    store<u32>(_HEX_DIGITS, 0x00350035, 85 * 4)
    store<u32>(_HEX_DIGITS, 0x00360035, 86 * 4)
    store<u32>(_HEX_DIGITS, 0x00370035, 87 * 4)
    store<u32>(_HEX_DIGITS, 0x00380035, 88 * 4)
    store<u32>(_HEX_DIGITS, 0x00390035, 89 * 4)
    store<u32>(_HEX_DIGITS, 0x00610035, 90 * 4)
    store<u32>(_HEX_DIGITS, 0x00620035, 91 * 4)
    store<u32>(_HEX_DIGITS, 0x00630035, 92 * 4)
    store<u32>(_HEX_DIGITS, 0x00640035, 93 * 4)
    store<u32>(_HEX_DIGITS, 0x00650035, 94 * 4)
    store<u32>(_HEX_DIGITS, 0x00660035, 95 * 4) // 5f

    store<u32>(_HEX_DIGITS, 0x00300036, 96 * 4) // 60
    store<u32>(_HEX_DIGITS, 0x00310036, 97 * 4)
    store<u32>(_HEX_DIGITS, 0x00320036, 98 * 4)
    store<u32>(_HEX_DIGITS, 0x00330036, 99 * 4)
    store<u32>(_HEX_DIGITS, 0x00340036, 100 * 4)
    store<u32>(_HEX_DIGITS, 0x00350036, 101 * 4)
    store<u32>(_HEX_DIGITS, 0x00360036, 102 * 4)
    store<u32>(_HEX_DIGITS, 0x00370036, 103 * 4)
    store<u32>(_HEX_DIGITS, 0x00380036, 104 * 4)
    store<u32>(_HEX_DIGITS, 0x00390036, 105 * 4)
    store<u32>(_HEX_DIGITS, 0x00610036, 106 * 4)
    store<u32>(_HEX_DIGITS, 0x00620036, 107 * 4)
    store<u32>(_HEX_DIGITS, 0x00630036, 108 * 4)
    store<u32>(_HEX_DIGITS, 0x00640036, 109 * 4)
    store<u32>(_HEX_DIGITS, 0x00650036, 110 * 4)
    store<u32>(_HEX_DIGITS, 0x00660036, 111 * 4) // 6f

    store<u32>(_HEX_DIGITS, 0x00300037, 112 * 4) // 70
    store<u32>(_HEX_DIGITS, 0x00310037, 113 * 4)
    store<u32>(_HEX_DIGITS, 0x00320037, 114 * 4)
    store<u32>(_HEX_DIGITS, 0x00330037, 115 * 4)
    store<u32>(_HEX_DIGITS, 0x00340037, 116 * 4)
    store<u32>(_HEX_DIGITS, 0x00350037, 117 * 4)
    store<u32>(_HEX_DIGITS, 0x00360037, 118 * 4)
    store<u32>(_HEX_DIGITS, 0x00370037, 119 * 4)
    store<u32>(_HEX_DIGITS, 0x00380037, 120 * 4)
    store<u32>(_HEX_DIGITS, 0x00390037, 121 * 4)
    store<u32>(_HEX_DIGITS, 0x00610037, 122 * 4)
    store<u32>(_HEX_DIGITS, 0x00620037, 123 * 4)
    store<u32>(_HEX_DIGITS, 0x00630037, 124 * 4)
    store<u32>(_HEX_DIGITS, 0x00640037, 125 * 4)
    store<u32>(_HEX_DIGITS, 0x00650037, 126 * 4)
    store<u32>(_HEX_DIGITS, 0x00660037, 127 * 4) // 7f

    store<u32>(_HEX_DIGITS, 0x00300038, 128 * 4) // 80
    store<u32>(_HEX_DIGITS, 0x00310038, 129 * 4)
    store<u32>(_HEX_DIGITS, 0x00320038, 130 * 4)
    store<u32>(_HEX_DIGITS, 0x00330038, 131 * 4)
    store<u32>(_HEX_DIGITS, 0x00340038, 132 * 4)
    store<u32>(_HEX_DIGITS, 0x00350038, 133 * 4)
    store<u32>(_HEX_DIGITS, 0x00360038, 134 * 4)
    store<u32>(_HEX_DIGITS, 0x00370038, 135 * 4)
    store<u32>(_HEX_DIGITS, 0x00380038, 136 * 4)
    store<u32>(_HEX_DIGITS, 0x00390038, 137 * 4)
    store<u32>(_HEX_DIGITS, 0x00610038, 138 * 4)
    store<u32>(_HEX_DIGITS, 0x00620038, 139 * 4)
    store<u32>(_HEX_DIGITS, 0x00630038, 140 * 4)
    store<u32>(_HEX_DIGITS, 0x00640038, 141 * 4)
    store<u32>(_HEX_DIGITS, 0x00650038, 142 * 4)
    store<u32>(_HEX_DIGITS, 0x00660038, 143 * 4) // 8f

    store<u32>(_HEX_DIGITS, 0x00300039, 144 * 4) // 90
    store<u32>(_HEX_DIGITS, 0x00310039, 145 * 4)
    store<u32>(_HEX_DIGITS, 0x00320039, 146 * 4)
    store<u32>(_HEX_DIGITS, 0x00330039, 147 * 4)
    store<u32>(_HEX_DIGITS, 0x00340039, 148 * 4)
    store<u32>(_HEX_DIGITS, 0x00350039, 149 * 4)
    store<u32>(_HEX_DIGITS, 0x00360039, 150 * 4)
    store<u32>(_HEX_DIGITS, 0x00370039, 151 * 4)
    store<u32>(_HEX_DIGITS, 0x00380039, 152 * 4)
    store<u32>(_HEX_DIGITS, 0x00390039, 153 * 4)
    store<u32>(_HEX_DIGITS, 0x00610039, 154 * 4)
    store<u32>(_HEX_DIGITS, 0x00620039, 155 * 4)
    store<u32>(_HEX_DIGITS, 0x00630039, 156 * 4)
    store<u32>(_HEX_DIGITS, 0x00640039, 157 * 4)
    store<u32>(_HEX_DIGITS, 0x00650039, 158 * 4)
    store<u32>(_HEX_DIGITS, 0x00660039, 159 * 4) // 9f

    store<u32>(_HEX_DIGITS, 0x00300061, 160 * 4) // a0
    store<u32>(_HEX_DIGITS, 0x00310061, 161 * 4)
    store<u32>(_HEX_DIGITS, 0x00320061, 162 * 4)
    store<u32>(_HEX_DIGITS, 0x00330061, 163 * 4)
    store<u32>(_HEX_DIGITS, 0x00340061, 164 * 4)
    store<u32>(_HEX_DIGITS, 0x00350061, 165 * 4)
    store<u32>(_HEX_DIGITS, 0x00360061, 166 * 4)
    store<u32>(_HEX_DIGITS, 0x00370061, 167 * 4)
    store<u32>(_HEX_DIGITS, 0x00380061, 168 * 4)
    store<u32>(_HEX_DIGITS, 0x00390061, 169 * 4)
    store<u32>(_HEX_DIGITS, 0x00610061, 170 * 4)
    store<u32>(_HEX_DIGITS, 0x00620061, 171 * 4)
    store<u32>(_HEX_DIGITS, 0x00630061, 172 * 4)
    store<u32>(_HEX_DIGITS, 0x00640061, 173 * 4)
    store<u32>(_HEX_DIGITS, 0x00650061, 174 * 4)
    store<u32>(_HEX_DIGITS, 0x00660061, 175 * 4) // af

    store<u32>(_HEX_DIGITS, 0x00300062, 176 * 4) // b0
    store<u32>(_HEX_DIGITS, 0x00310062, 177 * 4)
    store<u32>(_HEX_DIGITS, 0x00320062, 178 * 4)
    store<u32>(_HEX_DIGITS, 0x00330062, 179 * 4)
    store<u32>(_HEX_DIGITS, 0x00340062, 180 * 4)
    store<u32>(_HEX_DIGITS, 0x00350062, 181 * 4)
    store<u32>(_HEX_DIGITS, 0x00360062, 182 * 4)
    store<u32>(_HEX_DIGITS, 0x00370062, 183 * 4)
    store<u32>(_HEX_DIGITS, 0x00380062, 184 * 4)
    store<u32>(_HEX_DIGITS, 0x00390062, 185 * 4)
    store<u32>(_HEX_DIGITS, 0x00610062, 186 * 4)
    store<u32>(_HEX_DIGITS, 0x00620062, 187 * 4)
    store<u32>(_HEX_DIGITS, 0x00630062, 188 * 4)
    store<u32>(_HEX_DIGITS, 0x00640062, 189 * 4)
    store<u32>(_HEX_DIGITS, 0x00650062, 190 * 4)
    store<u32>(_HEX_DIGITS, 0x00660062, 191 * 4) // bf

    store<u32>(_HEX_DIGITS, 0x00300063, 192 * 4) // c0
    store<u32>(_HEX_DIGITS, 0x00310063, 193 * 4)
    store<u32>(_HEX_DIGITS, 0x00320063, 194 * 4)
    store<u32>(_HEX_DIGITS, 0x00330063, 195 * 4)
    store<u32>(_HEX_DIGITS, 0x00340063, 196 * 4)
    store<u32>(_HEX_DIGITS, 0x00350063, 197 * 4)
    store<u32>(_HEX_DIGITS, 0x00360063, 198 * 4)
    store<u32>(_HEX_DIGITS, 0x00370063, 199 * 4)
    store<u32>(_HEX_DIGITS, 0x00380063, 200 * 4)
    store<u32>(_HEX_DIGITS, 0x00390063, 201 * 4)
    store<u32>(_HEX_DIGITS, 0x00610063, 202 * 4)
    store<u32>(_HEX_DIGITS, 0x00620063, 203 * 4)
    store<u32>(_HEX_DIGITS, 0x00630063, 204 * 4)
    store<u32>(_HEX_DIGITS, 0x00640063, 205 * 4)
    store<u32>(_HEX_DIGITS, 0x00650063, 206 * 4)
    store<u32>(_HEX_DIGITS, 0x00660063, 207 * 4) // cf

    store<u32>(_HEX_DIGITS, 0x00300064, 208 * 4) // d0
    store<u32>(_HEX_DIGITS, 0x00310064, 209 * 4)
    store<u32>(_HEX_DIGITS, 0x00320064, 210 * 4)
    store<u32>(_HEX_DIGITS, 0x00330064, 211 * 4)
    store<u32>(_HEX_DIGITS, 0x00340064, 212 * 4)
    store<u32>(_HEX_DIGITS, 0x00350064, 213 * 4)
    store<u32>(_HEX_DIGITS, 0x00360064, 214 * 4)
    store<u32>(_HEX_DIGITS, 0x00370064, 215 * 4)
    store<u32>(_HEX_DIGITS, 0x00380064, 216 * 4)
    store<u32>(_HEX_DIGITS, 0x00390064, 217 * 4)
    store<u32>(_HEX_DIGITS, 0x00610064, 218 * 4)
    store<u32>(_HEX_DIGITS, 0x00620064, 219 * 4)
    store<u32>(_HEX_DIGITS, 0x00630064, 220 * 4)
    store<u32>(_HEX_DIGITS, 0x00640064, 221 * 4)
    store<u32>(_HEX_DIGITS, 0x00650064, 222 * 4)
    store<u32>(_HEX_DIGITS, 0x00660064, 223 * 4) // df

    store<u32>(_HEX_DIGITS, 0x00300065, 224 * 4) // e0
    store<u32>(_HEX_DIGITS, 0x00310065, 225 * 4)
    store<u32>(_HEX_DIGITS, 0x00320065, 226 * 4)
    store<u32>(_HEX_DIGITS, 0x00330065, 227 * 4)
    store<u32>(_HEX_DIGITS, 0x00340065, 228 * 4)
    store<u32>(_HEX_DIGITS, 0x00350065, 229 * 4)
    store<u32>(_HEX_DIGITS, 0x00360065, 230 * 4)
    store<u32>(_HEX_DIGITS, 0x00370065, 231 * 4)
    store<u32>(_HEX_DIGITS, 0x00380065, 232 * 4)
    store<u32>(_HEX_DIGITS, 0x00390065, 233 * 4)
    store<u32>(_HEX_DIGITS, 0x00610065, 234 * 4)
    store<u32>(_HEX_DIGITS, 0x00620065, 235 * 4)
    store<u32>(_HEX_DIGITS, 0x00630065, 236 * 4)
    store<u32>(_HEX_DIGITS, 0x00640065, 237 * 4)
    store<u32>(_HEX_DIGITS, 0x00650065, 238 * 4)
    store<u32>(_HEX_DIGITS, 0x00660065, 239 * 4) // ef

    store<u32>(_HEX_DIGITS, 0x00300066, 240 * 4) // f0
    store<u32>(_HEX_DIGITS, 0x00310066, 241 * 4)
    store<u32>(_HEX_DIGITS, 0x00320066, 242 * 4)
    store<u32>(_HEX_DIGITS, 0x00330066, 243 * 4)
    store<u32>(_HEX_DIGITS, 0x00340066, 244 * 4)
    store<u32>(_HEX_DIGITS, 0x00350066, 245 * 4)
    store<u32>(_HEX_DIGITS, 0x00360066, 246 * 4)
    store<u32>(_HEX_DIGITS, 0x00370066, 247 * 4)
    store<u32>(_HEX_DIGITS, 0x00380066, 248 * 4)
    store<u32>(_HEX_DIGITS, 0x00390066, 249 * 4)
    store<u32>(_HEX_DIGITS, 0x00610066, 250 * 4)
    store<u32>(_HEX_DIGITS, 0x00620066, 251 * 4)
    store<u32>(_HEX_DIGITS, 0x00630066, 252 * 4)
    store<u32>(_HEX_DIGITS, 0x00640066, 253 * 4)
    store<u32>(_HEX_DIGITS, 0x00650066, 254 * 4)
    store<u32>(_HEX_DIGITS, 0x00660066, 255 * 4) // ff
  }
  return _HEX_DIGITS
}

// @ts-ignore: decorator
let _ANY_DIGITS: usize = -1;

function get_ANY_DIGITS(): usize {
  if (_ANY_DIGITS == -1) {
    _ANY_DIGITS = <usize>(malloc(36 << 1))
    store<u16>(_ANY_DIGITS, CharCode._0)
    store<u16>(_ANY_DIGITS, CharCode._1, 2 * 1)
    store<u16>(_ANY_DIGITS, CharCode._2, 2 * 2)
    store<u16>(_ANY_DIGITS, CharCode._3, 2 * 3)
    store<u16>(_ANY_DIGITS, CharCode._4, 2 * 4)
    store<u16>(_ANY_DIGITS, CharCode._5, 2 * 5)
    store<u16>(_ANY_DIGITS, CharCode._6, 2 * 6)
    store<u16>(_ANY_DIGITS, CharCode._7, 2 * 7)
    store<u16>(_ANY_DIGITS, CharCode._8, 2 * 8)
    store<u16>(_ANY_DIGITS, CharCode._9, 2 * 9)
    store<u16>(_ANY_DIGITS, CharCode.a, 2 * 10)
    store<u16>(_ANY_DIGITS, CharCode.b, 2 * 11)
    store<u16>(_ANY_DIGITS, CharCode.c, 2 * 12)
    store<u16>(_ANY_DIGITS, CharCode.d, 2 * 13)
    store<u16>(_ANY_DIGITS, CharCode.e, 2 * 14)
    store<u16>(_ANY_DIGITS, CharCode.f, 2 * 15)
    store<u16>(_ANY_DIGITS, CharCode.g, 2 * 16)
    store<u16>(_ANY_DIGITS, CharCode.h, 2 * 17)
    store<u16>(_ANY_DIGITS, CharCode.i, 2 * 18)
    store<u16>(_ANY_DIGITS, CharCode.j, 2 * 19)
    store<u16>(_ANY_DIGITS, CharCode.k, 2 * 20)
    store<u16>(_ANY_DIGITS, CharCode.l, 2 * 21)
    store<u16>(_ANY_DIGITS, CharCode.m, 2 * 22)
    store<u16>(_ANY_DIGITS, CharCode.n, 2 * 23)
    store<u16>(_ANY_DIGITS, CharCode.o, 2 * 24)
    store<u16>(_ANY_DIGITS, CharCode.p, 2 * 25)
    store<u16>(_ANY_DIGITS, CharCode.q, 2 * 26)
    store<u16>(_ANY_DIGITS, CharCode.r, 2 * 27)
    store<u16>(_ANY_DIGITS, CharCode.s, 2 * 28)
    store<u16>(_ANY_DIGITS, CharCode.t, 2 * 29)
    store<u16>(_ANY_DIGITS, CharCode.u, 2 * 30)
    store<u16>(_ANY_DIGITS, CharCode.v, 2 * 31)
    store<u16>(_ANY_DIGITS, CharCode.w, 2 * 32)
    store<u16>(_ANY_DIGITS, CharCode.x, 2 * 33)
    store<u16>(_ANY_DIGITS, CharCode.y, 2 * 34)
    store<u16>(_ANY_DIGITS, CharCode.z, 2 * 35)
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

    let digits1 = <u64>load<u32>(get_DIGITS() + (<usize>d1 << alignof<u32>()));
    let digits2 = <u64>load<u32>(get_DIGITS() + (<usize>d2 << alignof<u32>()));

    offset -= 4;
    store<u64>(buffer + (offset << 1), digits1 | (digits2 << 32));
  }

  if (num >= 100) {
    let t  = num / 100;
    let d1 = num % 100;
    num = t;
    offset -= 2;
    let digits = load<u32>(get_DIGITS() + (<usize>d1 << alignof<u32>()));
    store<u32>(buffer + (offset << 1), digits);
  }

  if (num >= 10) {
    offset -= 2;
    let digits = load<u32>(get_DIGITS() + (<usize>num << alignof<u32>()));
    store<u32>(buffer + (offset << 1), digits);
  } else {
    offset -= 1;
    let digit = CharCode._0 + num;
    store<u16>(buffer + (offset << 1), digit);
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

    let digits1 = <u64>load<u32>(get_DIGITS() + (<usize>c1 << alignof<u32>()));
    let digits2 = <u64>load<u32>(get_DIGITS() + (<usize>c2 << alignof<u32>()));

    offset -= 4;
    store<u64>(buffer + (offset << 1), digits1 | (digits2 << 32));

    digits1 = <u64>load<u32>(get_DIGITS() + (<usize>b1 << alignof<u32>()));
    digits2 = <u64>load<u32>(get_DIGITS() + (<usize>b2 << alignof<u32>()));

    offset -= 4;
    store<u64>(buffer + (offset << 1), digits1 | (digits2 << 32));
  }

  utoa32_dec_lut(buffer, <u32>num, offset);
}

function utoa_hex_lut(buffer: usize, num: u64, offset: usize): void {
  const lut = get_HEX_DIGITS()
  while (offset >= 2) {
    offset -= 2;
    store<u32>(
      buffer + (offset << 1),
      load<u32>(lut + ((<usize>num & 0xFF) << alignof<u32>()))
    );
    num >>= 8;
  }
  if (offset & 1) {
    store<u16>(buffer, load<u16>(lut + (<usize>num << 6)));
  }
}

function utoa_dec_simple<T extends number>(buffer: usize, num: T, offset: usize): void {
  do {
    let t = num / 10;
    let r = <u32>(num % 10);
    num = changetype<T>(t);
    offset--;
    store<u16>(buffer + (offset << 1), CharCode._0 + r);
  } while (num);
}

function utoa_hex_simple<T extends number>(buffer: usize, num: T, offset: usize): void {
  do {
    let d = num & 0x0F | CharCode._0;
    d += select<T>(<T>0x27, <T>0, d > <T>CharCode._9);
    offset--;
    store<u16>(buffer + (offset << 1), d);
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
      store<u16>(buffer + (offset << 1), load<u16>(lut + (usize(num & mask) << 1)));
      num >>= shift;
    } while (num);
  } else {
    do {
      offset--;
      let q = num / base;
      store<u16>(buffer + (offset << 1), load<u16>(lut + (usize(num - q * base) << 1)));
      num = q;
    } while (num);
  }
}

// This file produces JS (UTF-16) strings. We need to re-encode them as UTF-8 for Grain.
// Note that all of the characters used in the strings produced in this file are ASCII,
// so we can safely just halve the size of the string.
// [TODO] (#475) Optimization: Make the functions in this file directly produce UTF-8
function fixEncoding(utf16StringRaw: u32): u32 {
  let utf16String = utf16StringRaw & ~GRAIN_GENERIC_HEAP_TAG_TYPE
  let utf16Size = stringSize(utf16StringRaw)
  let utf8Size = utf16Size >> 1
  let ret = allocateString(utf8Size)
  String.UTF8.encodeUnsafe(<usize>(utf16String + 8), utf8Size, ret + 8, false)
  decRef(utf16StringRaw)
  return ret | GRAIN_GENERIC_HEAP_TAG_TYPE
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
    // (#475) Need to allocated twice the actual size for intermediate UTF-16 representation
    out = allocateString(decimals << 1);
    utoa32_dec_core(out + 8, value, decimals);
  } else if (radix == 16) {
    let decimals = (31 - clz(value) >> 2) + 1;
    // (#475) Need to allocated twice the actual size for intermediate UTF-16 representation
    out = allocateString(decimals << 1);
    utoa32_hex_core(out + 8, value, decimals);
  } else {
    let decimals = ulog_base(value, radix);
    // (#475) Need to allocated twice the actual size for intermediate UTF-16 representation
    out = allocateString(decimals << 1);
    utoa64_any_core(out + 8, value, decimals, radix);
  }
  return fixEncoding(out); // retains
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
    // (#475) Need to allocated twice the actual size for intermediate UTF-16 representation
    out = allocateString(decimals << 1);
    utoa32_dec_core(<usize>(out + 8), value, decimals);
  } else if (radix == 16) {
    let decimals = (31 - clz(value) >> 2) + 1 + sign;
    // (#475) Need to allocated twice the actual size for intermediate UTF-16 representation
    out = allocateString(decimals << 1);
    utoa32_hex_core(<usize>(out + 8), value, decimals);
  } else {
    let val32 = u32(value);
    let decimals = ulog_base(val32, radix) + sign;
    // (#475) Need to allocated twice the actual size for intermediate UTF-16 representation
    out = allocateString(decimals << 1);
    utoa64_any_core(<usize>(out + 8), val32, decimals, radix);
  }
  if (sign) store<u16>(<usize>(out + 8), CharCode.MINUS);
  let ret = fixEncoding(out); // retains
  return ret
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
      // (#475) Need to allocated twice the actual size for intermediate UTF-16 representation
      out = allocateString(decimals << 1);
      utoa32_dec_core(out + 8, val32, decimals);
    } else {
      let decimals = decimalCount64High(value);
      // (#475) Need to allocated twice the actual size for intermediate UTF-16 representation
      out = allocateString(decimals << 1);
      utoa64_dec_core(out + 8, value, decimals);
    }
  } else if (radix == 16) {
    let decimals = (63 - u32(clz(value)) >> 2) + 1;
    // (#475) Need to allocated twice the actual size for intermediate UTF-16 representation
    out = allocateString(decimals << 1);
    utoa64_hex_core(out + 8, value, decimals);
  } else {
    let decimals = ulog_base(value, radix);
    out = allocateString(decimals << 1);
    utoa64_any_core(out + 8, value, decimals, radix);
  }
  return fixEncoding(out); // retains
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
      // (#475) Need to allocated twice the actual size for intermediate UTF-16 representation
      out = allocateString(decimals << 1);
      utoa32_dec_core(out + 8, val32, decimals);
    } else {
      let decimals = decimalCount64High(value) + sign;
      // (#475) Need to allocated twice the actual size for intermediate UTF-16 representation
      out = allocateString(decimals << 1);
      utoa64_dec_core(out + 8, value, decimals);
    }
  } else if (radix == 16) {
    let decimals = (63 - u32(clz(value)) >> 2) + 1 + sign;
    // (#475) Need to allocated twice the actual size for intermediate UTF-16 representation
    out = allocateString(decimals << 1);
    utoa64_hex_core(out + 8, value, decimals);
  } else {
    let decimals = ulog_base(value, radix) + sign;
    // (#475) Need to allocated twice the actual size for intermediate UTF-16 representation
    out = allocateString(decimals << 1);
    utoa64_any_core(out + 8, value, decimals, radix);
  }
  if (sign) store<u16>(out + 8, CharCode.MINUS);
  return fixEncoding(out); // retains
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
  var lastp = buffer + ((len - 1) << 1);
  var digit = load<u16>(lastp);
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
  store<u16>(lastp, digit);
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

    if (d | len) store<u16>(buffer + (len++ << 1), CharCode._0 + <u16>d);

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
    if (d | len) store<u16>(buffer + (len++ << 1), CharCode._0 + <u16>d);

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
  store<u16>(buffer, <u16>select<u32>(CharCode.MINUS, CharCode.PLUS, sign));
  return decimals;
}

function prettify(buffer: usize, length: i32, k: i32): i32 {
  if (!k) {
    store<u32>(buffer + (length << 1), CharCode.DOT | (CharCode._0 << 16));
    return length + 2;
  }

  var kk = length + k;
  if (length <= kk && kk <= 21) {
    // 1234e7 -> 12340000000
    for (let i = length; i < kk; ++i) {
      store<u16>(buffer + (i << 1), CharCode._0);
    }
    store<u32>(buffer + (kk << 1), CharCode.DOT | (CharCode._0 << 16));
    return kk + 2;
  } else if (kk > 0 && kk <= 21) {
    // 1234e-2 -> 12.34
    let ptr = buffer + (kk << 1);
    memory.copy(
      ptr + 2,
      ptr,
      -k << 1
    );
    store<u16>(buffer + (kk << 1), CharCode.DOT);
    return length + 1;
  } else if (-6 < kk && kk <= 0) {
    // 1234e-6 -> 0.001234
    let offset = 2 - kk;
    memory.copy(
      buffer + (offset << 1),
      buffer,
      length << 1
    );
    store<u32>(buffer, CharCode._0 | (CharCode.DOT << 16));
    for (let i = 2; i < offset; ++i) {
      store<u16>(buffer + (i << 1), CharCode._0);
    }
    return length + offset;
  } else if (length == 1) {
    // 1e30
    store<u16>(buffer, CharCode.e, 2);
    length = genExponent(buffer + 4, kk - 1);
    return length + 2;
  } else {
    let len = length << 1;
    memory.copy(
      buffer + 4,
      buffer + 2,
      len - 2
    );
    store<u16>(buffer,       CharCode.DOT, 2);
    store<u16>(buffer + len, CharCode.e,   2);
    length += genExponent(buffer + len + 4, kk - 1);
    return length + 2;
  }
}

function dtoa_core(buffer: usize, value: f64): i32 {
  var sign = i32(value < 0);
  if (sign) {
    value = -value;
    store<u16>(buffer, CharCode.MINUS);
  }
  // assert(value > 0 && value <= 1.7976931348623157e308);
  var len = grisu2(value, buffer, sign);
  len = prettify(buffer + (sign << 1), len - sign, _K);
  return len + sign;
}

// @ts-ignore: decorator
let _dtoa_buf: usize = -1

function get_dtoa_buf(): usize {
  if (_dtoa_buf == -1) {
    _dtoa_buf = <usize>(malloc(MAX_DOUBLE_LENGTH << 1))
  }
  return _dtoa_buf
}

export function dtoa(value: f64): u32 {
  if (value == 0) {
    let ret = allocateString(3)
    store<u8>(ret, CharCode._0, 8)
    store<u8>(ret, CharCode.DOT, 8 + 1)
    store<u8>(ret, CharCode._0, 8 + 2)
    return ret | GRAIN_GENERIC_HEAP_TAG_TYPE
  } else if (!isFinite(value)) {
    if (isNaN(value)) {
      let ret = allocateString(3)
      store<u8>(ret, CharCode.N, 8)
      store<u8>(ret, CharCode.a, 8 + 1)
      store<u8>(ret, CharCode.N, 8 + 2)
      return ret | GRAIN_GENERIC_HEAP_TAG_TYPE
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
      return ret | GRAIN_GENERIC_HEAP_TAG_TYPE
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
      return ret | GRAIN_GENERIC_HEAP_TAG_TYPE
    }
  }
  var size = dtoa_core(get_dtoa_buf(), value);
  var result = allocateString(size);
  String.UTF8.encodeUnsafe(get_dtoa_buf(), size, result + 8, false)
  return result | GRAIN_GENERIC_HEAP_TAG_TYPE;
}


export function dtoa_buffered(buffer: usize, value: f64): u32 {
  if (value == 0) {
    store<u16>(buffer, CharCode._0);
    store<u16>(buffer, CharCode.DOT, 2);
    store<u16>(buffer, CharCode._0,  4);
    return 3;
  }
  if (!isFinite(value)) {
    if (isNaN(value)) {
      store<u16>(buffer, CharCode.N);
      store<u16>(buffer, CharCode.a, 2);
      store<u16>(buffer, CharCode.N, 4);
      return 3;
    } else {
      let sign = value < 0;
      if (sign) {
        store<u16>(buffer, CharCode.MINUS); // -
        buffer += 2;
      }
      store<u64>(buffer, 0x690066006E0049, 0); // ifnI
      store<u64>(buffer, 0x7900740069006E, 8); // ytin
      return 8 + u32(sign);
    }
  }
  return dtoa_core(buffer, value);
}
