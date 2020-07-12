# Grain strings

Grain uses Unicode, specifically UTF-8 for strings.

## Definitions

- _code point_ an atomic unit of information. Text is a sequence of code points.
- _code unit_ a unit of storage of an encoded code point.
- _grapheme_ a sequence of one or more code points displayed as a single
  graphical element.

## Encoding

Unicode is the universal character encoding. Each character has a code point.
Valid code points are in the ranges 0x0000...0xD7FF and 0xE000...0x10FFFF.
The gap between the ranges is for surrogate pairs.

Several schemes exist to encode Unicode. Grain uses UTF-8. The 8 indicates the
number of bits in a code unit. UTF-8 doesn't use surrogate pairs so you don't
have to worry about that gap between the ranges.

Code points are encoded in the following way. A single code point can be up to
four bytes.

```
    0xxxxxxx
    110xxxxx 10xxxxxx
    1110xxxx 10xxxxxx 10xxxxxx
    11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
```

In order to know how many code points a string contains you count the leading
bytes which begin with either 00, 01 or 11. This is given by the following
expressions in AssemblyScript.

```
    (byte & 0b11000000) !== 0b01000000
    or
    (byte & 0xC0) !== 0x80)
```

## Combining and normalization

Some graphemes are encoded by multiple code points. Some graphemes can be
encoded by either a single code point or multiple code points. For example,
__ñ__ is U+00F1, but it can also be encoded by combining __n__ U+0006E and tilde
 __◌̃__ U+0303.

Normalization is the process by which combining code points are reduced to
either single code points or put into canonical order. This is necessary for
searching and comparing strings. Strings in Grain are not normalized.

## JavaScript

JavaScript strings are not encoded in UTF-8.

JavaScript uses UCS-2 with unpaired surrogate codepoints. Each codepoint is
represented by two bytes. A surrograte pair is a pair of codepoints used to
represent a codepoint greater than U+FFFF. DOMString is the  same but with a
replacement character, U+FFFD, for unpaired surrogate  codepoints. A USVString
is the same but  without unpaired surrogate codepoints. A CSSOMString is the
same as a DOMString.

A JavaScript string containing one or more unpaired surrogate codepoints is not
a  valid UTF-16 string. USVString, DOMString and CSSOMString are valid UTF-16
strings. The encoder converts to USVString.
