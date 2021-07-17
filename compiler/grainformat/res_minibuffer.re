/*
   This code is taken directly from the Rescript project
   https://github.com/rescript-lang/syntax

   Original license reproduced below:

   MIT License

   Copyright (c) 2020 ReScript

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.


 */

type t = {
  mutable buffer: bytes,
  mutable position: int,
  mutable length: int,
};

let create = n => {
  let n =
    if (n < 1) {
      1;
    } else {
      n;
    };
  let s = ([@doesNotRaise] Bytes.create)(n);
  {buffer: s, position: 0, length: n};
};

let contents = b =>
  ([@doesNotRaise] Bytes.sub_string)(b.buffer, 0, b.position);

/* Can't be called directly, don't add to the interface */
let resize_internal = (b, more) => {
  let len = b.length;
  let new_len = ref(len);
  while (b.position + more > new_len^) {
    new_len := 2 * new_len^;
  };
  if (new_len^ > Sys.max_string_length) {
    if (b.position + more <= Sys.max_string_length) {
      new_len := Sys.max_string_length;
    };
  };
  let new_buffer = ([@doesNotRaise] Bytes.create)(new_len^);
  /* PR#6148: let's keep using [blit] rather than [unsafe_blit] in
     this tricky function that is slow anyway. */
  [@doesNotRaise] Bytes.blit(b.buffer, 0, new_buffer, 0, b.position);
  b.buffer = new_buffer;
  b.length = new_len^;
};

let add_char = (b, c) => {
  let pos = b.position;
  if (pos >= b.length) {
    resize_internal(b, 1);
  };
  Bytes.unsafe_set(b.buffer, pos, c);
  b.position = pos + 1;
};

let add_string = (b, s) => {
  let len = String.length(s);
  let new_position = b.position + len;
  if (new_position > b.length) {
    resize_internal(b, len);
  };
  [@doesNotRaise] Bytes.blit_string(s, 0, b.buffer, b.position, len);
  b.position = new_position;
};

/* adds newline and trims all preceding whitespace */
let flush_newline = b => {
  let position = ref(b.position);
  while (Bytes.unsafe_get(b.buffer, position^ - 1) == ' ' && position^ >= 0) {
    position := position^ - 1;
  };
  b.position = position^;
  add_char(b, '\n');
};
