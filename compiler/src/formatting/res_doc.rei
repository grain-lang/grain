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

type lineStyle =
  | Classic /* fits? -> replace with space */
  | Soft /* fits? -> replaced with nothing */
  | Hard /* always included, forces breaks in parents */
  /* always included, forces breaks in parents, but doesn't increase indentation
     use case: template literals, multiline string content */
  | Literal;

type t =
  | Nil
  | Text(string)
  | Concat(list(t))
  | Indent(t)
  | IfBreaks({
      yes: t,
      no: t,
      mutable broken: bool,
    }) /* when broken is true, treat as the yes branch */
  | LineSuffix(t)
  | LineBreak(lineStyle)
  | Group({
      mutable shouldBreak: bool,
      doc: t,
    })
  | CustomLayout(list(t))
  | BreakParent;

let nil: t;
let line: t;
let hardLine: t;
let softLine: t;
let literalLine: t;
let text: string => t;
let concat: list(t) => t;
let indent: t => t;
let ifBreaks: (t, t) => t;
let lineSuffix: t => t;
let group: t => t;
let breakableGroup: (~forceBreak: bool, t) => t;
/* `customLayout docs` will pick the layout that fits from `docs`.
 * This is a very expensive computation as every layout from the list
 * will be checked until one fits. */
let customLayout: list(t) => t;
let breakParent: t;
let join: (~sep: t, list(t)) => t;

let space: t;
let comma: t;
let dot: t;
let dotdot: t;
let dotdotdot: t;
let lessThan: t;
let greaterThan: t;
let lbrace: t;
let rbrace: t;
let lparen: t;
let rparen: t;
let lbracket: t;
let rbracket: t;
let question: t;
let tilde: t;
let equal: t;
let trailingComma: t;
[@live]
let doubleQuote: t;

/*
 * `willBreak doc` checks whether `doc` contains forced line breaks.
 * This is more or less a "workaround" to make the parent of a `customLayout` break.
 * Forced breaks are not propagated through `customLayout`; otherwise we would always
 * get the last layout the algorithm tries…
 * This might result into some weird layouts:
 *  [fn(x => {
 *     let _ = x
 *   }), fn(y => {
 *     let _ = y
 *   }), fn(z => {
 *     let _ = z
 *   })]
 *  The `[` and `]` would be a lot better broken out.
 *  Although the layout of `fn(x => {...})` is correct, we need to break its parent (the array).
 *  `willBreak` can be used in this scenario to check if the `fn…` contains any forced breaks.
 *  The consumer can then manually insert a `breakParent` doc, to manually propagate the
 *  force breaks from bottom to top.
 */
let willBreak: t => bool;

let willIndent: t => bool;

let toString: (~width: int, ~eol: Grain_utils.Fs_access.eol, t) => string;
[@live]
let debug: (~eol: Grain_utils.Fs_access.eol, t) => unit;
