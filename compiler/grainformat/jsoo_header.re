// This code is used to refill the mock stdin with the actual 0 file descriptor
Js_of_ocaml.Sys_js.set_channel_filler(stdin, () => {
  Js_of_ocaml.Js.to_string(
    Js_of_ocaml.Js.Unsafe.js_expr("require('fs').readFileSync(0, 'utf8')"),
  )
});
