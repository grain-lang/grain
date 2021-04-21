//Provides: caml_make_path
//Requires: caml_current_dir
//Requires: caml_jsstring_of_string
function caml_make_path(name) {
  name = caml_jsstring_of_string(name);
  // We needed to provide our own `caml_make_path` because it doesn't check
  // for Windows drives as root
  if (name.charCodeAt(0) != 47 && name.charCodeAt(1) != 58)
    name = caml_current_dir + name;
  var comp = name.split("/");
  var ncomp = []
  for (var i = 0; i < comp.length; i++) {
    switch (comp[i]) {
      case "..": if (ncomp.length > 1) ncomp.pop(); break;
      case ".": break;
      case "": if (ncomp.length == 0) ncomp.push(""); break;
      default: ncomp.push(comp[i]); break
    }
  }
  ncomp.orig = name;
  return ncomp;
}
