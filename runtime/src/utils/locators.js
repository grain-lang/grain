import { readFile, readURL } from '../core/grain-module';

function normalizeSlash(s) {
  if (s) {
    return s.replace(/\/$/, '');
  }
}

// Default locator definitions. 'base' can either
// be a constant path or a thunk yielding a path.
// If 'base' yields 'null', then 'null' is returned
export function defaultURLLocator(base) {
  // normalize trailing slash
  base = normalizeSlash(base);
  return async (raw) => {
    if (base === null) {
      return null;
    }
    let module = raw.replace(/^GRAIN\$MODULE\$/, '');
    return readURL(base + "/" + module + ".wasm");
  };
}

export function defaultFileLocator(base) {
  const fs = require('fs');
  // normalize trailing slash
  base = normalizeSlash(base);
  return async (raw) => {
    let module = raw.replace(/^GRAIN\$MODULE\$/, '');
    if (base === null) {
      return null;
    }
    let fullpath = base + "/" + module + ".wasm";
    if (!fs.existsSync(fullpath)) {
      return null;
    }
    return readFile(fullpath);
  };
}