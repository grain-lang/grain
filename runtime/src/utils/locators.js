import path from 'path';
import { readFile, readURL } from '../core/grain-module';

function normalizeSlash(s) {
  if (s) {
    return s.replace(/\/$/, '');
  }
}

// Default locator definitions.
export function defaultURLLocator(bases = []) {
  // normalize trailing slash
  bases = bases.map(normalizeSlash);
  return async (raw) => {
    let module = raw.replace(/^GRAIN\$MODULE\$/, '');
    for (const base of bases) {
      let fullpath = base + "/" + module + ".gr.wasm";
      try {
        return await readURL(fullpath);
      } catch (e) {
        continue
      }
    }
    throw new Error(`Could not locate ${raw}`);
  };
}

export function defaultFileLocator(bases = []) {
  const fs = require('fs');
  // normalize trailing slash
  bases = bases.map(normalizeSlash);
  return async (raw) => {
    let module = raw.replace(/^GRAIN\$MODULE\$/, '');
    for (const base of bases) {
      let fullpath = path.join(base, module + ".gr.wasm");
      if (!fs.existsSync(fullpath)) {
        continue;
      }
      return readFile(fullpath);
    }
    return null
  };
}
