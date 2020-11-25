import { promisify } from 'util';
import { createRequire } from 'module';

import globby from 'globby';
import replaceExt from 'replace-ext';

// Making asc interop with ESM
const require = createRequire(import.meta.url);
const asc = require('assemblyscript/cli/asc');

const compileFile = promisify(asc.main);

// AssemblyScript compiler is ready
await asc.ready;

const filenames = await globby('stdlib-external/**/*.ts');

const files = filenames.map((filename) =>
  compileFile([filename, "--sourceMap", "-o", replaceExt(filename, ".gr.wasm")])
);

await Promise.all(files);
