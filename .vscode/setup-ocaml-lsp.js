const fs = require('fs');
const path = require('path');

const settingsPath = path.join(__dirname, 'settings.json');

const compilerPath = path.join(__dirname, '../compiler');

const output = `{
  "ocaml.sandbox": {
    "root": "${compilerPath}",
    "kind": "esy"
  }
}`;

fs.writeFileSync(settingsPath, output);
