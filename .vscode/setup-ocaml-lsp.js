const fs = require("fs");
const path = require("path");

const settingsPath = path.join(__dirname, "settings.json");

const ocamlsandbox = "ocaml.sandbox";

const output = {
  "files.associations": {
    "*.dyp": "ocaml.ocamllex",
  },
};

// On Mac and Linux, we can use our yarn trick
// but on Windows, they don't run vscode in the correct directory
// Ref https://github.com/microsoft/vscode/issues/43237
if (process.platform === "win32") {
  const compilerPath = path.join(__dirname, "../compiler");

  // This also means that Windows needs to have esy installed globally
  console.log(
    "NOTE: It looks like you are using Windows. You'll need to install esy globally using `npm i -g esy`"
  );

  output[ocamlsandbox] = {
    root: `${compilerPath}`,
    kind: "esy",
  };
} else {
  output[ocamlsandbox] = {
    kind: "custom",
    template:
      "yarn --silent workspace @grain/compiler run --silent esy b $prog $args",
  };
}

fs.writeFileSync(settingsPath, JSON.stringify(output, null, 2));
