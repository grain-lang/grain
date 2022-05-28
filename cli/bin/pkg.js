// This function paves over some of the weird virtual filesystem stuff inside `pkg`
if (process.pkg) {
  const fs = require("fs");
  const path = require("path");
  const stdlibPath = require("@grain/stdlib");
  const stdlibPathWithTrailingSep = `${stdlibPath}${path.sep}`;
  // From https://github.com/sindresorhus/escape-string-regexp/blob/main/index.js
  const regexpSafeStdlibPath = stdlibPathWithTrailingSep
    .replace(/[|\\{}()[\]^$+*?.]/g, "\\$&")
    .replace(/-/g, "\\x2d");

  const stdlibTarget = path.join(process.cwd(), "/target");

  try {
    fs.mkdirSync(stdlibTarget, { recursive: true });
  } catch (err) {
    console.error(`Unable to create directory: ${stdlibTarget}`, err);
    process.exit(1);
  }

  process.pkg.mount(
    new RegExp(`${regexpSafeStdlibPath}(.+)(?<!.gr)$`),
    function (_match, group1) {
      return path.join(stdlibTarget, group1);
    }
  );
}
