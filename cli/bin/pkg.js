// This function paves over some of the weird virtual filesystem stuff inside `pkg`
function preparePkg(stdlibPath) {
  if (!process.pkg || !stdlibPath) {
    return;
  }

  const fs = require("fs");
  const path = require("path");

  const stdlibTarget = path.join(process.cwd(), "/target");

  try {
    fs.mkdirSync(stdlibTarget, { recursive: true });
  } catch (err) {
    console.error(`Unable to create directory: ${stdlibTarget}`, err);
    process.exit(1);
  }

  process.pkg.mount(
    new RegExp(stdlibPath + "/(.+)(?<!.gr)$"),
    function (_match, group1) {
      return path.join(stdlibTarget, group1);
    }
  );
}

module.exports = preparePkg;
