#!/usr/bin/env node

if (process.pkg) {
  // This resolver paves over a quirk of pkg where `process.execPath` points to the sea executable rather than node, which causes
  // problems for our subcommands. To work around this we check the script being executed and route to the correct subcommand manually.
  // This is a bit hacky but it works for our use case and is scoped to the pkg build, so it shouldn't cause any issues for development.
  const execute = (package) => {
    // Remap the arguments to point to the correct subcommand script, then require it.
    process.argv = [process.argv[0], ...process.argv.slice(2)];
    require(package);
  };
  switch (process.argv[2]) {
    case "/snapshot/grain/cli/bin/grainc.js":
      execute("./grainc.js");
      break;
    case "/snapshot/grain/cli/bin/graindoc.js":
      execute("./graindoc.js");
      break;
    case "/snapshot/grain/cli/bin/grainformat.js":
      execute("./grainformat.js");
      break;
    case "/snapshot/grain/cli/bin/grainlsp.js":
      execute("./grainlsp.js");
      break;
    case "/snapshot/grain/cli/bin/grainrun.js":
      execute("./grainrun.js");
      break;
    default:
      // By default we don't need to remap anything
      require("./grain.js");
      break;
  }
} else {
  // In development, we can just run the main grain.js file normally.
  require("./grain.js");
}
