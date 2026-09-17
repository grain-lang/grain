// This function paves over some of the weird virtual filesystem stuff inside `pkg`
if (process.pkg) {
  const fs = require("fs");
  const path = require("path");
  const stdlib = require("@grain/stdlib");
  // NOTE: We need to path.resolve here because pkg's vfs returns a posix path on Windows, which breaks jsoo internals
  const stdlibPath = path.resolve(fs.realpathSync(stdlib));
  const vfs = process.pkg.vfs;

  // NOTE: pkg's SeaProvider lazily reads files outside its VFS, but doesn't
  // support file descriptor APIs, so we load the files into the VFS ourselves.
  const copyGrainFiles = (dir) => {
    for (const entry of fs.readdirSync(dir)) {
      const file = path.join(dir, entry);
      const stats = fs.statSync(file);
      if (stats.isDirectory()) {
        copyGrainFiles(file);
      } else if (stats.isFile() && path.extname(file) === ".gr") {
        const content = fs.readFileSync(file);
        vfs.writeFileSync(file, content);
      }
    }
  };
  copyGrainFiles(stdlibPath);

  const targetDirectory = path.join(process.cwd(), "target", "stdlib");

  // Helpers to determine if we are working with the packaged grain stdlib
  const isSubDirectory = (parent, child) => {
    const relative = path.relative(parent, child);
    return (
      relative !== "" &&
      !relative.startsWith("..") &&
      !path.isAbsolute(relative)
    );
  };
  const isInStdlib = (p) =>
    isSubDirectory(stdlib, p) || isSubDirectory(stdlibPath, p);
  const isGrainFile = (p) => path.extname(p) === ".gr";

  // Patch the fs module to redirect calls to the stdlib to the virtual filesystem
  // NOTE: While the `pkg` vfs does this itself it doesn't handle file descriptors correctly, so we need to do that ourselves
  const FS_NAMES = [
    "readFileSync",
    "readFile",
    "writeFileSync",
    "writeFile",
    "appendFileSync",
    "appendFile",
    "openSync",
    "open",
    "statSync",
    "stat",
    "lstatSync",
    "lstat",
    "readdirSync",
    "readdir",
    "existsSync",
    "accessSync",
    "access",
    "mkdirSync",
    "mkdir",
    "rmSync",
    "rm",
    "unlinkSync",
    "unlink",
    "realpathSync",
    "realpath",
    "createReadStream",
    "createWriteStream",
    "fstat",
    "fstatSync",
    "closeSync",
    "readSync",
  ];
  const virtualFileDescriptors = new Set();
  for (const name of FS_NAMES) {
    const originalFunc = fs[name];
    if (typeof originalFunc !== "function") continue;
    fs[name] = function (arg0, ...args) {
      if (typeof arg0 == "string" && isInStdlib(arg0)) {
        if (isGrainFile(arg0)) {
          const result = vfs[name](arg0, ...args);
          if (name === "openSync" || name === "open")
            virtualFileDescriptors.add(result);
          return result;
        } else {
          // Remap stdlib paths to target directory so that we can write to them
          const relativePath = path.relative(
            path.resolve(stdlibPath),
            path.resolve(arg0),
          );
          const newPath = path.join(targetDirectory, relativePath);
          if (!fs.existsSync(targetDirectory))
            fs.mkdirSync(targetDirectory, { recursive: true });
          return originalFunc.call(fs, newPath, ...args);
        }
      } else if (typeof arg0 == "number" && virtualFileDescriptors.has(arg0)) {
        if (name === "closeSync" || name === "close")
          virtualFileDescriptors.delete(arg0);
        return vfs[name](arg0, ...args);
      }
      return originalFunc.call(fs, arg0, ...args);
    };
  }
}
