let test_dir = Fp.At.(Fp.absoluteCurrentPlatformExn(Sys.getcwd()) / "test");
let test_input_dir = Fp.At.(test_dir / "input");
let test_output_dir = Fp.At.(test_dir / "output");
let test_snapshots_dir = Fp.At.(test_dir / "__snapshots__");

let rec readdir = dir => {
  Fs.readDirExn(dir)
  |> List.fold_left(
       (results, filepath) => {
         Sys.is_directory(Fp.toString(filepath))
           ? Array.append(results, readdir(filepath))
           : Array.append(results, [|filepath|])
       },
       [||],
     );
};

let clean_wasm = dir =>
  Array.iter(
    file => {
      let filename = Fp.toString(file);
      if (String.ends_with(~suffix=".wasm", filename)
          || String.ends_with(~suffix=".wat", filename)) {
        Fs.rmExn(file);
      };
    },
    readdir(dir),
  );

let clean_output = output =>
  if (Sys.file_exists(Fp.toString(output))) {
    Array.iter(Fs.rmExn, readdir(output));
  };

let () = {
  Printexc.record_backtrace(true);
  clean_wasm(test_input_dir);
  clean_output(test_output_dir);
};

include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir: Fp.toString(test_snapshots_dir),
      projectDir: Fp.toString(test_dir),
    });
});
