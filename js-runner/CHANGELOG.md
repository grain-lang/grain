# Changelog

### [0.5.3](https://github.com/grain-lang/grain/compare/js-runner-v0.5.2...js-runner-v0.5.3) (2022-08-05)


### Miscellaneous Chores

* **js-runner:** Synchronize Grain versions

### [0.5.2](https://github.com/grain-lang/grain/compare/js-runner-v0.5.1...js-runner-v0.5.2) (2022-06-29)


### Miscellaneous Chores

* **js-runner:** Synchronize Grain versions

### [0.5.1](https://github.com/grain-lang/grain/compare/js-runner-v0.5.0...js-runner-v0.5.1) (2022-06-08)


### Miscellaneous Chores

* **js-runner:** Synchronize Grain versions

## [0.5.0](https://github.com/grain-lang/grain/compare/js-runner-v0.4.0...js-runner-v0.5.0) (2022-06-05)


### ⚠ BREAKING CHANGES

* **compiler:** Remove decRefIgnoreZeros (#1068)

### Miscellaneous Chores

* **compiler:** Remove decRefIgnoreZeros ([#1068](https://github.com/grain-lang/grain/issues/1068)) ([3ae8eaa](https://github.com/grain-lang/grain/commit/3ae8eaabad4467304c500c2f0cc9c40749d8513b))

## [0.4.0](https://www.github.com/grain-lang/grain/compare/js-runner-v0.3.0...js-runner-v0.4.0) (2021-09-06)


### ⚠ BREAKING CHANGES

* **compiler:** Callee-owned values (#803)
* Rename JS "runtime" to "js-runner"/"runner" throughout project
* **compiler:** Rename `grainRuntime` to `_grainEnv`
* Refactor out references to "runtime" that aren't related to `stdlib/runtime` (#755)

### Features

* **compiler:** Callee-owned values ([#803](https://www.github.com/grain-lang/grain/issues/803)) ([c242e89](https://www.github.com/grain-lang/grain/commit/c242e89767788e590f053c3d3ddfa7208387c247))


### Bug Fixes

* **js-runner:** Add default object for destructuring ([#833](https://www.github.com/grain-lang/grain/issues/833)) ([ab96fd6](https://www.github.com/grain-lang/grain/commit/ab96fd6c29f297f0e5423f3a6011fe07cf488df8))
* **js-runner:** Monkeypatch fs_write in browser environment ([#832](https://www.github.com/grain-lang/grain/issues/832)) ([330b56f](https://www.github.com/grain-lang/grain/commit/330b56f3bdda17bf993e88bb35532831f95c63b3))


### Miscellaneous Chores

* **compiler:** Rename `grainRuntime` to `_grainEnv` ([2d99c5a](https://www.github.com/grain-lang/grain/commit/2d99c5ab8fa527328f18d471e8a9128561af7056))
* Refactor out references to "runtime" that aren't related to `stdlib/runtime` ([#755](https://www.github.com/grain-lang/grain/issues/755)) ([2d99c5a](https://www.github.com/grain-lang/grain/commit/2d99c5ab8fa527328f18d471e8a9128561af7056))
* Rename JS "runtime" to "js-runner"/"runner" throughout project ([2d99c5a](https://www.github.com/grain-lang/grain/commit/2d99c5ab8fa527328f18d471e8a9128561af7056))

## [0.3.0](https://www.github.com/grain-lang/grain/compare/runtime-v0.2.0...runtime-v0.3.0) (2021-04-21)


### ⚠ BREAKING CHANGES

* Correct type signature for `_start`
* Introduce `_gmain` for old behavior of `_start`
* Tail calls must be enabled explicitly via `--experimental-wasm-tail-call`
* Static linking of Grain modules (#584)
* Streamline runtime dependencies & entrypoints (#585)
* Use Grain exceptions instead of JS exceptions (#565)
* Grain implementation of memory allocator (#530)
* Refactor value tags (#526)
* Char (#474)
* Rework memory management (#461)

### Features

* Add ability to `throw` exceptions ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))
* Add ability to register custom exception printers ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))
* Add Exception stdlib with Exception.registerPrinter ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))
* Add support for loading Grain modules directly from an ArrayBuffer and running them ([f07f305](https://www.github.com/grain-lang/grain/commit/f07f30541b5d5d4e579db2d5e41b15841fc4ebbc))
* allow setting the initial memory via CLI option ([#448](https://www.github.com/grain-lang/grain/issues/448)) ([213ee5a](https://www.github.com/grain-lang/grain/commit/213ee5a2736a71eb6a62a28bd60f338e196ea265))
* Char ([#474](https://www.github.com/grain-lang/grain/issues/474)) ([c9422f8](https://www.github.com/grain-lang/grain/commit/c9422f89573cc94081d2dd5f9e11ee17ec475668))
* Grain implementation of equals ([#538](https://www.github.com/grain-lang/grain/issues/538)) ([09617bc](https://www.github.com/grain-lang/grain/commit/09617bca84e503935ef33e3f7f523a0fce7c4f17))
* Grain implementation of memory allocator ([#530](https://www.github.com/grain-lang/grain/issues/530)) ([fd8faaa](https://www.github.com/grain-lang/grain/commit/fd8faaa1425d398c55af36c9cdd77a59cf2eeccf))
* Grain implementation of memory manager ([#534](https://www.github.com/grain-lang/grain/issues/534)) ([cea6dcc](https://www.github.com/grain-lang/grain/commit/cea6dccaf45e8bdd07eb6c674f30c53a50f37a19))
* Grain implementation of toString/print ([#540](https://www.github.com/grain-lang/grain/issues/540)) ([8c77905](https://www.github.com/grain-lang/grain/commit/8c779059c4a2a71d0ccacc51d946dde2d48d6623))
* Implement support for printing in Grain ([#561](https://www.github.com/grain-lang/grain/issues/561)) ([bfe471c](https://www.github.com/grain-lang/grain/commit/bfe471c668ee5838bde8a307bfc0f5d650a9b594))
* Normalized wasm exports for linked modules ([3d4ac6e](https://www.github.com/grain-lang/grain/commit/3d4ac6edd1b805d7cada5e6fa951bb6afb0467d9))
* Static linking of Grain modules ([#584](https://www.github.com/grain-lang/grain/issues/584)) ([3d4ac6e](https://www.github.com/grain-lang/grain/commit/3d4ac6edd1b805d7cada5e6fa951bb6afb0467d9))
* Support for more WebAssembly runtimes, including Wasmtime and Wasmer ([3d4ac6e](https://www.github.com/grain-lang/grain/commit/3d4ac6edd1b805d7cada5e6fa951bb6afb0467d9))
* Use Grain exceptions instead of JS exceptions ([#565](https://www.github.com/grain-lang/grain/issues/565)) ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))


### Bug Fixes

* `export *` with exceptions ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))
* Correct type signature for `_start` ([3d4ac6e](https://www.github.com/grain-lang/grain/commit/3d4ac6edd1b805d7cada5e6fa951bb6afb0467d9))
* Rework memory management ([#461](https://www.github.com/grain-lang/grain/issues/461)) ([84318b0](https://www.github.com/grain-lang/grain/commit/84318b01a21137492e9728f346680225f1d1ea9a))
* **runtime:** Properly decRef record and array values ([#500](https://www.github.com/grain-lang/grain/issues/500)) ([88adb94](https://www.github.com/grain-lang/grain/commit/88adb942ecf5425b74540a9cddbcb775128d97eb))
* **runtime:** Return correct pointer to morecore ([#602](https://www.github.com/grain-lang/grain/issues/602)) ([af39cc4](https://www.github.com/grain-lang/grain/commit/af39cc46c8e12d869aaf5c42c0a3fd2afbc61638))
* Use Is instead of Eq for match variant comparison ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))
* Use proper return type for calls to external functions ([3d4ac6e](https://www.github.com/grain-lang/grain/commit/3d4ac6edd1b805d7cada5e6fa951bb6afb0467d9))
* utilize path.join to create filesystem paths in default locator ([#432](https://www.github.com/grain-lang/grain/issues/432)) ([8aa4fa0](https://www.github.com/grain-lang/grain/commit/8aa4fa0e454dfbfc828e27f6b0cc6ed042d22a3b))


### Miscellaneous Chores

* Introduce `_gmain` for old behavior of `_start` ([3d4ac6e](https://www.github.com/grain-lang/grain/commit/3d4ac6edd1b805d7cada5e6fa951bb6afb0467d9))
* Refactor value tags ([#526](https://www.github.com/grain-lang/grain/issues/526)) ([9b21d22](https://www.github.com/grain-lang/grain/commit/9b21d22e403f37d0cfb89891f9c31f40f03ae061))
* Streamline runtime dependencies & entrypoints ([#585](https://www.github.com/grain-lang/grain/issues/585)) ([bbdd38d](https://www.github.com/grain-lang/grain/commit/bbdd38d5f4e5958aa10d602ba6b57a588024b050))
* Tail calls must be enabled explicitly via `--experimental-wasm-tail-call` ([3d4ac6e](https://www.github.com/grain-lang/grain/commit/3d4ac6edd1b805d7cada5e6fa951bb6afb0467d9))
