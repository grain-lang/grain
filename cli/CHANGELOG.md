# Changelog

### [0.4.4](https://www.github.com/grain-lang/grain/compare/cli-v0.4.3...cli-v0.4.4) (2021-10-27)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * @grain/stdlib bumped from ^0.4.2 to ^0.4.3

### [0.4.3](https://www.github.com/grain-lang/grain/compare/cli-v0.4.2...cli-v0.4.3) (2021-10-11)


### Features

* **cli:** Refactor forwardable options ([#908](https://www.github.com/grain-lang/grain/issues/908)) ([9ecb49b](https://www.github.com/grain-lang/grain/commit/9ecb49b7fc29ab8cb09baefe512471c07b9dbdc6))
* **grainfmt:** Add format in place or output to a new file ([#904](https://www.github.com/grain-lang/grain/issues/904)) ([0d18935](https://www.github.com/grain-lang/grain/commit/0d1893576bf3ad9e4d5c3aca5bfa963966b84b66))


### Bug Fixes

* **cli:** Forward stdlib option to lsp and graindoc ([#916](https://www.github.com/grain-lang/grain/issues/916)) ([07c5237](https://www.github.com/grain-lang/grain/commit/07c5237795c9717cf925c6889e165f4eb8a04f8d))
* **cli:** Only add --experimental-wasm-bigint flag on node 14 or 15 ([#899](https://www.github.com/grain-lang/grain/issues/899)) ([1cc0c5c](https://www.github.com/grain-lang/grain/commit/1cc0c5c03d38957c90f71e99a188c448923a481c))


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * @grain/stdlib bumped from ^0.4.1 to ^0.4.2

### [0.4.2](https://www.github.com/grain-lang/grain/compare/cli-v0.4.1...cli-v0.4.2) (2021-09-07)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * @grain/stdlib bumped from ^0.4.0 to ^0.4.1

### [0.4.1](https://www.github.com/grain-lang/grain/compare/cli-v0.4.0...cli-v0.4.1) (2021-09-06)


### Bug Fixes

* **cli:** Manually bump CLI dependencies ([#871](https://www.github.com/grain-lang/grain/issues/871)) ([1e4a7a1](https://www.github.com/grain-lang/grain/commit/1e4a7a171c8dd41caf54e6218d59dffafef8f226))

## [0.4.0](https://www.github.com/grain-lang/grain/compare/cli-v0.3.2...cli-v0.4.0) (2021-09-06)


### ⚠ BREAKING CHANGES

* Rename JS "runtime" to "js-runner"/"runner" throughout project
* **compiler:** Rename `grainRuntime` to `_grainEnv`
* Refactor out references to "runtime" that aren't related to `stdlib/runtime` (#755)
* **compiler:** Universal WebAssembly initial and maximum pages flags (#668)
* **cli:** Split -g into --debug and --wat (#620)

### Features

* Add format command to the Grain CLI ([#829](https://www.github.com/grain-lang/grain/issues/829)) ([9334b71](https://www.github.com/grain-lang/grain/commit/9334b71c8282a143d44c5c8c731bc057281a772c))
* Add GrainDoc for markdown generation using doc comments ([#574](https://www.github.com/grain-lang/grain/issues/574)) ([558d5e2](https://www.github.com/grain-lang/grain/commit/558d5e2a7bccb5ca5b32b9da036b673e381bfc60))
* **cli:** Add `grain doc` command ([558d5e2](https://www.github.com/grain-lang/grain/commit/558d5e2a7bccb5ca5b32b9da036b673e381bfc60))
* **cli:** Split -g into --debug and --wat ([#620](https://www.github.com/grain-lang/grain/issues/620)) ([79809bb](https://www.github.com/grain-lang/grain/commit/79809bb12592bdcb52fcb8301e7d4b64f35276e8))
* **compiler:** Add --no-bulk-memory flag to polyfill bulk memory ops ([#819](https://www.github.com/grain-lang/grain/issues/819)) ([7db4ea6](https://www.github.com/grain-lang/grain/commit/7db4ea6578990c2f175c083ef378c47599d47fd1))
* **compiler:** Add --wasi-polyfill flag for custom implementations ([#800](https://www.github.com/grain-lang/grain/issues/800)) ([6879286](https://www.github.com/grain-lang/grain/commit/68792867f31671e12d07067dbef2c4c1288d7eac))
* **compiler:** Add Comments module to Grain_diagnostics ([558d5e2](https://www.github.com/grain-lang/grain/commit/558d5e2a7bccb5ca5b32b9da036b673e381bfc60))
* **compiler:** Add Markdown module to Grain_utils ([558d5e2](https://www.github.com/grain-lang/grain/commit/558d5e2a7bccb5ca5b32b9da036b673e381bfc60))
* **compiler:** Add Range module to Grain_utils ([558d5e2](https://www.github.com/grain-lang/grain/commit/558d5e2a7bccb5ca5b32b9da036b673e381bfc60))
* **compiler:** Allow disabling Binaryen optimizations ([#780](https://www.github.com/grain-lang/grain/issues/780)) ([a6c929c](https://www.github.com/grain-lang/grain/commit/a6c929c6265dd12379d31cfdc8f50bc83ddc6802))
* **compiler:** Universal WebAssembly initial and maximum pages flags ([#668](https://www.github.com/grain-lang/grain/issues/668)) ([ec9c0e2](https://www.github.com/grain-lang/grain/commit/ec9c0e211cf0872f2ac2d52fffd848c74e7a8894))
* **linker:** Add --use-start-section flag to use start section in output ([#844](https://www.github.com/grain-lang/grain/issues/844)) ([c221834](https://www.github.com/grain-lang/grain/commit/c221834f93b897c001d7530e6b4a354fd5c5d17b))


### Miscellaneous Chores

* **compiler:** Rename `grainRuntime` to `_grainEnv` ([2d99c5a](https://www.github.com/grain-lang/grain/commit/2d99c5ab8fa527328f18d471e8a9128561af7056))
* Refactor out references to "runtime" that aren't related to `stdlib/runtime` ([#755](https://www.github.com/grain-lang/grain/issues/755)) ([2d99c5a](https://www.github.com/grain-lang/grain/commit/2d99c5ab8fa527328f18d471e8a9128561af7056))
* Rename JS "runtime" to "js-runner"/"runner" throughout project ([2d99c5a](https://www.github.com/grain-lang/grain/commit/2d99c5ab8fa527328f18d471e8a9128561af7056))

### [0.3.2](https://www.github.com/grain-lang/grain/compare/cli-v0.3.1...cli-v0.3.2) (2021-05-29)


### Dependencies

* The following workspace dependencies were updated
  * dependencies
    * @grain/stdlib bumped from * to ^0.3.2

### [0.3.1](https://www.github.com/grain-lang/grain/compare/cli-v0.3.0...cli-v0.3.1) (2021-04-29)


### Bug Fixes

* **cli:** Correct typo in lsp catch block ([#618](https://www.github.com/grain-lang/grain/issues/618)) ([b903ced](https://www.github.com/grain-lang/grain/commit/b903ced32ad7a6a2055a73473921e791adfd6ae4))

## [0.3.0](https://www.github.com/grain-lang/grain/compare/cli-v0.2.0...cli-v0.3.0) (2021-04-21)


### ⚠ BREAKING CHANGES

* **cli:** Remove `-f` CLI option
* **compiler:** Remove --principal-types and --recursive-types compiler flags
* **compiler:** Rename --cdebug to --verbose
* **cli:** Pass compiler flags from the CLI to grainc directly (#613)
* Rework memory management (#461)

### Features

* allow setting the initial memory via CLI option ([#448](https://www.github.com/grain-lang/grain/issues/448)) ([213ee5a](https://www.github.com/grain-lang/grain/commit/213ee5a2736a71eb6a62a28bd60f338e196ea265))
* **cli:** Pass compiler flags from the CLI to grainc directly ([#613](https://www.github.com/grain-lang/grain/issues/613)) ([8f40383](https://www.github.com/grain-lang/grain/commit/8f40383af23b236b0333032bba193c39757d2569))
* Compile grainc to JS & create binaries with pkg ([#570](https://www.github.com/grain-lang/grain/issues/570)) ([f4919bd](https://www.github.com/grain-lang/grain/commit/f4919bdbab7dddd433b3f53bf8a8536a7efd5b03))
* Grain implementation of memory manager ([#534](https://www.github.com/grain-lang/grain/issues/534)) ([cea6dcc](https://www.github.com/grain-lang/grain/commit/cea6dccaf45e8bdd07eb6c674f30c53a50f37a19))
* Grain implementation of toString/print ([#540](https://www.github.com/grain-lang/grain/issues/540)) ([8c77905](https://www.github.com/grain-lang/grain/commit/8c779059c4a2a71d0ccacc51d946dde2d48d6623))
* Use real wasm tail call instruction ([#510](https://www.github.com/grain-lang/grain/issues/510)) ([9c9ffe4](https://www.github.com/grain-lang/grain/commit/9c9ffe48d78ed315f0a406d704c7a0fdbb116f1f))


### Bug Fixes

* replace shebang trick with flagged-respawn to support Windows ([#430](https://www.github.com/grain-lang/grain/issues/430)) ([4ea2602](https://www.github.com/grain-lang/grain/commit/4ea26021d270adc542736efa90093d2f2cdbef5b))
* Rework memory management ([#461](https://www.github.com/grain-lang/grain/issues/461)) ([84318b0](https://www.github.com/grain-lang/grain/commit/84318b01a21137492e9728f346680225f1d1ea9a))


### Miscellaneous Chores

* **cli:** Remove `-f` CLI option ([8f40383](https://www.github.com/grain-lang/grain/commit/8f40383af23b236b0333032bba193c39757d2569))
* **compiler:** Remove --principal-types and --recursive-types compiler flags ([8f40383](https://www.github.com/grain-lang/grain/commit/8f40383af23b236b0333032bba193c39757d2569))
* **compiler:** Rename --cdebug to --verbose ([8f40383](https://www.github.com/grain-lang/grain/commit/8f40383af23b236b0333032bba193c39757d2569))
