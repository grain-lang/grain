# Changelog

### [0.3.2](https://www.github.com/grain-lang/grain/compare/stdlib-v0.3.1...stdlib-v0.3.2) (2021-05-29)


### Features

* Export number runtime functions as operators & deprecate old identifiers ([#629](https://www.github.com/grain-lang/grain/issues/629)) ([b99441a](https://www.github.com/grain-lang/grain/commit/b99441aebc8c6d643be460177277112428e7cf58))
* Implement Bytes type and Bytes stdlib ([#633](https://www.github.com/grain-lang/grain/issues/633)) ([4b81898](https://www.github.com/grain-lang/grain/commit/4b81898d4cf17b34dc35e975b228b08b53d81a2a))
* Implement Grain array methods in Grain instead of the compiler ([#660](https://www.github.com/grain-lang/grain/issues/660)) ([b1397fa](https://www.github.com/grain-lang/grain/commit/b1397fae4c49a6b6789cbacc4f14ec6ee484c483))
* Rename Queue/Stack functions & deprecate old identifiers ([#635](https://www.github.com/grain-lang/grain/issues/635)) ([3a8da65](https://www.github.com/grain-lang/grain/commit/3a8da654f2f3a032e3413a44f543b79dc3ee23e1))
* **stdlib:** add `Queue.size` and `Stack.size` ([#647](https://www.github.com/grain-lang/grain/issues/647)) ([82ed533](https://www.github.com/grain-lang/grain/commit/82ed5336c3c5ab0679f5a2db2172084ec08ce411))
* **stdlib:** add Range.map ([#674](https://www.github.com/grain-lang/grain/issues/674)) ([5c33861](https://www.github.com/grain-lang/grain/commit/5c33861d933e6ee0049dadd54e612c2c25d623ca)), closes [#616](https://www.github.com/grain-lang/grain/issues/616)
* Support \b, \f, \t, and \v escapes ([c5d3e44](https://www.github.com/grain-lang/grain/commit/c5d3e44981b4456bbb9c9c63ef5f46721cb4a8c7))


### Bug Fixes

* **compiler:** properly handle special float equality ([#664](https://www.github.com/grain-lang/grain/issues/664)) ([b63597a](https://www.github.com/grain-lang/grain/commit/b63597a2a0a490015b2af0668894ed9c9afb81ce))
* Fix parsing of '\'' char literal ([c5d3e44](https://www.github.com/grain-lang/grain/commit/c5d3e44981b4456bbb9c9c63ef5f46721cb4a8c7))
* Properly escape quoted strings in toString/print ([#670](https://www.github.com/grain-lang/grain/issues/670)) ([c5d3e44](https://www.github.com/grain-lang/grain/commit/c5d3e44981b4456bbb9c9c63ef5f46721cb4a8c7))
* **stdlib:** fdWrite range error ([#677](https://www.github.com/grain-lang/grain/issues/677)) ([73e0a7a](https://www.github.com/grain-lang/grain/commit/73e0a7a1d5d393dd4b050275b68b949a14fe9936))

### [0.3.1](https://www.github.com/grain-lang/grain/compare/stdlib-v0.3.0...stdlib-v0.3.1) (2021-04-29)


### ⚠ BREAKING CHANGES

* rename Queue/Stack lib methods (#625)

### Reverts

* "chore!: rename Queue/Stack lib methods ([#625](https://www.github.com/grain-lang/grain/issues/625))" ([#632](https://www.github.com/grain-lang/grain/issues/632)) ([c9af90e](https://www.github.com/grain-lang/grain/commit/c9af90e77e5c5bb9649b8072b9cdff16ae2d1e60))


### Miscellaneous Chores

* rename Queue/Stack lib methods ([#625](https://www.github.com/grain-lang/grain/issues/625)) ([8117592](https://www.github.com/grain-lang/grain/commit/8117592dbaa09b4443003f234b6a2dcadb235a8b))

## [0.3.0](https://www.github.com/grain-lang/grain/compare/stdlib-v0.2.0...stdlib-v0.3.0) (2021-04-21)


### ⚠ BREAKING CHANGES

* **stdlib:** Replace JS entrypoint with locator file (#586)
* Rename `registerBasePrinter` to `dangerouslyRegisterBasePrinter` in runtime/exception
* Use Grain exceptions instead of JS exceptions (#565)
* Update assignment semantics (#560)
* Grain implementation of memory allocator (#530)
* Refactor value tags (#526)
* **stdlib:** Ensure string methods are data-last (#494)
* **runtime:** Print strings without quotes (#495)
* Char literals (#477)
* Char (#474)
* Rework memory management (#461)
* convert fallible List methods to return Option (#460)
* change return type of Array.find/findIndex to Option (#459)
* Add support for bitwise operators (#425)
* remove `^` as unbox operator (fixes #183) (#426)

### Features

* Add ability to `throw` exceptions ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))
* Add ability to register custom exception printers ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))
* Add Exception stdlib with Exception.registerPrinter ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))
* add initial Range stdlib module ([#456](https://www.github.com/grain-lang/grain/issues/456)) ([508f23c](https://www.github.com/grain-lang/grain/commit/508f23c32ad9b713843b3fb35dc3c9ac308303e9))
* add List.init to stdlib ([#465](https://www.github.com/grain-lang/grain/issues/465)) ([6b833d8](https://www.github.com/grain-lang/grain/commit/6b833d839deaa81c9d755ebf2da61e36688cd8ee))
* add reduce/flatMap/every/some to Array stdlib ([#455](https://www.github.com/grain-lang/grain/issues/455)) ([51a7a80](https://www.github.com/grain-lang/grain/commit/51a7a80b92ebe5e369660b293dd1d954aea56b9b))
* add reducei/counti/filter/filteri/unique to Array stdlib ([#473](https://www.github.com/grain-lang/grain/issues/473)) ([3e70cd5](https://www.github.com/grain-lang/grain/commit/3e70cd5c70aa12f14b6de1381f7cb38e88bbb890))
* add Set stdlib module ([#466](https://www.github.com/grain-lang/grain/issues/466)) ([2eb2604](https://www.github.com/grain-lang/grain/commit/2eb2604b1eeac6cf9f1d9cc6a6e11772bfc956c8))
* Add support for bitwise operators ([#425](https://www.github.com/grain-lang/grain/issues/425)) ([5c2b8bf](https://www.github.com/grain-lang/grain/commit/5c2b8bf0e97db35c98918e115e9d7cc48425fded))
* add update method to map stdlib ([#421](https://www.github.com/grain-lang/grain/issues/421)) ([292b5f9](https://www.github.com/grain-lang/grain/commit/292b5f9bbf1ea84b0540fe828fb1e3dba712340f))
* change return type of Array.find/findIndex to Option ([#459](https://www.github.com/grain-lang/grain/issues/459)) ([aa3767d](https://www.github.com/grain-lang/grain/commit/aa3767d5d429b33c54cec458f916715f931f9bc2))
* Char ([#474](https://www.github.com/grain-lang/grain/issues/474)) ([c9422f8](https://www.github.com/grain-lang/grain/commit/c9422f89573cc94081d2dd5f9e11ee17ec475668))
* Char literals ([#477](https://www.github.com/grain-lang/grain/issues/477)) ([cf7eaa5](https://www.github.com/grain-lang/grain/commit/cf7eaa55d239fc7a524a85dc2d53bcfb233e8061))
* Compile grainc to JS & create binaries with pkg ([#570](https://www.github.com/grain-lang/grain/issues/570)) ([f4919bd](https://www.github.com/grain-lang/grain/commit/f4919bdbab7dddd433b3f53bf8a8536a7efd5b03))
* convert fallible List methods to return Option ([#460](https://www.github.com/grain-lang/grain/issues/460)) ([a08768e](https://www.github.com/grain-lang/grain/commit/a08768e85e94288c261746ede40842c833438faa))
* Grain implementation of Char and String libraries ([#559](https://www.github.com/grain-lang/grain/issues/559)) ([7424cc5](https://www.github.com/grain-lang/grain/commit/7424cc5befe5c3416c26c5e943cc064329eac025))
* Grain implementation of equals ([#538](https://www.github.com/grain-lang/grain/issues/538)) ([09617bc](https://www.github.com/grain-lang/grain/commit/09617bca84e503935ef33e3f7f523a0fce7c4f17))
* Grain implementation of hashing ([#557](https://www.github.com/grain-lang/grain/issues/557)) ([40723fc](https://www.github.com/grain-lang/grain/commit/40723fca645f147b98eea83ffc11bd53359ce29a))
* Grain implementation of memory allocator ([#530](https://www.github.com/grain-lang/grain/issues/530)) ([fd8faaa](https://www.github.com/grain-lang/grain/commit/fd8faaa1425d398c55af36c9cdd77a59cf2eeccf))
* Grain implementation of memory manager ([#534](https://www.github.com/grain-lang/grain/issues/534)) ([cea6dcc](https://www.github.com/grain-lang/grain/commit/cea6dccaf45e8bdd07eb6c674f30c53a50f37a19))
* Grain implementation of number libraries ([#542](https://www.github.com/grain-lang/grain/issues/542)) ([d90d38b](https://www.github.com/grain-lang/grain/commit/d90d38b8e5756ea15f90504652af5698cb8822f8))
* Grain implementation of number runtime ([#537](https://www.github.com/grain-lang/grain/issues/537)) ([94460c0](https://www.github.com/grain-lang/grain/commit/94460c0aa83c737278f14bde2d2651bf6f8d9798))
* Grain implementation of toString/print ([#540](https://www.github.com/grain-lang/grain/issues/540)) ([8c77905](https://www.github.com/grain-lang/grain/commit/8c779059c4a2a71d0ccacc51d946dde2d48d6623))
* Grain wasi bindings ([#562](https://www.github.com/grain-lang/grain/issues/562)) ([323006b](https://www.github.com/grain-lang/grain/commit/323006b7a3e12194b252bcbad2ef8a3eb8e1f043))
* Implement low-level wasm i32/i64/f32/f64 libraries ([#517](https://www.github.com/grain-lang/grain/issues/517)) ([721d011](https://www.github.com/grain-lang/grain/commit/721d011abeb79b86516bbb145e5424abfc11b1d4))
* Implement String.indexOf, String.explode, and String.split ([#450](https://www.github.com/grain-lang/grain/issues/450)) ([6dc5466](https://www.github.com/grain-lang/grain/commit/6dc54666db1417ce4afa998c663754089d497676))
* Implement support for printing in Grain ([#561](https://www.github.com/grain-lang/grain/issues/561)) ([bfe471c](https://www.github.com/grain-lang/grain/commit/bfe471c668ee5838bde8a307bfc0f5d650a9b594))
* Optimize number to string conversions by writing UTF8 directly ([#539](https://www.github.com/grain-lang/grain/issues/539)) ([f18d26e](https://www.github.com/grain-lang/grain/commit/f18d26ef0129440ef5a6a8d072d8bdc1c3990c2f))
* **stdlib:** add Stack module ([#491](https://www.github.com/grain-lang/grain/issues/491)) ([8310d33](https://www.github.com/grain-lang/grain/commit/8310d337cb8c99135b9eb00e6bc9634b896762c7))
* **stdlib:** Implement Char toString ([#481](https://www.github.com/grain-lang/grain/issues/481)) ([37ba683](https://www.github.com/grain-lang/grain/commit/37ba683bb177a59ae43101a7abac5f8dc0e935c4))
* **stdlib:** String.implode ([#489](https://www.github.com/grain-lang/grain/issues/489)) ([045077b](https://www.github.com/grain-lang/grain/commit/045077bedbc5585bc26385af3750e50b0e055852))
* Update assignment semantics ([#560](https://www.github.com/grain-lang/grain/issues/560)) ([03a3217](https://www.github.com/grain-lang/grain/commit/03a3217fb7bc755407917991e0ebac3a66ea071e))
* Use Grain exceptions instead of JS exceptions ([#565](https://www.github.com/grain-lang/grain/issues/565)) ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))


### Bug Fixes

* `export *` with exceptions ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))
* Can't use memory.data ([#480](https://www.github.com/grain-lang/grain/issues/480)) ([1a4c3b9](https://www.github.com/grain-lang/grain/commit/1a4c3b9ff32d261aaf1a3670b14904c90ed22f76))
* loop body typechecking ([#533](https://www.github.com/grain-lang/grain/issues/533)) ([18e68ea](https://www.github.com/grain-lang/grain/commit/18e68eaf8f617c3cafa8d7aa5427ba01bead7172))
* Properly incRef exception printers ([#581](https://www.github.com/grain-lang/grain/issues/581)) ([61c2a94](https://www.github.com/grain-lang/grain/commit/61c2a94dbe38ff074e6b53395c403d30996b60a0))
* remove `^` as unbox operator (fixes [#183](https://www.github.com/grain-lang/grain/issues/183)) ([#426](https://www.github.com/grain-lang/grain/issues/426)) ([08b6b05](https://www.github.com/grain-lang/grain/commit/08b6b057a98a6a81c10d0856e27735dc6d5bdef0))
* Rework memory management ([#461](https://www.github.com/grain-lang/grain/issues/461)) ([84318b0](https://www.github.com/grain-lang/grain/commit/84318b01a21137492e9728f346680225f1d1ea9a))
* **runtime:** Return correct pointer to morecore ([#602](https://www.github.com/grain-lang/grain/issues/602)) ([af39cc4](https://www.github.com/grain-lang/grain/commit/af39cc46c8e12d869aaf5c42c0a3fd2afbc61638))
* Throwing an exception now traps immediately in all cases ([61c2a94](https://www.github.com/grain-lang/grain/commit/61c2a94dbe38ff074e6b53395c403d30996b60a0))
* use direct node call instead of executable file ([#431](https://www.github.com/grain-lang/grain/issues/431)) ([747ee8d](https://www.github.com/grain-lang/grain/commit/747ee8d8938b112befc018d81111f244c6884c18))
* Use Is instead of Eq for match variant comparison ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))


### Miscellaneous Chores

* Refactor value tags ([#526](https://www.github.com/grain-lang/grain/issues/526)) ([9b21d22](https://www.github.com/grain-lang/grain/commit/9b21d22e403f37d0cfb89891f9c31f40f03ae061))
* Rename `registerBasePrinter` to `dangerouslyRegisterBasePrinter` in runtime/exception ([61c2a94](https://www.github.com/grain-lang/grain/commit/61c2a94dbe38ff074e6b53395c403d30996b60a0))
* **runtime:** Print strings without quotes ([#495](https://www.github.com/grain-lang/grain/issues/495)) ([16671c2](https://www.github.com/grain-lang/grain/commit/16671c2448044bfc20eec32800b4a49ee6f2bb4e))
* **stdlib:** Ensure string methods are data-last ([#494](https://www.github.com/grain-lang/grain/issues/494)) ([f871efc](https://www.github.com/grain-lang/grain/commit/f871efc8943c640c377fae369d7704844d3f86a4))
* **stdlib:** Replace JS entrypoint with locator file ([#586](https://www.github.com/grain-lang/grain/issues/586)) ([a9ec7e2](https://www.github.com/grain-lang/grain/commit/a9ec7e237f22fe4eb507919972f4c41aa5eed55b))
