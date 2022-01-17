# Changelog

### [0.4.6](https://www.github.com/grain-lang/grain/compare/@grain/compiler-v0.4.5...@grain/compiler-v0.4.6) (2022-01-17)


### Bug Fixes

* **compiler:** Account for config that is not digestable ([#1098](https://www.github.com/grain-lang/grain/issues/1098)) ([b58323f](https://www.github.com/grain-lang/grain/commit/b58323f1764f4d870875b8c4ef19f9861b4188ad))
* **compiler:** Apply optimization flag at link stage ([#1099](https://www.github.com/grain-lang/grain/issues/1099)) ([49dac79](https://www.github.com/grain-lang/grain/commit/49dac797aa4833cc141f7c58d18d80d7cbc6b1c7))
* **compiler:** Inform the compiler that lsp mode is running on a root file ([b58323f](https://www.github.com/grain-lang/grain/commit/b58323f1764f4d870875b8c4ef19f9861b4188ad))
* **compiler:** Move lsp flag into grainc to remove it as a recompilation flag ([b58323f](https://www.github.com/grain-lang/grain/commit/b58323f1764f4d870875b8c4ef19f9861b4188ad))
* **graindoc:** Remove special idents from receiving parens ([#1104](https://www.github.com/grain-lang/grain/issues/1104)) ([5cb0fda](https://www.github.com/grain-lang/grain/commit/5cb0fda652cad0644f48d8196089430dca1cac0d))
* **grainfmt:** Indent function application args when adding parens ([#1095](https://www.github.com/grain-lang/grain/issues/1095)) ([64af7d3](https://www.github.com/grain-lang/grain/commit/64af7d387dca2fddb9b3d190ccdf5790ec3d8e65))
* **grainfmt:** Inform the formatter that it is running against root files ([b58323f](https://www.github.com/grain-lang/grain/commit/b58323f1764f4d870875b8c4ef19f9861b4188ad))
* **grainfmt:** Scope the comments in callback lambda closure ([#1090](https://www.github.com/grain-lang/grain/issues/1090)) ([0975580](https://www.github.com/grain-lang/grain/commit/09755801d8598691e4a2884a732c0ed9a9e10a2d))

### [0.4.5](https://www.github.com/grain-lang/grain/compare/@grain/compiler-v0.4.4...@grain/compiler-v0.4.5) (2021-12-31)


### Features

* **compiler:** Inline all primitives ([#1076](https://www.github.com/grain-lang/grain/issues/1076)) ([c227130](https://www.github.com/grain-lang/grain/commit/c2271307b7220d54dc1c252dfc4ee5532f68dd29))
* **grainfmt:** Drastically improve performance ([#1042](https://www.github.com/grain-lang/grain/issues/1042)) ([9bc7a55](https://www.github.com/grain-lang/grain/commit/9bc7a55a525c8018e9d915cf94af2f2486a21e27))
* **stdlib:** Add sign function to Number module ([#1079](https://www.github.com/grain-lang/grain/issues/1079)) ([b6483d5](https://www.github.com/grain-lang/grain/commit/b6483d5046cd1b6b89a717a925594d3b20b05837))


### Bug Fixes

* **compiler:** Correctly supply locations for types & variants ([9bc7a55](https://www.github.com/grain-lang/grain/commit/9bc7a55a525c8018e9d915cf94af2f2486a21e27))
* **grainfmt:** Improve correctness of formatting ([9bc7a55](https://www.github.com/grain-lang/grain/commit/9bc7a55a525c8018e9d915cf94af2f2486a21e27))

### [0.4.4](https://www.github.com/grain-lang/grain/compare/@grain/compiler-v0.4.3...@grain/compiler-v0.4.4) (2021-12-11)


### Features

* **compiler:** Allow [@external](https://www.github.com/external)Name attribute for foreign names ([#1060](https://www.github.com/grain-lang/grain/issues/1060)) ([f4c1311](https://www.github.com/grain-lang/grain/commit/f4c13115ef2eb589f035c31370e56a12bdc2e10c))
* **compiler:** Allow a newline character before a match guard ([#1063](https://www.github.com/grain-lang/grain/issues/1063)) ([bf41a9c](https://www.github.com/grain-lang/grain/commit/bf41a9cf585c63eb2482c7d94d9c615428bc8435))
* **stdlib:** Add reverse function to String module ([#1027](https://www.github.com/grain-lang/grain/issues/1027)) ([df761db](https://www.github.com/grain-lang/grain/commit/df761db55b3e14e31190090ae008ce5047135c09))
* **stdlib:** Add rotate function in Array module ([#838](https://www.github.com/grain-lang/grain/issues/838)) ([98fc577](https://www.github.com/grain-lang/grain/commit/98fc577ee754317cd2421bfaa8e3c1e049488949))
* **stdlib:** Add sort function to Array module ([#1012](https://www.github.com/grain-lang/grain/issues/1012)) ([9091930](https://www.github.com/grain-lang/grain/commit/9091930344224925bb7b2e1ef6f879c79a5c2f62))
* **stdlib:** Implement List.sort via mergesort ([#1014](https://www.github.com/grain-lang/grain/issues/1014)) ([a076e20](https://www.github.com/grain-lang/grain/commit/a076e200013114ccf16c2e6cbe814af1ec09c1ce))
* **stdlib:** Number.parseInt ([#1051](https://www.github.com/grain-lang/grain/issues/1051)) ([abafb58](https://www.github.com/grain-lang/grain/commit/abafb587e54219a32ed77ba09863bb2d6a80bac8))


### Bug Fixes

* **compiler:** Allow modules to re-export imported types containing generics ([#1019](https://www.github.com/grain-lang/grain/issues/1019)) ([003cb1e](https://www.github.com/grain-lang/grain/commit/003cb1e10ca51c3f16d2155581eb8dccdde3ce5c))
* **compiler:** Consider invalid CMIs as out of date and recompile ([#1061](https://www.github.com/grain-lang/grain/issues/1061)) ([80da3a9](https://www.github.com/grain-lang/grain/commit/80da3a9667020e4f5f855a0b9da8286508a07275))
* **compiler:** Fix binop operator reported location ([#1018](https://www.github.com/grain-lang/grain/issues/1018)) ([422dbcb](https://www.github.com/grain-lang/grain/commit/422dbcb01cd09627c6a5f4494cc7c2e7644e92f0))
* **compiler:** Fix counting of stack size ([#1066](https://www.github.com/grain-lang/grain/issues/1066)) ([f1b3e51](https://www.github.com/grain-lang/grain/commit/f1b3e51580062da9b7e42ec36eb9880b71375cb9))
* **compiler:** Fix error message when trying to export a constructor ([#1055](https://www.github.com/grain-lang/grain/issues/1055)) ([54ddb67](https://www.github.com/grain-lang/grain/commit/54ddb671745b554eacc8640b1337a46f423b1cc5))
* **compiler:** Fix memory leaks when loading from heap values ([#1038](https://www.github.com/grain-lang/grain/issues/1038)) ([7d3402d](https://www.github.com/grain-lang/grain/commit/7d3402db09b6e4e24e49d168e8ddd49044a98df1))
* **compiler:** Properly apply bindings when executing match guard ([#1039](https://www.github.com/grain-lang/grain/issues/1039)) ([121644b](https://www.github.com/grain-lang/grain/commit/121644be48e66ea1a7ce884ea816fe9af223442c))
* **compiler:** Remove <id></id> from default ident printer ([#1056](https://www.github.com/grain-lang/grain/issues/1056)) ([49cd5a0](https://www.github.com/grain-lang/grain/commit/49cd5a08e5d9b2753b47af324b7fb90f4a98b339))
* **graindoc:** Ensure value_description is resolved to outcome before printing ([#1070](https://www.github.com/grain-lang/grain/issues/1070)) ([5eb05cc](https://www.github.com/grain-lang/grain/commit/5eb05cc2dedc3b933e194be86dd5d3c3656d6490))
* **grainfmt:** Handle recursive enums ([7dc77cd](https://www.github.com/grain-lang/grain/commit/7dc77cd3ec28745dfad6cdbd1551c09a82a1ba6b))
* **grainfmt:** Resugar match lists properly ([7dc77cd](https://www.github.com/grain-lang/grain/commit/7dc77cd3ec28745dfad6cdbd1551c09a82a1ba6b))
* **stdlib:** Add bounds checking to Buffer addStringSlice & addBytesSlice ([#1065](https://www.github.com/grain-lang/grain/issues/1065)) ([06fe512](https://www.github.com/grain-lang/grain/commit/06fe512e863aeeb855ccf3e3b83bcd3bc8854723))
* **stdlib:** Support arrays of any type in Array rotate ([#1048](https://www.github.com/grain-lang/grain/issues/1048)) ([3ceb1cf](https://www.github.com/grain-lang/grain/commit/3ceb1cf04c1604f49077e8733dcccb6cdaaf9f3a))
* **stdlib:** Support empty arrays in Array rotate ([3ceb1cf](https://www.github.com/grain-lang/grain/commit/3ceb1cf04c1604f49077e8733dcccb6cdaaf9f3a))

### [0.4.3](https://www.github.com/grain-lang/grain/compare/@grain/compiler-v0.4.2...@grain/compiler-v0.4.3) (2021-10-27)


### Features

* **compiler:** Type aliases ([#995](https://www.github.com/grain-lang/grain/issues/995)) ([bc648ce](https://www.github.com/grain-lang/grain/commit/bc648ce98f1263cdb4163f7fd620de75771848ab))
* **stdlib:** Add cycle function to Array module ([#993](https://www.github.com/grain-lang/grain/issues/993)) ([c595622](https://www.github.com/grain-lang/grain/commit/c595622b28366655dfd5447270e8f9fc3f988a67))


### Bug Fixes

* **compiler:** Ensure main module is included in linker dependency graph ([#996](https://www.github.com/grain-lang/grain/issues/996)) ([d5c6c04](https://www.github.com/grain-lang/grain/commit/d5c6c048d4239ef07fb6ac0a79774d85933cccf9))
* **compiler:** Ensure TExpApp is always expansive in Grain ([ef0a69f](https://www.github.com/grain-lang/grain/commit/ef0a69fb4418d318b3227e5db7e743a026762274))
* **compiler:** Handle let-mut value restriction, such that mutable lets are always expansive ([ef0a69f](https://www.github.com/grain-lang/grain/commit/ef0a69fb4418d318b3227e5db7e743a026762274))
* **compiler:** Properly handle value restriction on function application & mutable vars ([#988](https://www.github.com/grain-lang/grain/issues/988)) ([ef0a69f](https://www.github.com/grain-lang/grain/commit/ef0a69fb4418d318b3227e5db7e743a026762274))
* **compiler:** Properly unmark type manifest bodies ([bc648ce](https://www.github.com/grain-lang/grain/commit/bc648ce98f1263cdb4163f7fd620de75771848ab))
* **grainfmt:** Break long parameterized types ([bc648ce](https://www.github.com/grain-lang/grain/commit/bc648ce98f1263cdb4163f7fd620de75771848ab))
* **grainfmt:** Wrap type annotation for a long line let assignment ([#1005](https://www.github.com/grain-lang/grain/issues/1005)) ([963b505](https://www.github.com/grain-lang/grain/commit/963b5053b57aece6b721bd99dbe143b117ad86d7))
* **linking:** Close file after reading ([c404fcf](https://www.github.com/grain-lang/grain/commit/c404fcf30bceab2911d3ab0c99f284791c799ca0))
* Properly open & close files ([#985](https://www.github.com/grain-lang/grain/issues/985)) ([c404fcf](https://www.github.com/grain-lang/grain/commit/c404fcf30bceab2911d3ab0c99f284791c799ca0))
* **runtime:** Add types to boxed GC functions to avoid weak type errors ([ef0a69f](https://www.github.com/grain-lang/grain/commit/ef0a69fb4418d318b3227e5db7e743a026762274))

### [0.4.2](https://www.github.com/grain-lang/grain/compare/@grain/compiler-v0.4.1...@grain/compiler-v0.4.2) (2021-10-11)


### Features

* **compiler:** Inline not `(!)` operator ([#937](https://www.github.com/grain-lang/grain/issues/937)) ([3f5e9a9](https://www.github.com/grain-lang/grain/commit/3f5e9a962ec8565f4fb79a1ee36d7b492da5cf11))
* **grainfmt:** Add format in place or output to a new file ([#904](https://www.github.com/grain-lang/grain/issues/904)) ([0d18935](https://www.github.com/grain-lang/grain/commit/0d1893576bf3ad9e4d5c3aca5bfa963966b84b66))
* **stdlib:** Add Conv.wasmI32ToNumber function ([#978](https://www.github.com/grain-lang/grain/issues/978)) ([c93ade8](https://www.github.com/grain-lang/grain/commit/c93ade873b51acca556e19a7483a5f29b4b6caa9))
* **stdlib:** Add string trim functions to String module ([#951](https://www.github.com/grain-lang/grain/issues/951)) ([e55de8f](https://www.github.com/grain-lang/grain/commit/e55de8fc0335b45252da9741a80b46a702cdb5b6))
* **stdlib:** Regular Expressions ([#680](https://www.github.com/grain-lang/grain/issues/680)) ([9601e16](https://www.github.com/grain-lang/grain/commit/9601e1655f1ffc76b700efef317366457c5614ef))


### Bug Fixes

* **compiler:** Allow trailing commas in imports and destructuring ([#890](https://www.github.com/grain-lang/grain/issues/890)) ([f5c24c9](https://www.github.com/grain-lang/grain/commit/f5c24c946b3356090b9aca6123bc14a19b4e06fb))
* **compiler:** Do not inline aliased mutables ([#909](https://www.github.com/grain-lang/grain/issues/909)) ([a50430a](https://www.github.com/grain-lang/grain/commit/a50430a2cff5f50dd666f1ec14bd485bfda77e86))
* **compiler:** Don't over-optimize mutable imports ([#967](https://www.github.com/grain-lang/grain/issues/967)) ([c3066f6](https://www.github.com/grain-lang/grain/commit/c3066f6e32bbbe3bd51b9a7b498567b8deb8d9ba))
* **compiler:** Don't warn on exhaustive boolean matches ([#913](https://www.github.com/grain-lang/grain/issues/913)) ([4189745](https://www.github.com/grain-lang/grain/commit/4189745d09c3fc6971f95dc1c8428dc73761aed3))
* **compiler:** Ensure cwd directory is normalized on Windows ([#952](https://www.github.com/grain-lang/grain/issues/952)) ([a3ab9c3](https://www.github.com/grain-lang/grain/commit/a3ab9c3cd2359866a61a1dd4b63f40b0f964e2a4))
* **compiler:** Export functions with aliased names as wasm functions ([#920](https://www.github.com/grain-lang/grain/issues/920)) ([afba142](https://www.github.com/grain-lang/grain/commit/afba142ef90d0d95ae0085debe50e1b34588b267))
* **compiler:** Fix parsing of block with lone 0-arg enum ([#885](https://www.github.com/grain-lang/grain/issues/885)) ([edfc21e](https://www.github.com/grain-lang/grain/commit/edfc21e0ca8f76f00b53d0fd07075ccf6299358e))
* **compiler:** Fix stack/heap number allocation boundary ([#974](https://www.github.com/grain-lang/grain/issues/974)) ([47b0f56](https://www.github.com/grain-lang/grain/commit/47b0f56c00e88ddc6054b8f8f8eea4b21da0c7f7))
* **compiler:** Follow proper calling convention when callling stdlib equals in match ([#895](https://www.github.com/grain-lang/grain/issues/895)) ([73fe7d4](https://www.github.com/grain-lang/grain/commit/73fe7d47783a76454496c8bba55ff051561ef975))
* **compiler:** Parse array set with newline correctly ([#926](https://www.github.com/grain-lang/grain/issues/926)) ([14e1822](https://www.github.com/grain-lang/grain/commit/14e18229f82637870c651b948fb15c3e5a7b082c))
* **compiler:** Remove existing exports when writing universal exports ([#919](https://www.github.com/grain-lang/grain/issues/919)) ([e80de43](https://www.github.com/grain-lang/grain/commit/e80de439e79ed1dab1c1adcbd5b9cca7a79be023))
* **compiler:** Resolve all modules fully during linking ([#950](https://www.github.com/grain-lang/grain/issues/950)) ([33b33a3](https://www.github.com/grain-lang/grain/commit/33b33a3926cc059a0a759c4ec894fb9f6f03d904))
* Correctly preserve start position for character literals ([#931](https://www.github.com/grain-lang/grain/issues/931)) ([8fcc575](https://www.github.com/grain-lang/grain/commit/8fcc575a3afde79ac6455b35839b1bcdd6ded41f))
* **graindoc:** Only extract attributes from Doc comments ([#970](https://www.github.com/grain-lang/grain/issues/970)) ([7b6ee5f](https://www.github.com/grain-lang/grain/commit/7b6ee5f085af89f3db181c8d6c2718ca161f2ead))
* **grainfmt:** fix some spacing issues with the formatter ([#955](https://www.github.com/grain-lang/grain/issues/955)) ([9c4be85](https://www.github.com/grain-lang/grain/commit/9c4be85011d6c030fd54954bd86b0a3ee0f80660))
* **grainfmt:** Format CRLF files without error ([#941](https://www.github.com/grain-lang/grain/issues/941)) ([ece7536](https://www.github.com/grain-lang/grain/commit/ece7536b6d20694360257515a6f963f115ed21b3))
* **grainfmt:** Group infix functions when passed as params ([bb9666d](https://www.github.com/grain-lang/grain/commit/bb9666d24991df94c82d4f50c1d25d7669cec3c9))
* **grainfmt:** handle empty type for foreign exports ([#907](https://www.github.com/grain-lang/grain/issues/907)) ([d3b60a4](https://www.github.com/grain-lang/grain/commit/d3b60a422568941185c2446464ba2eeb4a776ed2))
* **grainfmt:** Improve formatter performance ([#959](https://www.github.com/grain-lang/grain/issues/959)) ([4e739f1](https://www.github.com/grain-lang/grain/commit/4e739f1d12a1493207a81549f1c0145f296b50be))
* **grainfmt:** Print attributes for non-toplevel expressions ([#887](https://www.github.com/grain-lang/grain/issues/887)) ([72292fe](https://www.github.com/grain-lang/grain/commit/72292fe67cb21905dd976933d278810ac49bd35b))
* **grainfmt:** remove extra spaces around for/while loop conditions ([#897](https://www.github.com/grain-lang/grain/issues/897)) ([2054a83](https://www.github.com/grain-lang/grain/commit/2054a832d713cd335417fe6e04e537453045c43f))
* **grainfmt:** Remove extraneous parens around infix function application ([#902](https://www.github.com/grain-lang/grain/issues/902)) ([5c1906a](https://www.github.com/grain-lang/grain/commit/5c1906a98664f1b14f969fff5c0e48ddd08988c4))
* **grainfmt:** Removed a trailing comma after a spread in a list, and fixed escaped single quote ([#923](https://www.github.com/grain-lang/grain/issues/923)) ([3a138d6](https://www.github.com/grain-lang/grain/commit/3a138d6002fdb2a84df89355e10a143ff792b122))
* **grainfmt:** Resolve issues with comments within braces ([#888](https://www.github.com/grain-lang/grain/issues/888)) ([9c88b89](https://www.github.com/grain-lang/grain/commit/9c88b89db4ce1d7a256cbde51d74225513b76a3e))
* **stdlib:** Fix Char.code memory issue ([#928](https://www.github.com/grain-lang/grain/issues/928)) ([dfa31d8](https://www.github.com/grain-lang/grain/commit/dfa31d8aa60a41d08494f3817bdb628a30d83f41))
* **stdlib:** Fix issue with list printing ([#894](https://www.github.com/grain-lang/grain/issues/894)) ([e2a33a9](https://www.github.com/grain-lang/grain/commit/e2a33a9607bd076abbf6375389f1a7896775e6bc))
* **stdlib:** Fix memory issue in list printing ([#912](https://www.github.com/grain-lang/grain/issues/912)) ([3e0a805](https://www.github.com/grain-lang/grain/commit/3e0a8059a36d1042fa06d6aed54565ced116fd87))

### [0.4.1](https://www.github.com/grain-lang/grain/compare/@grain/compiler-v0.4.0...@grain/compiler-v0.4.1) (2021-09-07)


### Bug Fixes

* **grainfmt:** Remove extra spacing from type signatures ([#877](https://www.github.com/grain-lang/grain/issues/877)) ([c8cf421](https://www.github.com/grain-lang/grain/commit/c8cf421940fb21c06c5e3f9a11c72670bccccfb2))

## [0.4.0](https://www.github.com/grain-lang/grain/compare/@grain/compiler-v0.3.2...@grain/compiler-v0.4.0) (2021-09-06)


### ⚠ BREAKING CHANGES

* **compiler:** Add typed well-formedness pass which forbids usage of WasmXX values outside of `@disableGC` context (#772)
* **stdlib:** Convert sys functions to return Results instead of throwing errors (#792)
* **compiler:** Callee-owned values (#803)
* **compiler:** Remove user manual memory management calls when --no-gc supplied (#817)
* **compiler:** Refactor module resolution in order to properly resolve relative paths (#797)
* Rename JS "runtime" to "js-runner"/"runner" throughout project
* **compiler:** Rename `grainRuntime` to `_grainEnv`
* Refactor out references to "runtime" that aren't related to `stdlib/runtime` (#755)
* **compiler:** Improve assert messages to contain location information (#737)
* **runtime:** Fix bug in equalHelp preventing simpleNum/boxedNum equality checks from happening correctly
* Adjust Number equality, thus that 5 == 5.0 (#726)
* **compiler:** Avoid building JS bundles by default (#739)
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
* **compiler:** Add typed well-formedness pass which forbids usage of WasmXX values outside of `[@disable](https://www.github.com/disable)GC` context ([#772](https://www.github.com/grain-lang/grain/issues/772)) ([42fbad6](https://www.github.com/grain-lang/grain/commit/42fbad632f1f7137285dbf4dbe609ddfb91c956e))
* **compiler:** Allow disabling Binaryen optimizations ([#780](https://www.github.com/grain-lang/grain/issues/780)) ([a6c929c](https://www.github.com/grain-lang/grain/commit/a6c929c6265dd12379d31cfdc8f50bc83ddc6802))
* **compiler:** Call known functions directly ([#777](https://www.github.com/grain-lang/grain/issues/777)) ([ad282be](https://www.github.com/grain-lang/grain/commit/ad282be2671a84d5383f4e221059dff527c37049))
* **compiler:** Callee-owned values ([#803](https://www.github.com/grain-lang/grain/issues/803)) ([c242e89](https://www.github.com/grain-lang/grain/commit/c242e89767788e590f053c3d3ddfa7208387c247))
* **compiler:** Emit a warning when using Pervasives.(!=) with unsafe Wasm types ([#758](https://www.github.com/grain-lang/grain/issues/758)) ([1041e38](https://www.github.com/grain-lang/grain/commit/1041e38279677e57ef83b7172fc8db95d3fc9598))
* **compiler:** Emit a warning when using Pervasives.(==) with unsafe Wasm types ([#742](https://www.github.com/grain-lang/grain/issues/742)) ([8b00055](https://www.github.com/grain-lang/grain/commit/8b00055d7d91b3b8f5d8757a2aed14baa37e4bd6))
* **compiler:** Improve assert messages to contain location information ([#737](https://www.github.com/grain-lang/grain/issues/737)) ([26f645b](https://www.github.com/grain-lang/grain/commit/26f645ba5f3be20d5db3a0933165bb6346d06b9d))
* **compiler:** List exports in wasm_utils ([#845](https://www.github.com/grain-lang/grain/issues/845)) ([4fea67b](https://www.github.com/grain-lang/grain/commit/4fea67bc31192e694a4e108ca2917fdae0b8da04))
* **compiler:** Remove user manual memory management calls when --no-gc supplied ([#817](https://www.github.com/grain-lang/grain/issues/817)) ([92efd40](https://www.github.com/grain-lang/grain/commit/92efd40264345ddee15acc84899bf3da5bc52b55))
* **compiler:** Universal WebAssembly initial and maximum pages flags ([#668](https://www.github.com/grain-lang/grain/issues/668)) ([ec9c0e2](https://www.github.com/grain-lang/grain/commit/ec9c0e211cf0872f2ac2d52fffd848c74e7a8894))
* **graindoc:** Add support for deprecated attribute ([#751](https://www.github.com/grain-lang/grain/issues/751)) ([8540c73](https://www.github.com/grain-lang/grain/commit/8540c73f26b9ed4cdd1a68ea671ad4b54d5ca0b2))
* **graindoc:** Add support for extra information in module docblock ([#809](https://www.github.com/grain-lang/grain/issues/809)) ([f28d234](https://www.github.com/grain-lang/grain/commit/f28d2340d3c93be267ac6c2b238679c2259f4a7e))
* **graindoc:** Support `[@since](https://www.github.com/since)` and `[@history](https://www.github.com/history)` attributes ([#785](https://www.github.com/grain-lang/grain/issues/785)) ([9386f46](https://www.github.com/grain-lang/grain/commit/9386f46304ad958c29bb099570f06193911f7131))
* **linker:** Add --use-start-section flag to use start section in output ([#844](https://www.github.com/grain-lang/grain/issues/844)) ([c221834](https://www.github.com/grain-lang/grain/commit/c221834f93b897c001d7530e6b4a354fd5c5d17b))
* **linker:** Add better error for failed wasi polyfill import ([3d8f70c](https://www.github.com/grain-lang/grain/commit/3d8f70cc8255075a462d892fdfcfe30d48c599f0))
* **stdlib:** Add Array.reverse() function ([#698](https://www.github.com/grain-lang/grain/issues/698)) ([538e987](https://www.github.com/grain-lang/grain/commit/538e987d9828b0851d0cc14a26c8b5815f012f5b))
* **stdlib:** Add Array.slice function ([#727](https://www.github.com/grain-lang/grain/issues/727)) ([66319ca](https://www.github.com/grain-lang/grain/commit/66319ca8f3bbeb80e18525d1c15e2b84f0abd0c1))
* **stdlib:** Add Array.unzip function ([#699](https://www.github.com/grain-lang/grain/issues/699)) ([cb9b49e](https://www.github.com/grain-lang/grain/commit/cb9b49e5d3110a6c23c72065ee306a5ba7ee27af))
* **stdlib:** Add buffer module ([627f181](https://www.github.com/grain-lang/grain/commit/627f181e38a843d86a52f55b0d0a4fc02b14fc46))
* **stdlib:** Add Float32/Float64 constants for infinity/nan ([#720](https://www.github.com/grain-lang/grain/issues/720)) ([4ff3b9f](https://www.github.com/grain-lang/grain/commit/4ff3b9f99369e9b69a0b29299c89050180bbf8ec))
* **stdlib:** Add List.join and Array.join functions ([#722](https://www.github.com/grain-lang/grain/issues/722)) ([01a64b6](https://www.github.com/grain-lang/grain/commit/01a64b64f2c6e5233f2e1492c75c7531d19f637b))
* **stdlib:** Add Result.expect & Result.unwrap functions ([#808](https://www.github.com/grain-lang/grain/issues/808)) ([c390e61](https://www.github.com/grain-lang/grain/commit/c390e610e3c05eadc42aca57804a4ddb48cccff2))
* **stdlib:** Add String.chatAt function ([#721](https://www.github.com/grain-lang/grain/issues/721)) ([94ffbbe](https://www.github.com/grain-lang/grain/commit/94ffbbe0f84820d282784d62e295796ab865c837))
* **stdlib:** Add String.encode and String.decode functions to standard library ([#683](https://www.github.com/grain-lang/grain/issues/683)) ([5635a36](https://www.github.com/grain-lang/grain/commit/5635a3682e88292e3623157b34323d968f6946c3))
* **stdlib:** Convert sys functions to return Results instead of throwing errors ([#792](https://www.github.com/grain-lang/grain/issues/792)) ([35cd957](https://www.github.com/grain-lang/grain/commit/35cd957d9c04d84d9f12b54cd2882a6bbc67c175))
* **stdlib:** forEachCodePoint and forEachCodePointi ([#766](https://www.github.com/grain-lang/grain/issues/766)) ([b95cfb7](https://www.github.com/grain-lang/grain/commit/b95cfb77fd2f248f611f2b6a55d58d67ee800859))
* **stdlib:** Implement initial Number library ([#687](https://www.github.com/grain-lang/grain/issues/687)) ([4a71209](https://www.github.com/grain-lang/grain/commit/4a7120964a31602f763b31064e112cfeaa4d1d38))
* **stdlib:** Int32/64 clz, ctz, popcnt, rotl, rotr, eq, ne, eqz ([#807](https://www.github.com/grain-lang/grain/issues/807)) ([ffddc51](https://www.github.com/grain-lang/grain/commit/ffddc512d2c2879b76486f9d7a91621cebb064bd))
* **stdlib:** Number utilities isNaN, isFinite, and isInfinite ([#729](https://www.github.com/grain-lang/grain/issues/729)) ([b907da7](https://www.github.com/grain-lang/grain/commit/b907da7a9e2e7fdbf5d9f376533fbec21458017e))
* **stdlib:** Provide Bytes hash implementation ([#853](https://www.github.com/grain-lang/grain/issues/853)) ([ec7d902](https://www.github.com/grain-lang/grain/commit/ec7d902e900e57a0e3e6fb2a9c16e8c578a1ba47))
* Support mutually recursive data definitions ([#725](https://www.github.com/grain-lang/grain/issues/725)) ([4474f88](https://www.github.com/grain-lang/grain/commit/4474f887514caa35132724ad2c658cc5328807c5))


### Bug Fixes

* Adjust Number equality, thus that 5 == 5.0 ([#726](https://www.github.com/grain-lang/grain/issues/726)) ([04aef16](https://www.github.com/grain-lang/grain/commit/04aef163b862588e5d7f699fc33e0810625ab22a))
* **compiler:** Correct pretty-printing of record types ([#750](https://www.github.com/grain-lang/grain/issues/750)) ([3c0f942](https://www.github.com/grain-lang/grain/commit/3c0f942395aa2417b50bba68e5009510c5a49a36))
* **compiler:** Do not add transitive dependencies as imports during CRC consistency check ([#830](https://www.github.com/grain-lang/grain/issues/830)) ([a1caacc](https://www.github.com/grain-lang/grain/commit/a1caacc523a2af52f5f0f2655211b9257b30af73))
* **compiler:** Don't report comment locations when disabled ([#861](https://www.github.com/grain-lang/grain/issues/861)) ([c6821dd](https://www.github.com/grain-lang/grain/commit/c6821ddb63cf42a04c7ee86bc7b248765f605d1d))
* **compiler:** Fix incorrect char well-formedness error ([#837](https://www.github.com/grain-lang/grain/issues/837)) ([4a201cf](https://www.github.com/grain-lang/grain/commit/4a201cf2829a1721fe1f5e322d45106bd06d51e8))
* **compiler:** Fix location information for parsed toplevel statements ([#801](https://www.github.com/grain-lang/grain/issues/801)) ([36c85f7](https://www.github.com/grain-lang/grain/commit/36c85f7b8aae010401c92ceb2b33224938e10fa7))
* **compiler:** Fix location of toplevel import statement ([#794](https://www.github.com/grain-lang/grain/issues/794)) ([3f94057](https://www.github.com/grain-lang/grain/commit/3f9405754f2b58f32819f7d8809eab1563c4a31a))
* **compiler:** Fix typechecking of single-argument tuple annotation ([#827](https://www.github.com/grain-lang/grain/issues/827)) ([7b1bb58](https://www.github.com/grain-lang/grain/commit/7b1bb58971bf6b714454c1cfce9c0dea673a3670))
* **compiler:** Print parens for unit functions ([#749](https://www.github.com/grain-lang/grain/issues/749)) ([fd88d2e](https://www.github.com/grain-lang/grain/commit/fd88d2e7a82c852ef52f13c3cd58e3fd6fd789df))
* **compiler:** Provide named function export for recursive functions ([#843](https://www.github.com/grain-lang/grain/issues/843)) ([e7d2ff4](https://www.github.com/grain-lang/grain/commit/e7d2ff42d2a6e287ec6152b1a4e7cf2b366658f1))
* **compiler:** Recompile cached modules when compiler flags change ([#820](https://www.github.com/grain-lang/grain/issues/820)) ([50497dd](https://www.github.com/grain-lang/grain/commit/50497dd62aa02043f1f7bf1a5cc3ea74e7c080b6))
* **compiler:** Refactor function return value incRef logic ([#765](https://www.github.com/grain-lang/grain/issues/765)) ([97fbe3d](https://www.github.com/grain-lang/grain/commit/97fbe3dd6615b07db249eae5946a6dfeebcec3c0))
* **compiler:** Refactor module resolution in order to properly resolve relative paths ([#797](https://www.github.com/grain-lang/grain/issues/797)) ([97cd97d](https://www.github.com/grain-lang/grain/commit/97cd97d2126297044391c92d791846932eef1ded))
* **compiler:** Return full string locations from parsing ([#798](https://www.github.com/grain-lang/grain/issues/798)) ([392eabf](https://www.github.com/grain-lang/grain/commit/392eabf6c1fa6963d7d08f95549e1a213e7acb03))
* **compiler:** Strip leading line asterisks in block and doc comments ([#740](https://www.github.com/grain-lang/grain/issues/740)) ([45d8564](https://www.github.com/grain-lang/grain/commit/45d85644e2b686462ef58dcee608a19058c4a2d1))
* else clause on new line ([#678](https://www.github.com/grain-lang/grain/issues/678)) ([13b01c9](https://www.github.com/grain-lang/grain/commit/13b01c9df77d261e55639ae10ded50a922485894))
* **graindoc:** Added linker fixes for Windows ([#824](https://www.github.com/grain-lang/grain/issues/824)) ([41dc6c8](https://www.github.com/grain-lang/grain/commit/41dc6c824055cec9118015dd3d5c8b8e8af65305))
* **graindoc:** Correctly order sections from source code ([#748](https://www.github.com/grain-lang/grain/issues/748)) ([61da929](https://www.github.com/grain-lang/grain/commit/61da9296e3c9b61181acb82c0c09cf7b197d6ef6))
* **graindoc:** Correctly parse attributes, no matter the order ([#784](https://www.github.com/grain-lang/grain/issues/784)) ([4a1b280](https://www.github.com/grain-lang/grain/commit/4a1b2807cb76a3cedc40e66a4ce6442cd87ef7ef))
* **graindoc:** Ensure argument-less variants print with trailing comma ([#810](https://www.github.com/grain-lang/grain/issues/810)) ([d0cc615](https://www.github.com/grain-lang/grain/commit/d0cc6153fc1187e509e1e4356bb054f2776dc8e7))
* **grainfmt:** Correctly add space between arguments in enum pattern ([#865](https://www.github.com/grain-lang/grain/issues/865)) ([8c647fa](https://www.github.com/grain-lang/grain/commit/8c647fab856a15f06ebfe9e0cc75e0f090ed707d))
* **grainfmt:** Handle trailing block comments better ([#860](https://www.github.com/grain-lang/grain/issues/860)) ([f15f37b](https://www.github.com/grain-lang/grain/commit/f15f37ba1b1df776d868e0dfccf0bb8d42ada2a7))
* **grainfmt:** Use Doc.space and surround trailing type annotations with parens ([#866](https://www.github.com/grain-lang/grain/issues/866)) ([7bad8e2](https://www.github.com/grain-lang/grain/commit/7bad8e2afb00b3756c191acc2181acefd28ed7fc))
* **lsp:** Pattern-match the phase to match the stop hook ([#760](https://www.github.com/grain-lang/grain/issues/760)) ([c4da4d9](https://www.github.com/grain-lang/grain/commit/c4da4d938ffa15a21dbdef7b2648b37e2278b079))
* Properly initialize malloc free list ([#700](https://www.github.com/grain-lang/grain/issues/700)) ([0576fd9](https://www.github.com/grain-lang/grain/commit/0576fd9be8dbfecc5d7081731a1c5bedee32f9f1))
* Record function-call associativity ([#701](https://www.github.com/grain-lang/grain/issues/701)) ([a6ca63f](https://www.github.com/grain-lang/grain/commit/a6ca63fd8b3830b076d649e8c78cdde7b86507ca))
* **runtime:** Fix bug in equalHelp preventing simpleNum/boxedNum equality checks from happening correctly ([04aef16](https://www.github.com/grain-lang/grain/commit/04aef163b862588e5d7f699fc33e0810625ab22a))
* **stdlib:** Avoid `Pervasives.(!=)` references on WasmI32 values ([#759](https://www.github.com/grain-lang/grain/issues/759)) ([0f42544](https://www.github.com/grain-lang/grain/commit/0f42544597c628454bede14fd82542331211dab8))
* **stdlib:** Correctly indent nested record braces when printing ([#724](https://www.github.com/grain-lang/grain/issues/724)) ([05f795d](https://www.github.com/grain-lang/grain/commit/05f795dcbf77642070eae1613492d701d7b78113))
* **stdlib:** Fixed String.writeUtf8CodePoint for two byte sequences. ([#786](https://www.github.com/grain-lang/grain/issues/786)) ([20b1a77](https://www.github.com/grain-lang/grain/commit/20b1a770fe4dd5302a92c763c4748fd501b41c96))
* **stdlib:** Premature free in toString ([#863](https://www.github.com/grain-lang/grain/issues/863)) ([eec0f09](https://www.github.com/grain-lang/grain/commit/eec0f092987cd6c9ddd7519b139596611dd84d7c))
* **stdlib:** Remove fd_sync calls that are not used ([#846](https://www.github.com/grain-lang/grain/issues/846)) ([3d8f70c](https://www.github.com/grain-lang/grain/commit/3d8f70cc8255075a462d892fdfcfe30d48c599f0))
* **stdlib:** Respect callee-owned convention in sys/xx modules ([35cd957](https://www.github.com/grain-lang/grain/commit/35cd957d9c04d84d9f12b54cd2882a6bbc67c175))
* **tests:** Fix mistaken `==` in WasmI32 tests ([0f42544](https://www.github.com/grain-lang/grain/commit/0f42544597c628454bede14fd82542331211dab8))


### Miscellaneous Chores

* **compiler:** Avoid building JS bundles by default ([#739](https://www.github.com/grain-lang/grain/issues/739)) ([2925fb8](https://www.github.com/grain-lang/grain/commit/2925fb8149cb09659e0267a581f0390b73b0f119))
* **compiler:** Rename `grainRuntime` to `_grainEnv` ([2d99c5a](https://www.github.com/grain-lang/grain/commit/2d99c5ab8fa527328f18d471e8a9128561af7056))
* Refactor out references to "runtime" that aren't related to `stdlib/runtime` ([#755](https://www.github.com/grain-lang/grain/issues/755)) ([2d99c5a](https://www.github.com/grain-lang/grain/commit/2d99c5ab8fa527328f18d471e8a9128561af7056))
* Rename JS "runtime" to "js-runner"/"runner" throughout project ([2d99c5a](https://www.github.com/grain-lang/grain/commit/2d99c5ab8fa527328f18d471e8a9128561af7056))

### [0.3.2](https://www.github.com/grain-lang/grain/compare/@grain/compiler-v0.3.1...@grain/compiler-v0.3.2) (2021-05-29)


### Features

* **compiler:** add better hover to LSP ([#667](https://www.github.com/grain-lang/grain/issues/667)) ([7c7e9d7](https://www.github.com/grain-lang/grain/commit/7c7e9d7a568cc350f6323a8941858c04bb46cac2))
* **compiler:** Upgrade to binaryen.ml v0.10.0 ([#623](https://www.github.com/grain-lang/grain/issues/623)) ([940f0f1](https://www.github.com/grain-lang/grain/commit/940f0f17bc75e6aa861798a765215db0955bbe72))
* Implement Bytes type and Bytes stdlib ([#633](https://www.github.com/grain-lang/grain/issues/633)) ([4b81898](https://www.github.com/grain-lang/grain/commit/4b81898d4cf17b34dc35e975b228b08b53d81a2a))
* Implement Grain array methods in Grain instead of the compiler ([#660](https://www.github.com/grain-lang/grain/issues/660)) ([b1397fa](https://www.github.com/grain-lang/grain/commit/b1397fae4c49a6b6789cbacc4f14ec6ee484c483))
* Keep track of warnings and report them in LSP mode ([#659](https://www.github.com/grain-lang/grain/issues/659)) ([5144e1b](https://www.github.com/grain-lang/grain/commit/5144e1b2e25c95c195a9ed29676d3bf4f01a81d2))
* Rename Queue/Stack functions & deprecate old identifiers ([#635](https://www.github.com/grain-lang/grain/issues/635)) ([3a8da65](https://www.github.com/grain-lang/grain/commit/3a8da654f2f3a032e3413a44f543b79dc3ee23e1))
* **stdlib:** add `Queue.size` and `Stack.size` ([#647](https://www.github.com/grain-lang/grain/issues/647)) ([82ed533](https://www.github.com/grain-lang/grain/commit/82ed5336c3c5ab0679f5a2db2172084ec08ce411))
* **stdlib:** add Range.map ([#674](https://www.github.com/grain-lang/grain/issues/674)) ([5c33861](https://www.github.com/grain-lang/grain/commit/5c33861d933e6ee0049dadd54e612c2c25d623ca)), closes [#616](https://www.github.com/grain-lang/grain/issues/616)
* Support \b, \f, \t, and \v escapes ([c5d3e44](https://www.github.com/grain-lang/grain/commit/c5d3e44981b4456bbb9c9c63ef5f46721cb4a8c7))


### Bug Fixes

* Always use correct wasm type for throw ([#652](https://www.github.com/grain-lang/grain/issues/652)) ([dcec4e0](https://www.github.com/grain-lang/grain/commit/dcec4e0a9b620248b19b311d952a601e0967a92b))
* **compiler:** Correct reporting of locations for CRLF files ([#658](https://www.github.com/grain-lang/grain/issues/658)) ([457a011](https://www.github.com/grain-lang/grain/commit/457a01183e9234a2190f9e50668305cfcefd8d87))
* **compiler:** properly handle special float equality ([#664](https://www.github.com/grain-lang/grain/issues/664)) ([b63597a](https://www.github.com/grain-lang/grain/commit/b63597a2a0a490015b2af0668894ed9c9afb81ce))
* Fix parsing of '\'' char literal ([c5d3e44](https://www.github.com/grain-lang/grain/commit/c5d3e44981b4456bbb9c9c63ef5f46721cb4a8c7))
* **grainc:** Apply separator normalization before cmdliner util ([#654](https://www.github.com/grain-lang/grain/issues/654)) ([992dfbd](https://www.github.com/grain-lang/grain/commit/992dfbdf3f3c58927eb4eccf125173b9994b4626))
* Properly escape quoted strings in toString/print ([#670](https://www.github.com/grain-lang/grain/issues/670)) ([c5d3e44](https://www.github.com/grain-lang/grain/commit/c5d3e44981b4456bbb9c9c63ef5f46721cb4a8c7))
* **stdlib:** fdWrite range error ([#677](https://www.github.com/grain-lang/grain/issues/677)) ([73e0a7a](https://www.github.com/grain-lang/grain/commit/73e0a7a1d5d393dd4b050275b68b949a14fe9936))

### [0.3.1](https://www.github.com/grain-lang/grain/compare/@grain/compiler-v0.3.0...@grain/compiler-v0.3.1) (2021-04-29)


### ⚠ BREAKING CHANGES

* rename Queue/Stack lib methods (#625)

### Bug Fixes

* Workaround for Windows drive letters with jsoo & pkg ([#636](https://www.github.com/grain-lang/grain/issues/636)) ([d205cde](https://www.github.com/grain-lang/grain/commit/d205cde76c97d9b830653c48004b97ec9dc25840))


### Reverts

* "chore!: rename Queue/Stack lib methods ([#625](https://www.github.com/grain-lang/grain/issues/625))" ([#632](https://www.github.com/grain-lang/grain/issues/632)) ([c9af90e](https://www.github.com/grain-lang/grain/commit/c9af90e77e5c5bb9649b8072b9cdff16ae2d1e60))


### Miscellaneous Chores

* rename Queue/Stack lib methods ([#625](https://www.github.com/grain-lang/grain/issues/625)) ([8117592](https://www.github.com/grain-lang/grain/commit/8117592dbaa09b4443003f234b6a2dcadb235a8b))

## [0.3.0](https://www.github.com/grain-lang/grain/compare/@grain/compiler-v0.2.0...@grain/compiler-v0.3.0) (2021-04-21)


### ⚠ BREAKING CHANGES

* **cli:** Remove `-f` CLI option
* **compiler:** Remove --principal-types and --recursive-types compiler flags
* **compiler:** Rename --cdebug to --verbose
* **cli:** Pass compiler flags from the CLI to grainc directly (#613)
* Correct type signature for `_start`
* Introduce `_gmain` for old behavior of `_start`
* Tail calls must be enabled explicitly via `--experimental-wasm-tail-call`
* Static linking of Grain modules (#584)
* Rename `registerBasePrinter` to `dangerouslyRegisterBasePrinter` in runtime/exception
* Use Grain exceptions instead of JS exceptions (#565)
* Update assignment semantics (#560)
* Grain implementation of memory allocator (#530)
* Refactor value tags (#526)
* **stdlib:** Ensure string methods are data-last (#494)
* **runtime:** Print strings without quotes (#495)
* Char literals (#477)
* Char (#474)
* convert fallible List methods to return Option (#460)
* change return type of Array.find/findIndex to Option (#459)
* upgrade binaryen to 0.4.0 & update test invocation to work with it (#435)
* Add support for bitwise operators (#425)
* remove `^` as unbox operator (fixes #183) (#426)

### Features

* `for` loops ([#525](https://www.github.com/grain-lang/grain/issues/525)) ([1032a3d](https://www.github.com/grain-lang/grain/commit/1032a3d4e92e8947b6406147e8992b9a0dd5b9e2))
* Add ability to `throw` exceptions ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))
* Add ability to register custom exception printers ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))
* add command line option to disable garbage collector ([#239](https://www.github.com/grain-lang/grain/issues/239)) ([7f27a0a](https://www.github.com/grain-lang/grain/commit/7f27a0a5b700eff0a0a80401262286a4762f8c44))
* Add Exception stdlib with Exception.registerPrinter ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))
* add initial Range stdlib module ([#456](https://www.github.com/grain-lang/grain/issues/456)) ([508f23c](https://www.github.com/grain-lang/grain/commit/508f23c32ad9b713843b3fb35dc3c9ac308303e9))
* add List.init to stdlib ([#465](https://www.github.com/grain-lang/grain/issues/465)) ([6b833d8](https://www.github.com/grain-lang/grain/commit/6b833d839deaa81c9d755ebf2da61e36688cd8ee))
* add reduce/flatMap/every/some to Array stdlib ([#455](https://www.github.com/grain-lang/grain/issues/455)) ([51a7a80](https://www.github.com/grain-lang/grain/commit/51a7a80b92ebe5e369660b293dd1d954aea56b9b))
* add reducei/counti/filter/filteri/unique to Array stdlib ([#473](https://www.github.com/grain-lang/grain/issues/473)) ([3e70cd5](https://www.github.com/grain-lang/grain/commit/3e70cd5c70aa12f14b6de1381f7cb38e88bbb890))
* add Set stdlib module ([#466](https://www.github.com/grain-lang/grain/issues/466)) ([2eb2604](https://www.github.com/grain-lang/grain/commit/2eb2604b1eeac6cf9f1d9cc6a6e11772bfc956c8))
* Add support for bitwise operators ([#425](https://www.github.com/grain-lang/grain/issues/425)) ([5c2b8bf](https://www.github.com/grain-lang/grain/commit/5c2b8bf0e97db35c98918e115e9d7cc48425fded))
* add support for LSP lenses ([#416](https://www.github.com/grain-lang/grain/issues/416)) ([753a0dc](https://www.github.com/grain-lang/grain/commit/753a0dc7fa02766a930238077a5deea66b0bf158))
* add update method to map stdlib ([#421](https://www.github.com/grain-lang/grain/issues/421)) ([292b5f9](https://www.github.com/grain-lang/grain/commit/292b5f9bbf1ea84b0540fe828fb1e3dba712340f))
* Allow newline when creating array ([#447](https://www.github.com/grain-lang/grain/issues/447)) ([114cb1a](https://www.github.com/grain-lang/grain/commit/114cb1a85b55086bb72e95ab886ff93eaf5d7162))
* Allow pattern matching on unsafe wasm i32s ([#532](https://www.github.com/grain-lang/grain/issues/532)) ([8839c55](https://www.github.com/grain-lang/grain/commit/8839c5542718f35d10f3958559862faab506a0b6))
* Allow unsafe wasm values in globals ([#519](https://www.github.com/grain-lang/grain/issues/519)) ([111b549](https://www.github.com/grain-lang/grain/commit/111b549dfcb0645157dc4a3be54665d7c96debfd))
* Better lenses and hover for LSP ([#501](https://www.github.com/grain-lang/grain/issues/501)) ([1223e9a](https://www.github.com/grain-lang/grain/commit/1223e9a453e3e248e55fc5b8ba2ee797ffd44224))
* change return type of Array.find/findIndex to Option ([#459](https://www.github.com/grain-lang/grain/issues/459)) ([aa3767d](https://www.github.com/grain-lang/grain/commit/aa3767d5d429b33c54cec458f916715f931f9bc2))
* Char ([#474](https://www.github.com/grain-lang/grain/issues/474)) ([c9422f8](https://www.github.com/grain-lang/grain/commit/c9422f89573cc94081d2dd5f9e11ee17ec475668))
* Char literals ([#477](https://www.github.com/grain-lang/grain/issues/477)) ([cf7eaa5](https://www.github.com/grain-lang/grain/commit/cf7eaa55d239fc7a524a85dc2d53bcfb233e8061))
* **cli:** Pass compiler flags from the CLI to grainc directly ([#613](https://www.github.com/grain-lang/grain/issues/613)) ([8f40383](https://www.github.com/grain-lang/grain/commit/8f40383af23b236b0333032bba193c39757d2569))
* Compile grainc to JS & create binaries with pkg ([#570](https://www.github.com/grain-lang/grain/issues/570)) ([f4919bd](https://www.github.com/grain-lang/grain/commit/f4919bdbab7dddd433b3f53bf8a8536a7efd5b03))
* convert fallible List methods to return Option ([#460](https://www.github.com/grain-lang/grain/issues/460)) ([a08768e](https://www.github.com/grain-lang/grain/commit/a08768e85e94288c261746ede40842c833438faa))
* Enable Binaryen optimizations ([#556](https://www.github.com/grain-lang/grain/issues/556)) ([50ebf74](https://www.github.com/grain-lang/grain/commit/50ebf74f98dedad89399424841de1427a8ac10ef))
* Export compiled functions from modules ([#529](https://www.github.com/grain-lang/grain/issues/529)) ([e302049](https://www.github.com/grain-lang/grain/commit/e3020493cda821e8847e4517e4d2d137f7d24a7c))
* Grain implementation of Char and String libraries ([#559](https://www.github.com/grain-lang/grain/issues/559)) ([7424cc5](https://www.github.com/grain-lang/grain/commit/7424cc5befe5c3416c26c5e943cc064329eac025))
* Grain implementation of equals ([#538](https://www.github.com/grain-lang/grain/issues/538)) ([09617bc](https://www.github.com/grain-lang/grain/commit/09617bca84e503935ef33e3f7f523a0fce7c4f17))
* Grain implementation of hashing ([#557](https://www.github.com/grain-lang/grain/issues/557)) ([40723fc](https://www.github.com/grain-lang/grain/commit/40723fca645f147b98eea83ffc11bd53359ce29a))
* Grain implementation of memory allocator ([#530](https://www.github.com/grain-lang/grain/issues/530)) ([fd8faaa](https://www.github.com/grain-lang/grain/commit/fd8faaa1425d398c55af36c9cdd77a59cf2eeccf))
* Grain implementation of memory manager ([#534](https://www.github.com/grain-lang/grain/issues/534)) ([cea6dcc](https://www.github.com/grain-lang/grain/commit/cea6dccaf45e8bdd07eb6c674f30c53a50f37a19))
* Grain implementation of number runtime ([#537](https://www.github.com/grain-lang/grain/issues/537)) ([94460c0](https://www.github.com/grain-lang/grain/commit/94460c0aa83c737278f14bde2d2651bf6f8d9798))
* Grain implementation of toString/print ([#540](https://www.github.com/grain-lang/grain/issues/540)) ([8c77905](https://www.github.com/grain-lang/grain/commit/8c779059c4a2a71d0ccacc51d946dde2d48d6623))
* Grain wasi bindings ([#562](https://www.github.com/grain-lang/grain/issues/562)) ([323006b](https://www.github.com/grain-lang/grain/commit/323006b7a3e12194b252bcbad2ef8a3eb8e1f043))
* Implement low-level wasm i32/i64/f32/f64 libraries ([#517](https://www.github.com/grain-lang/grain/issues/517)) ([721d011](https://www.github.com/grain-lang/grain/commit/721d011abeb79b86516bbb145e5424abfc11b1d4))
* Implement String.indexOf, String.explode, and String.split ([#450](https://www.github.com/grain-lang/grain/issues/450)) ([6dc5466](https://www.github.com/grain-lang/grain/commit/6dc54666db1417ce4afa998c663754089d497676))
* Implement support for printing in Grain ([#561](https://www.github.com/grain-lang/grain/issues/561)) ([bfe471c](https://www.github.com/grain-lang/grain/commit/bfe471c668ee5838bde8a307bfc0f5d650a9b594))
* Normalized wasm exports for linked modules ([3d4ac6e](https://www.github.com/grain-lang/grain/commit/3d4ac6edd1b805d7cada5e6fa951bb6afb0467d9))
* Optimize local mutations ([#527](https://www.github.com/grain-lang/grain/issues/527)) ([65a1947](https://www.github.com/grain-lang/grain/commit/65a1947f3f87099a324dd7c00b14ff3affba8a0f))
* Optimize number to string conversions by writing UTF8 directly ([#539](https://www.github.com/grain-lang/grain/issues/539)) ([f18d26e](https://www.github.com/grain-lang/grain/commit/f18d26ef0129440ef5a6a8d072d8bdc1c3990c2f))
* Pattern matching on arrays ([#478](https://www.github.com/grain-lang/grain/issues/478)) ([681dddf](https://www.github.com/grain-lang/grain/commit/681dddfc0dbf4d98523574e8ac334b4635bef5e1))
* Static linking of Grain modules ([#584](https://www.github.com/grain-lang/grain/issues/584)) ([3d4ac6e](https://www.github.com/grain-lang/grain/commit/3d4ac6edd1b805d7cada5e6fa951bb6afb0467d9))
* statically link MinGW libraries on Windows ([#438](https://www.github.com/grain-lang/grain/issues/438)) ([a154ac3](https://www.github.com/grain-lang/grain/commit/a154ac384d19768a3d30f5ace3898f390dcee20f))
* **stdlib:** add Stack module ([#491](https://www.github.com/grain-lang/grain/issues/491)) ([8310d33](https://www.github.com/grain-lang/grain/commit/8310d337cb8c99135b9eb00e6bc9634b896762c7))
* **stdlib:** Implement Char toString ([#481](https://www.github.com/grain-lang/grain/issues/481)) ([37ba683](https://www.github.com/grain-lang/grain/commit/37ba683bb177a59ae43101a7abac5f8dc0e935c4))
* **stdlib:** String.implode ([#489](https://www.github.com/grain-lang/grain/issues/489)) ([045077b](https://www.github.com/grain-lang/grain/commit/045077bedbc5585bc26385af3750e50b0e055852))
* Support for more WebAssembly runtimes, including Wasmtime and Wasmer ([3d4ac6e](https://www.github.com/grain-lang/grain/commit/3d4ac6edd1b805d7cada5e6fa951bb6afb0467d9))
* Update assignment semantics ([#560](https://www.github.com/grain-lang/grain/issues/560)) ([03a3217](https://www.github.com/grain-lang/grain/commit/03a3217fb7bc755407917991e0ebac3a66ea071e))
* upgrade binaryen to 0.4.0 & update test invocation to work with it ([#435](https://www.github.com/grain-lang/grain/issues/435)) ([fbf52a8](https://www.github.com/grain-lang/grain/commit/fbf52a82636e9d823a0a6ee1767eac16f7030f82))
* Upgrade to Binaryen 100 ([#541](https://www.github.com/grain-lang/grain/issues/541)) ([6d1fc53](https://www.github.com/grain-lang/grain/commit/6d1fc5394d52135a4ca483d4d1d089e0d1198b7f))
* Use Grain exceptions instead of JS exceptions ([#565](https://www.github.com/grain-lang/grain/issues/565)) ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))
* Use real wasm tail call instruction ([#510](https://www.github.com/grain-lang/grain/issues/510)) ([9c9ffe4](https://www.github.com/grain-lang/grain/commit/9c9ffe48d78ed315f0a406d704c7a0fdbb116f1f))


### Bug Fixes

* `export *` with exceptions ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))
* Allow bodiless blocks ([#444](https://www.github.com/grain-lang/grain/issues/444)) ([ecfc70f](https://www.github.com/grain-lang/grain/commit/ecfc70fb1af70ce9fe465d20807275853a099756))
* and/or short circuiting ([#536](https://www.github.com/grain-lang/grain/issues/536)) ([38ad495](https://www.github.com/grain-lang/grain/commit/38ad4952be827d289145c75ec71cb383221ee822))
* **cli:** Rework JSOO hacks to ensure stdin always gets data ([#614](https://www.github.com/grain-lang/grain/issues/614)) ([b064119](https://www.github.com/grain-lang/grain/commit/b064119fe2a10dca68e30f4d66db455d37991ead))
* **compiler:** Always get little endian i64 from bytes ([#587](https://www.github.com/grain-lang/grain/issues/587)) ([1b9853a](https://www.github.com/grain-lang/grain/commit/1b9853ad3ebc91c1022efbf484532254c67c845e))
* **compiler:** Implement proper dependency resolver ([#590](https://www.github.com/grain-lang/grain/issues/590)) ([d9ce392](https://www.github.com/grain-lang/grain/commit/d9ce392341acebd0b46a0e6fe78469e23e73ee92))
* **compiler:** Resolve names when doing CRC checks ([#615](https://www.github.com/grain-lang/grain/issues/615)) ([15ea9a1](https://www.github.com/grain-lang/grain/commit/15ea9a18c0c5a8762a4e232720a2654c33881dac))
* **compiler:** Update binaryen.ml bindings to include JS fixes ([#600](https://www.github.com/grain-lang/grain/issues/600)) ([7bd9ac6](https://www.github.com/grain-lang/grain/commit/7bd9ac69a1ba9c9506e8d65b1f51831a63fdf294))
* Correct type signature for `_start` ([3d4ac6e](https://www.github.com/grain-lang/grain/commit/3d4ac6edd1b805d7cada5e6fa951bb6afb0467d9))
* Don't optimize away expressions which could possibly contain side effects ([#523](https://www.github.com/grain-lang/grain/issues/523)) ([acc7d65](https://www.github.com/grain-lang/grain/commit/acc7d655621a1397481a10e8840c547ae5690d2b))
* **grainc:** Only print compiler backtraces in debug mode ([#588](https://www.github.com/grain-lang/grain/issues/588)) ([f5145ff](https://www.github.com/grain-lang/grain/commit/f5145ff5eb931ec017228c08d6516384dae6b0e5))
* loop body typechecking ([#533](https://www.github.com/grain-lang/grain/issues/533)) ([18e68ea](https://www.github.com/grain-lang/grain/commit/18e68eaf8f617c3cafa8d7aa5427ba01bead7172))
* Multiline char locs ([#484](https://www.github.com/grain-lang/grain/issues/484)) ([1e4a691](https://www.github.com/grain-lang/grain/commit/1e4a6919568f554bd1f1b5b9adc5ade9c8ff42b8))
* Multiline string locs ([#482](https://www.github.com/grain-lang/grain/issues/482)) ([d3eccee](https://www.github.com/grain-lang/grain/commit/d3ecceefaf49b8e3ffe9d6421f875268b4fecd85))
* normalize filesystem paths to always use / separator ([#433](https://www.github.com/grain-lang/grain/issues/433)) ([084a485](https://www.github.com/grain-lang/grain/commit/084a485570d69b2600ed0eb2da37dacb5d18edaf))
* One-sided if results in any type ([#531](https://www.github.com/grain-lang/grain/issues/531)) ([d85659b](https://www.github.com/grain-lang/grain/commit/d85659b7d1b6d3fa8f0dc77ac515695e173660f7))
* Only backpatch closures for let-bindings ([#428](https://www.github.com/grain-lang/grain/issues/428)) ([98a0b0b](https://www.github.com/grain-lang/grain/commit/98a0b0b92a22cd37d753d09988ef5032526c5ab5))
* Parsing of conditional assignment with binary operators ([#572](https://www.github.com/grain-lang/grain/issues/572)) ([17fd4fa](https://www.github.com/grain-lang/grain/commit/17fd4fa7e2d14d0392c98924625248ed6fd20b9a))
* Properly incRef exception printers ([#581](https://www.github.com/grain-lang/grain/issues/581)) ([61c2a94](https://www.github.com/grain-lang/grain/commit/61c2a94dbe38ff074e6b53395c403d30996b60a0))
* remove `^` as unbox operator (fixes [#183](https://www.github.com/grain-lang/grain/issues/183)) ([#426](https://www.github.com/grain-lang/grain/issues/426)) ([08b6b05](https://www.github.com/grain-lang/grain/commit/08b6b057a98a6a81c10d0856e27735dc6d5bdef0))
* Remove ppx_expect entirely to build on Windows ([#609](https://www.github.com/grain-lang/grain/issues/609)) ([a234dd8](https://www.github.com/grain-lang/grain/commit/a234dd8e027bbdb0f5dbf66bcf09fe8298a6ef7d))
* Reset type vars when printing types ([#420](https://www.github.com/grain-lang/grain/issues/420)) ([bb17787](https://www.github.com/grain-lang/grain/commit/bb17787ce8b183569053b51f7d0ef0e88a1550bd))
* **runtime:** Properly decRef record and array values ([#500](https://www.github.com/grain-lang/grain/issues/500)) ([88adb94](https://www.github.com/grain-lang/grain/commit/88adb942ecf5425b74540a9cddbcb775128d97eb))
* **runtime:** Return correct pointer to morecore ([#602](https://www.github.com/grain-lang/grain/issues/602)) ([af39cc4](https://www.github.com/grain-lang/grain/commit/af39cc46c8e12d869aaf5c42c0a3fd2afbc61638))
* set test sharding to 1 in attempt to avoid test race condition ([#440](https://www.github.com/grain-lang/grain/issues/440)) ([3783ec6](https://www.github.com/grain-lang/grain/commit/3783ec62efbe5b7b76d14cd070e17e99f7737cca))
* temporarily disable inline tests ([#434](https://www.github.com/grain-lang/grain/issues/434)) ([0bc446a](https://www.github.com/grain-lang/grain/commit/0bc446a4149942416e8d439a223c0d39f7c028d4))
* Throwing an exception now traps immediately in all cases ([61c2a94](https://www.github.com/grain-lang/grain/commit/61c2a94dbe38ff074e6b53395c403d30996b60a0))
* Tidy up type printing ([#417](https://www.github.com/grain-lang/grain/issues/417)) ([14c7bd6](https://www.github.com/grain-lang/grain/commit/14c7bd6cbfa497e4a0f691288415865e865beb7d))
* Use Is instead of Eq for match variant comparison ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))
* Use proper return type for calls to external functions ([3d4ac6e](https://www.github.com/grain-lang/grain/commit/3d4ac6edd1b805d7cada5e6fa951bb6afb0467d9))
* word-size independent LEB input byte checking ([#567](https://www.github.com/grain-lang/grain/issues/567)) ([4079824](https://www.github.com/grain-lang/grain/commit/407982444aea158dccfd226d64cb94b3b60ba938))


### Miscellaneous Chores

* **cli:** Remove `-f` CLI option ([8f40383](https://www.github.com/grain-lang/grain/commit/8f40383af23b236b0333032bba193c39757d2569))
* **compiler:** Remove --principal-types and --recursive-types compiler flags ([8f40383](https://www.github.com/grain-lang/grain/commit/8f40383af23b236b0333032bba193c39757d2569))
* **compiler:** Rename --cdebug to --verbose ([8f40383](https://www.github.com/grain-lang/grain/commit/8f40383af23b236b0333032bba193c39757d2569))
* Introduce `_gmain` for old behavior of `_start` ([3d4ac6e](https://www.github.com/grain-lang/grain/commit/3d4ac6edd1b805d7cada5e6fa951bb6afb0467d9))
* Refactor value tags ([#526](https://www.github.com/grain-lang/grain/issues/526)) ([9b21d22](https://www.github.com/grain-lang/grain/commit/9b21d22e403f37d0cfb89891f9c31f40f03ae061))
* Rename `registerBasePrinter` to `dangerouslyRegisterBasePrinter` in runtime/exception ([61c2a94](https://www.github.com/grain-lang/grain/commit/61c2a94dbe38ff074e6b53395c403d30996b60a0))
* **runtime:** Print strings without quotes ([#495](https://www.github.com/grain-lang/grain/issues/495)) ([16671c2](https://www.github.com/grain-lang/grain/commit/16671c2448044bfc20eec32800b4a49ee6f2bb4e))
* **stdlib:** Ensure string methods are data-last ([#494](https://www.github.com/grain-lang/grain/issues/494)) ([f871efc](https://www.github.com/grain-lang/grain/commit/f871efc8943c640c377fae369d7704844d3f86a4))
* Tail calls must be enabled explicitly via `--experimental-wasm-tail-call` ([3d4ac6e](https://www.github.com/grain-lang/grain/commit/3d4ac6edd1b805d7cada5e6fa951bb6afb0467d9))
