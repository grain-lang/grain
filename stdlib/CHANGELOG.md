# Changelog

### [0.5.3](https://github.com/grain-lang/grain/compare/stdlib-v0.5.2...stdlib-v0.5.3) (2022-08-05)


### Features

* Implement Pervasives.compare ([#1399](https://github.com/grain-lang/grain/issues/1399)) ([ebd87e4](https://github.com/grain-lang/grain/commit/ebd87e4308a8950fd95f060ebc446833b064237c))
* **stdlib:** Add `charCodeAt` function to String module ([#1376](https://github.com/grain-lang/grain/issues/1376)) ([c3abbc9](https://github.com/grain-lang/grain/commit/c3abbc991c8b05e3de20e670d2bc3e491feebf8a))
* **stdlib:** Add `lastIndexOf` function to String module ([#1372](https://github.com/grain-lang/grain/issues/1372)) ([b73d9bf](https://github.com/grain-lang/grain/commit/b73d9bf9ff3291b83e6f4263e392395d04dc9995))
* **stdlib:** Implement `isFloat`, `isInteger` & `isRational` in Number module ([#1393](https://github.com/grain-lang/grain/issues/1393)) ([0af0669](https://github.com/grain-lang/grain/commit/0af066993a2b80e417d2c625b27fd11cb1f1f55f))
* **stdlib:** Implement List.zip, List.unzip, List.zipWith, Array.reduceRight, Array.zipWith ([#1363](https://github.com/grain-lang/grain/issues/1363)) ([3e7c147](https://github.com/grain-lang/grain/commit/3e7c147fea2d2fb8b7c5a3d6b3eb1453f2861e36))
* **stdlib:** Implement mutable/immutable priority queues ([#1397](https://github.com/grain-lang/grain/issues/1397)) ([244be1b](https://github.com/grain-lang/grain/commit/244be1b7254caf0b451902ac56413382eb196747))
* **stdlib:** Marshal ([#1352](https://github.com/grain-lang/grain/issues/1352)) ([d659de2](https://github.com/grain-lang/grain/commit/d659de2d92260f7726164876827c639bfd9d0590))


### Bug Fixes

* **compiler:** Correctly handle underscores in bigint literals ([0af0669](https://github.com/grain-lang/grain/commit/0af066993a2b80e417d2c625b27fd11cb1f1f55f))
* **graindoc:** Use defined module name throughout generated doc ([#1406](https://github.com/grain-lang/grain/issues/1406)) ([c33a777](https://github.com/grain-lang/grain/commit/c33a777a93d5e40a081991db5e6ea61ade4fbabc))
* **stdlib:** Correctly promote numbers to bigints when left-shifting ([#1354](https://github.com/grain-lang/grain/issues/1354)) ([5280e98](https://github.com/grain-lang/grain/commit/5280e98a91a57fae074299fc7bad3c41f69fa2a3))
* **stdlib:** Prevent addBytesSlice throwing error on empty buffer ([#1394](https://github.com/grain-lang/grain/issues/1394)) ([bdd4be4](https://github.com/grain-lang/grain/commit/bdd4be46730290908b7b939f41679acce7834167))

### [0.5.2](https://github.com/grain-lang/grain/compare/stdlib-v0.5.1...stdlib-v0.5.2) (2022-06-29)


### Features

* **stdlib:** Add number constants to number libraries ([#1331](https://github.com/grain-lang/grain/issues/1331)) ([f640ec2](https://github.com/grain-lang/grain/commit/f640ec20aa507c83c9cde290b911d0adcb4e8254))
* **stdlib:** Implement Number.sin and Number.cos ([#1343](https://github.com/grain-lang/grain/issues/1343)) ([9357126](https://github.com/grain-lang/grain/commit/93571267b7df53e1cb9f61eaebf8748885e7392c))


### Bug Fixes

* **stdlib:** Make toNumber functions respect Number invariants ([#1347](https://github.com/grain-lang/grain/issues/1347)) ([78db882](https://github.com/grain-lang/grain/commit/78db8820cf5667a4d6737c9109f4223c1348b245))

### [0.5.1](https://github.com/grain-lang/grain/compare/stdlib-v0.5.0...stdlib-v0.5.1) (2022-06-08)


### Miscellaneous Chores

* **stdlib:** Synchronize Grain versions

## [0.5.0](https://github.com/grain-lang/grain/compare/stdlib-v0.4.6...stdlib-v0.5.0) (2022-06-05)


### ⚠ BREAKING CHANGES

* **stdlib:** Use explicit exports for Pervasives (#1301)
* **stdlib:** Remove `sum` function from the List module (#1300)
* **stdlib:** Ensure Void return for forEach functions in List module (#1307)
* **stdlib:** Provide correct types for BigInt operations (#1297)
* **grainfmt:** Replace `--in-place` flag with `-o` flag
* **grainfmt:** Remove stdin formatting support
* **compiler:** Arbitrary-Precision Integer Arithmetic (#1167)
* Drop node 14 support (#1092)
* **stdlib:** Add explicit void return type on Set.forEach (#1225)
* **stdlib:** Add explicit void return type on Map.forEach (#1220)
* **stdlib:** Add type aliases to regex lib (#1036)
* **compiler:** Stack-allocated Chars (#1103)
* **stdlib:** Align Buffer's `addStringSlice` API with String's `slice` (#1136)
* **graindoc:** Add `--current-version` flag, required for since/history attributes (#1116)
* **compiler:** Remove decRefIgnoreZeros (#1068)
* **compiler:** Add `--memory-base` flag (#1115)
* **compiler:** Re-implement Grain parser (#1033)

### Features

* **compiler:** Add `--memory-base` flag ([#1115](https://github.com/grain-lang/grain/issues/1115)) ([0680826](https://github.com/grain-lang/grain/commit/068082663c4387c3ab54c052869e9b9a06b87e26))
* **compiler:** Arbitrary-Precision Integer Arithmetic ([#1167](https://github.com/grain-lang/grain/issues/1167)) ([6f34de2](https://github.com/grain-lang/grain/commit/6f34de214b28358ea1df553685fa3a19336ddba9))
* **compiler:** Call known functions across module boundaries ([#1175](https://github.com/grain-lang/grain/issues/1175)) ([b2d7440](https://github.com/grain-lang/grain/commit/b2d744034ec7e0601554531c910e9d0f5451d464))
* **compiler:** Convert `runtime/dataStructures.gr` to primitives ([#1145](https://github.com/grain-lang/grain/issues/1145)) ([2d43b28](https://github.com/grain-lang/grain/commit/2d43b286141df75f6b92300e48d2bc4804014872))
* **compiler:** Convert Sys libraries to [@unsafe](https://github.com/unsafe) ([#1272](https://github.com/grain-lang/grain/issues/1272)) ([fcdfc2a](https://github.com/grain-lang/grain/commit/fcdfc2a815be889d5f0424a04dd5b2373dbd983b))
* **compiler:** Don't close over global values ([#1134](https://github.com/grain-lang/grain/issues/1134)) ([e8caec6](https://github.com/grain-lang/grain/commit/e8caec6c2a5892e955c8827b18d8d436bebe6073))
* **compiler:** Re-implement Grain parser ([#1033](https://github.com/grain-lang/grain/issues/1033)) ([9dc3c96](https://github.com/grain-lang/grain/commit/9dc3c96f87a0b2affe9db36e1b03360d198f79f1))
* **compiler:** Stack-allocated Chars ([#1103](https://github.com/grain-lang/grain/issues/1103)) ([095385e](https://github.com/grain-lang/grain/commit/095385e7c67bbc7a417a21acaf6f1c498c75ce63))
* **graindoc:** Add `--current-version` flag, required for since/history attributes ([#1116](https://github.com/grain-lang/grain/issues/1116)) ([0f681ea](https://github.com/grain-lang/grain/commit/0f681ea140749395f3ce99a460f30778537183ac))
* **graindoc:** Allow directory input & output ([#1263](https://github.com/grain-lang/grain/issues/1263)) ([d4cb8ab](https://github.com/grain-lang/grain/commit/d4cb8abcb4accafeb3cae0bac77eee9a365e464d))
* **grainfmt:** Allow directory input & output ([#1274](https://github.com/grain-lang/grain/issues/1274)) ([d3e7a33](https://github.com/grain-lang/grain/commit/d3e7a33b01352a9c2bcc3b17a5b2995451d92221))
* **grainfmt:** Replace `--in-place` flag with `-o` flag ([d3e7a33](https://github.com/grain-lang/grain/commit/d3e7a33b01352a9c2bcc3b17a5b2995451d92221))
* **stdlib:** Add module for pseudo-random number generation ([#921](https://github.com/grain-lang/grain/issues/921)) ([db1fa4e](https://github.com/grain-lang/grain/commit/db1fa4e491d35bb582beaba12157884647384a77))
* **stdlib:** Add unsigned versions of Int32/Int64 comparison operations ([#831](https://github.com/grain-lang/grain/issues/831)) ([5f20868](https://github.com/grain-lang/grain/commit/5f20868e7b6e3f52d62c8531d99d1130ca84961e))
* **stdlib:** Added Bytes.clear function. Avoid allocation in Buffer.clear ([#1124](https://github.com/grain-lang/grain/issues/1124)) ([4afd17a](https://github.com/grain-lang/grain/commit/4afd17a91e59027fa7af0fe58977bdbe942f8072))
* **stdlib:** Convert runtime printing utils to [@unsafe](https://github.com/unsafe) ([#1135](https://github.com/grain-lang/grain/issues/1135)) ([403e1d2](https://github.com/grain-lang/grain/commit/403e1d20e2082fe2dcd2721f83f6c2b36d4154bf))


### Bug Fixes

* **cli:** Ensure parent flags are inherited by the format command ([d3e7a33](https://github.com/grain-lang/grain/commit/d3e7a33b01352a9c2bcc3b17a5b2995451d92221))
* **graindoc:** Add parens around infix operators in titles ([#1303](https://github.com/grain-lang/grain/issues/1303)) ([acba9c1](https://github.com/grain-lang/grain/commit/acba9c1757688756c3ca98b22a0a159b8d8f9e7d))
* **graindoc:** Preserve indentation in Doc comments during trim ([#1119](https://github.com/grain-lang/grain/issues/1119)) ([b8a6d57](https://github.com/grain-lang/grain/commit/b8a6d57cce274bfbc2cc16c5b25215b042d4264c))
* **graindoc:** Remove spaces between parens and infix idents ([#1302](https://github.com/grain-lang/grain/issues/1302)) ([95e596f](https://github.com/grain-lang/grain/commit/95e596fa3fdae5a8a0e07d76ff8c11eeab99e8d7))
* **graindoc:** Use value_descriptions and type_declarations defined by the module signature ([#1241](https://github.com/grain-lang/grain/issues/1241)) ([5896242](https://github.com/grain-lang/grain/commit/5896242c324622f3329c144bd2c9642aade9d049))
* **grainfmt:** Remove parens around annotated types ([#1109](https://github.com/grain-lang/grain/issues/1109)) ([0ca66bd](https://github.com/grain-lang/grain/commit/0ca66bd43703826f86ef5b28b49d250af219fb0b))
* **stdlib:** Add explicit void return type on Map.forEach ([#1220](https://github.com/grain-lang/grain/issues/1220)) ([ab2066a](https://github.com/grain-lang/grain/commit/ab2066aadffa8813344d9df7c7a10ec0f76751b6))
* **stdlib:** Add explicit void return type on Set.forEach ([#1225](https://github.com/grain-lang/grain/issues/1225)) ([694e6cf](https://github.com/grain-lang/grain/commit/694e6cf8fcd061a77b2915dc0ea9e9b1ae143ae4))
* **stdlib:** Align Buffer's `addStringSlice` API with String's `slice` ([#1136](https://github.com/grain-lang/grain/issues/1136)) ([0c7cb82](https://github.com/grain-lang/grain/commit/0c7cb820d49cda74598680cc614c0d893b4d2b40))
* **stdlib:** Buffer.toBytes should not expose the raw instance of Bytes used by the buffer ([#1130](https://github.com/grain-lang/grain/issues/1130)) ([d2bb585](https://github.com/grain-lang/grain/commit/d2bb585d3e2092f1331a679f4e84a9500b8c87a5))
* **stdlib:** Ensure Void return for forEach functions in List module ([#1307](https://github.com/grain-lang/grain/issues/1307)) ([31f480c](https://github.com/grain-lang/grain/commit/31f480c22e0a39b347fc3a48e1d0b5bd40c2f19e))
* **stdlib:** Fix float printing in dtoa ([#1165](https://github.com/grain-lang/grain/issues/1165)) ([2987210](https://github.com/grain-lang/grain/commit/2987210648873ab474990ff2b7146a489fecb268))
* **stdlib:** Fixed a memory leak in Buffer.addStringSlice. ([#1122](https://github.com/grain-lang/grain/issues/1122)) ([c4e1911](https://github.com/grain-lang/grain/commit/c4e19110bd9923e58a7953fcbcef18bf07f3efd6))
* **stdlib:** Fixed length and byteLength for strings over 2GiB. ([#1126](https://github.com/grain-lang/grain/issues/1126)) ([08a9487](https://github.com/grain-lang/grain/commit/08a948709270a9aa4ac853c2887bc652c4734f36))
* **stdlib:** Make Bytes.length handle sizes over 2GiB ([#1123](https://github.com/grain-lang/grain/issues/1123)) ([d7386eb](https://github.com/grain-lang/grain/commit/d7386eb36c32452d69a734b447015b4db23f1ac4))
* **stdlib:** Provide correct types for BigInt operations ([#1297](https://github.com/grain-lang/grain/issues/1297)) ([fdd2f1c](https://github.com/grain-lang/grain/commit/fdd2f1c49b938a013d6ae199b5e662cb93d051e6))
* **stdlib:** Remove intermediate resizes in Buffer.autogrow ([#1125](https://github.com/grain-lang/grain/issues/1125)) ([c1695d0](https://github.com/grain-lang/grain/commit/c1695d066ccd40b3118c5c870353addeef67bfc1))
* **stdlib:** Use explicit exports for Pervasives ([#1301](https://github.com/grain-lang/grain/issues/1301)) ([bad5897](https://github.com/grain-lang/grain/commit/bad5897062444ec4d4ace805adcd382725b86125))


### Miscellaneous Chores

* **compiler:** Remove decRefIgnoreZeros ([#1068](https://github.com/grain-lang/grain/issues/1068)) ([3ae8eaa](https://github.com/grain-lang/grain/commit/3ae8eaabad4467304c500c2f0cc9c40749d8513b))
* Drop node 14 support ([#1092](https://github.com/grain-lang/grain/issues/1092)) ([ef4358f](https://github.com/grain-lang/grain/commit/ef4358ff7de14a35edf3e971e04513d497fe1574))
* **grainfmt:** Remove stdin formatting support ([d3e7a33](https://github.com/grain-lang/grain/commit/d3e7a33b01352a9c2bcc3b17a5b2995451d92221))
* **stdlib:** Add type aliases to regex lib ([#1036](https://github.com/grain-lang/grain/issues/1036)) ([a926ea5](https://github.com/grain-lang/grain/commit/a926ea5cafa18e46487a585abbdf5460b15b4f48))
* **stdlib:** Remove `sum` function from the List module ([#1300](https://github.com/grain-lang/grain/issues/1300)) ([9101615](https://github.com/grain-lang/grain/commit/9101615688f20310ae32573f93f36cfcf5c69be1))

### [0.4.6](https://www.github.com/grain-lang/grain/compare/stdlib-v0.4.5...stdlib-v0.4.6) (2022-01-17)


### Bug Fixes

* **grainfmt:** Indent function application args when adding parens ([#1095](https://www.github.com/grain-lang/grain/issues/1095)) ([64af7d3](https://www.github.com/grain-lang/grain/commit/64af7d387dca2fddb9b3d190ccdf5790ec3d8e65))

### [0.4.5](https://www.github.com/grain-lang/grain/compare/stdlib-v0.4.4...stdlib-v0.4.5) (2021-12-31)


### Features

* **stdlib:** Add sign function to Number module ([#1079](https://www.github.com/grain-lang/grain/issues/1079)) ([b6483d5](https://www.github.com/grain-lang/grain/commit/b6483d5046cd1b6b89a717a925594d3b20b05837))

### [0.4.4](https://www.github.com/grain-lang/grain/compare/stdlib-v0.4.3...stdlib-v0.4.4) (2021-12-11)


### Features

* **stdlib:** Add reverse function to String module ([#1027](https://www.github.com/grain-lang/grain/issues/1027)) ([df761db](https://www.github.com/grain-lang/grain/commit/df761db55b3e14e31190090ae008ce5047135c09))
* **stdlib:** Add rotate function in Array module ([#838](https://www.github.com/grain-lang/grain/issues/838)) ([98fc577](https://www.github.com/grain-lang/grain/commit/98fc577ee754317cd2421bfaa8e3c1e049488949))
* **stdlib:** Add sort function to Array module ([#1012](https://www.github.com/grain-lang/grain/issues/1012)) ([9091930](https://www.github.com/grain-lang/grain/commit/9091930344224925bb7b2e1ef6f879c79a5c2f62))
* **stdlib:** Implement List.sort via mergesort ([#1014](https://www.github.com/grain-lang/grain/issues/1014)) ([a076e20](https://www.github.com/grain-lang/grain/commit/a076e200013114ccf16c2e6cbe814af1ec09c1ce))
* **stdlib:** Number.parseInt ([#1051](https://www.github.com/grain-lang/grain/issues/1051)) ([abafb58](https://www.github.com/grain-lang/grain/commit/abafb587e54219a32ed77ba09863bb2d6a80bac8))


### Bug Fixes

* **graindoc:** Ensure value_description is resolved to outcome before printing ([#1070](https://www.github.com/grain-lang/grain/issues/1070)) ([5eb05cc](https://www.github.com/grain-lang/grain/commit/5eb05cc2dedc3b933e194be86dd5d3c3656d6490))
* **stdlib:** Add bounds checking to Buffer addStringSlice & addBytesSlice ([#1065](https://www.github.com/grain-lang/grain/issues/1065)) ([06fe512](https://www.github.com/grain-lang/grain/commit/06fe512e863aeeb855ccf3e3b83bcd3bc8854723))
* **stdlib:** Fix String.encode GC ([#1067](https://www.github.com/grain-lang/grain/issues/1067)) ([0ab38c9](https://www.github.com/grain-lang/grain/commit/0ab38c9f4aa0ee84688ba5c6bec1521b380d38b1))
* **stdlib:** Removed memory leak in Hash module ([#1045](https://www.github.com/grain-lang/grain/issues/1045)) ([01a81c6](https://www.github.com/grain-lang/grain/commit/01a81c6a2573cca94b2d57d0fc70693d39f810a1))
* **stdlib:** Removed memory leaks in Buffer module ([#1047](https://www.github.com/grain-lang/grain/issues/1047)) ([d33017b](https://www.github.com/grain-lang/grain/commit/d33017b37e988d3facbca2e30e3de4fb8c7b5b8a))
* **stdlib:** Support arrays of any type in Array rotate ([#1048](https://www.github.com/grain-lang/grain/issues/1048)) ([3ceb1cf](https://www.github.com/grain-lang/grain/commit/3ceb1cf04c1604f49077e8733dcccb6cdaaf9f3a))
* **stdlib:** Support empty arrays in Array rotate ([3ceb1cf](https://www.github.com/grain-lang/grain/commit/3ceb1cf04c1604f49077e8733dcccb6cdaaf9f3a))

### [0.4.3](https://www.github.com/grain-lang/grain/compare/stdlib-v0.4.2...stdlib-v0.4.3) (2021-10-27)


### Features

* **stdlib:** Add cycle function to Array module ([#993](https://www.github.com/grain-lang/grain/issues/993)) ([c595622](https://www.github.com/grain-lang/grain/commit/c595622b28366655dfd5447270e8f9fc3f988a67))


### Bug Fixes

* **compiler:** Ensure TExpApp is always expansive in Grain ([ef0a69f](https://www.github.com/grain-lang/grain/commit/ef0a69fb4418d318b3227e5db7e743a026762274))
* **compiler:** Handle let-mut value restriction, such that mutable lets are always expansive ([ef0a69f](https://www.github.com/grain-lang/grain/commit/ef0a69fb4418d318b3227e5db7e743a026762274))
* **compiler:** Properly handle value restriction on function application & mutable vars ([#988](https://www.github.com/grain-lang/grain/issues/988)) ([ef0a69f](https://www.github.com/grain-lang/grain/commit/ef0a69fb4418d318b3227e5db7e743a026762274))
* **runtime:** Add types to boxed GC functions to avoid weak type errors ([ef0a69f](https://www.github.com/grain-lang/grain/commit/ef0a69fb4418d318b3227e5db7e743a026762274))
* **stdlib:** Fixed memory leak in String.explode ([#1001](https://www.github.com/grain-lang/grain/issues/1001)) ([c479a05](https://www.github.com/grain-lang/grain/commit/c479a05f48abcc4c9e98d5cf0ba698230d41031b))

### [0.4.2](https://www.github.com/grain-lang/grain/compare/stdlib-v0.4.1...stdlib-v0.4.2) (2021-10-11)


### Features

* **compiler:** Inline not `(!)` operator ([#937](https://www.github.com/grain-lang/grain/issues/937)) ([3f5e9a9](https://www.github.com/grain-lang/grain/commit/3f5e9a962ec8565f4fb79a1ee36d7b492da5cf11))
* **stdlib:** Add Conv.wasmI32ToNumber function ([#978](https://www.github.com/grain-lang/grain/issues/978)) ([c93ade8](https://www.github.com/grain-lang/grain/commit/c93ade873b51acca556e19a7483a5f29b4b6caa9))
* **stdlib:** Add string trim functions to String module ([#951](https://www.github.com/grain-lang/grain/issues/951)) ([e55de8f](https://www.github.com/grain-lang/grain/commit/e55de8fc0335b45252da9741a80b46a702cdb5b6))
* **stdlib:** Regular Expressions ([#680](https://www.github.com/grain-lang/grain/issues/680)) ([9601e16](https://www.github.com/grain-lang/grain/commit/9601e1655f1ffc76b700efef317366457c5614ef))


### Bug Fixes

* **stdlib:** Fix Char.code memory issue ([#928](https://www.github.com/grain-lang/grain/issues/928)) ([dfa31d8](https://www.github.com/grain-lang/grain/commit/dfa31d8aa60a41d08494f3817bdb628a30d83f41))
* **stdlib:** Fix issue with list printing ([#894](https://www.github.com/grain-lang/grain/issues/894)) ([e2a33a9](https://www.github.com/grain-lang/grain/commit/e2a33a9607bd076abbf6375389f1a7896775e6bc))
* **stdlib:** Fix memory issue in list printing ([#912](https://www.github.com/grain-lang/grain/issues/912)) ([3e0a805](https://www.github.com/grain-lang/grain/commit/3e0a8059a36d1042fa06d6aed54565ced116fd87))
* **stdlib:** Fix memory leaks in Char stdlib ([#929](https://www.github.com/grain-lang/grain/issues/929)) ([99cc94b](https://www.github.com/grain-lang/grain/commit/99cc94b3e2010c7fa6b2c244758dbbe13e43f903))

### [0.4.1](https://www.github.com/grain-lang/grain/compare/stdlib-v0.4.0...stdlib-v0.4.1) (2021-09-07)


### Bug Fixes

* **stdlib:** Annotate generic types in Buffer ([#876](https://www.github.com/grain-lang/grain/issues/876)) ([ad46b9e](https://www.github.com/grain-lang/grain/commit/ad46b9e6b0d2d9d562b56e5cbf1ae6751028d93a))
* **stdlib:** Correct fdPwrite return value ([#875](https://www.github.com/grain-lang/grain/issues/875)) ([ceaf6af](https://www.github.com/grain-lang/grain/commit/ceaf6af52889f5d7c8a16df9a5a9ae5ff6752105))
* **stdlib:** Correct type signatures on some Array functions ([#880](https://www.github.com/grain-lang/grain/issues/880)) ([04bf4d3](https://www.github.com/grain-lang/grain/commit/04bf4d3066cf80de8cb3da834124e29558c6d21a))

## [0.4.0](https://www.github.com/grain-lang/grain/compare/stdlib-v0.3.2...stdlib-v0.4.0) (2021-09-06)


### ⚠ BREAKING CHANGES

* **compiler:** Add typed well-formedness pass which forbids usage of WasmXX values outside of `@disableGC` context (#772)
* **stdlib:** Use random seed for hash module (#854)
* **stdlib:** Remove deprecated functions (#812)
* **stdlib:** Convert sys functions to return Results instead of throwing errors (#792)
* **compiler:** Callee-owned values (#803)
* **compiler:** Improve assert messages to contain location information (#737)
* **runtime:** Fix bug in equalHelp preventing simpleNum/boxedNum equality checks from happening correctly
* Adjust Number equality, thus that 5 == 5.0 (#726)
* **stdlib:** Export coerceNumberToWasmI32/I64/F32/F64 from Number runtime (#713)

### Features

* **compiler:** Add --no-bulk-memory flag to polyfill bulk memory ops ([#819](https://www.github.com/grain-lang/grain/issues/819)) ([7db4ea6](https://www.github.com/grain-lang/grain/commit/7db4ea6578990c2f175c083ef378c47599d47fd1))
* **compiler:** Add typed well-formedness pass which forbids usage of WasmXX values outside of `[@disable](https://www.github.com/disable)GC` context ([#772](https://www.github.com/grain-lang/grain/issues/772)) ([42fbad6](https://www.github.com/grain-lang/grain/commit/42fbad632f1f7137285dbf4dbe609ddfb91c956e))
* **compiler:** Callee-owned values ([#803](https://www.github.com/grain-lang/grain/issues/803)) ([c242e89](https://www.github.com/grain-lang/grain/commit/c242e89767788e590f053c3d3ddfa7208387c247))
* **compiler:** Improve assert messages to contain location information ([#737](https://www.github.com/grain-lang/grain/issues/737)) ([26f645b](https://www.github.com/grain-lang/grain/commit/26f645ba5f3be20d5db3a0933165bb6346d06b9d))
* **graindoc:** Add support for deprecated attribute ([#751](https://www.github.com/grain-lang/grain/issues/751)) ([8540c73](https://www.github.com/grain-lang/grain/commit/8540c73f26b9ed4cdd1a68ea671ad4b54d5ca0b2))
* **graindoc:** Support `[@since](https://www.github.com/since)` and `[@history](https://www.github.com/history)` attributes ([#785](https://www.github.com/grain-lang/grain/issues/785)) ([9386f46](https://www.github.com/grain-lang/grain/commit/9386f46304ad958c29bb099570f06193911f7131))
* **linker:** Add better error for failed wasi polyfill import ([3d8f70c](https://www.github.com/grain-lang/grain/commit/3d8f70cc8255075a462d892fdfcfe30d48c599f0))
* **stdlib:** Add Array.reverse() function ([#698](https://www.github.com/grain-lang/grain/issues/698)) ([538e987](https://www.github.com/grain-lang/grain/commit/538e987d9828b0851d0cc14a26c8b5815f012f5b))
* **stdlib:** Add Array.slice function ([#727](https://www.github.com/grain-lang/grain/issues/727)) ([66319ca](https://www.github.com/grain-lang/grain/commit/66319ca8f3bbeb80e18525d1c15e2b84f0abd0c1))
* **stdlib:** Add Array.unzip function ([#699](https://www.github.com/grain-lang/grain/issues/699)) ([cb9b49e](https://www.github.com/grain-lang/grain/commit/cb9b49e5d3110a6c23c72065ee306a5ba7ee27af))
* **stdlib:** Add Array.zip function ([#719](https://www.github.com/grain-lang/grain/issues/719)) ([1dc7f56](https://www.github.com/grain-lang/grain/commit/1dc7f56b8d4edafe0b720e031793d01b7d471855))
* **stdlib:** Add buffer module ([627f181](https://www.github.com/grain-lang/grain/commit/627f181e38a843d86a52f55b0d0a4fc02b14fc46))
* **stdlib:** Add Float32/Float64 constants for infinity/nan ([#720](https://www.github.com/grain-lang/grain/issues/720)) ([4ff3b9f](https://www.github.com/grain-lang/grain/commit/4ff3b9f99369e9b69a0b29299c89050180bbf8ec))
* **stdlib:** Add GrainDoc to Array module ([#763](https://www.github.com/grain-lang/grain/issues/763)) ([155c3e8](https://www.github.com/grain-lang/grain/commit/155c3e835850472529f21401a138b69bb5ff7318))
* **stdlib:** Add List.join and Array.join functions ([#722](https://www.github.com/grain-lang/grain/issues/722)) ([01a64b6](https://www.github.com/grain-lang/grain/commit/01a64b64f2c6e5233f2e1492c75c7531d19f637b))
* **stdlib:** Add Result.expect & Result.unwrap functions ([#808](https://www.github.com/grain-lang/grain/issues/808)) ([c390e61](https://www.github.com/grain-lang/grain/commit/c390e610e3c05eadc42aca57804a4ddb48cccff2))
* **stdlib:** Add String.chatAt function ([#721](https://www.github.com/grain-lang/grain/issues/721)) ([94ffbbe](https://www.github.com/grain-lang/grain/commit/94ffbbe0f84820d282784d62e295796ab865c837))
* **stdlib:** Add String.encode and String.decode functions to standard library ([#683](https://www.github.com/grain-lang/grain/issues/683)) ([5635a36](https://www.github.com/grain-lang/grain/commit/5635a3682e88292e3623157b34323d968f6946c3))
* **stdlib:** Convert sys functions to return Results instead of throwing errors ([#792](https://www.github.com/grain-lang/grain/issues/792)) ([35cd957](https://www.github.com/grain-lang/grain/commit/35cd957d9c04d84d9f12b54cd2882a6bbc67c175))
* **stdlib:** Export coerceNumberToWasmI32/I64/F32/F64 from Number runtime ([#713](https://www.github.com/grain-lang/grain/issues/713)) ([9353f0b](https://www.github.com/grain-lang/grain/commit/9353f0b103f6fd047230a55eb6bdede95fccceb2))
* **stdlib:** forEachCodePoint and forEachCodePointi ([#766](https://www.github.com/grain-lang/grain/issues/766)) ([b95cfb7](https://www.github.com/grain-lang/grain/commit/b95cfb77fd2f248f611f2b6a55d58d67ee800859))
* **stdlib:** Implement initial Number library ([#687](https://www.github.com/grain-lang/grain/issues/687)) ([4a71209](https://www.github.com/grain-lang/grain/commit/4a7120964a31602f763b31064e112cfeaa4d1d38))
* **stdlib:** Int32/64 clz, ctz, popcnt, rotl, rotr, eq, ne, eqz ([#807](https://www.github.com/grain-lang/grain/issues/807)) ([ffddc51](https://www.github.com/grain-lang/grain/commit/ffddc512d2c2879b76486f9d7a91621cebb064bd))
* **stdlib:** Number utilities isNaN, isFinite, and isInfinite ([#729](https://www.github.com/grain-lang/grain/issues/729)) ([b907da7](https://www.github.com/grain-lang/grain/commit/b907da7a9e2e7fdbf5d9f376533fbec21458017e))
* **stdlib:** Optimized coerceNumberToWasmI32. ([#782](https://www.github.com/grain-lang/grain/issues/782)) ([98e86ae](https://www.github.com/grain-lang/grain/commit/98e86ae98fff4b9b3d07f2815477762d2d797e30))
* **stdlib:** Provide Bytes hash implementation ([#853](https://www.github.com/grain-lang/grain/issues/853)) ([ec7d902](https://www.github.com/grain-lang/grain/commit/ec7d902e900e57a0e3e6fb2a9c16e8c578a1ba47))
* **stdlib:** String.encodeAtHelp bounds checks and optimization ([#764](https://www.github.com/grain-lang/grain/issues/764)) ([27fccae](https://www.github.com/grain-lang/grain/commit/27fccae0ffbfd4cd60cd64f60927ca4a1a7b9fae))


### Bug Fixes

* Add [@disable](https://www.github.com/disable)GC annotation to WasmXX-using getSize function in Bytes ([#771](https://www.github.com/grain-lang/grain/issues/771)) ([bc4146b](https://www.github.com/grain-lang/grain/commit/bc4146b426860f519d4fb7bb4e8345e227dffc69))
* Adjust Number equality, thus that 5 == 5.0 ([#726](https://www.github.com/grain-lang/grain/issues/726)) ([04aef16](https://www.github.com/grain-lang/grain/commit/04aef163b862588e5d7f699fc33e0810625ab22a))
* **compiler:** Refactor function return value incRef logic ([#765](https://www.github.com/grain-lang/grain/issues/765)) ([97fbe3d](https://www.github.com/grain-lang/grain/commit/97fbe3dd6615b07db249eae5946a6dfeebcec3c0))
* **compiler:** Strip leading line asterisks in block and doc comments ([#740](https://www.github.com/grain-lang/grain/issues/740)) ([45d8564](https://www.github.com/grain-lang/grain/commit/45d85644e2b686462ef58dcee608a19058c4a2d1))
* Properly initialize malloc free list ([#700](https://www.github.com/grain-lang/grain/issues/700)) ([0576fd9](https://www.github.com/grain-lang/grain/commit/0576fd9be8dbfecc5d7081731a1c5bedee32f9f1))
* Reduce reduntant memory loads in Malloc.free ([#747](https://www.github.com/grain-lang/grain/issues/747)) ([a5817b1](https://www.github.com/grain-lang/grain/commit/a5817b10fc0c7170ab75d03f0e2cc2abae8f77c6))
* **runtime:** Fix bug in equalHelp preventing simpleNum/boxedNum equality checks from happening correctly ([04aef16](https://www.github.com/grain-lang/grain/commit/04aef163b862588e5d7f699fc33e0810625ab22a))
* **stdlib:** Avoid `Pervasives.(!=)` references on WasmI32 values ([#759](https://www.github.com/grain-lang/grain/issues/759)) ([0f42544](https://www.github.com/grain-lang/grain/commit/0f42544597c628454bede14fd82542331211dab8))
* **stdlib:** Correctly indent nested record braces when printing ([#724](https://www.github.com/grain-lang/grain/issues/724)) ([05f795d](https://www.github.com/grain-lang/grain/commit/05f795dcbf77642070eae1613492d701d7b78113))
* **stdlib:** decodeRangeHelp equality check in String module ([#735](https://www.github.com/grain-lang/grain/issues/735)) ([42a03ca](https://www.github.com/grain-lang/grain/commit/42a03ca7a13187f69107e920ee25d026fd467f61))
* **stdlib:** Fixed memory leak in the print function. ([#770](https://www.github.com/grain-lang/grain/issues/770)) ([df094f1](https://www.github.com/grain-lang/grain/commit/df094f1ab412a03302aaf9fcb6ac14bd24cd85b2))
* **stdlib:** Fixed String.writeUtf8CodePoint for two byte sequences. ([#786](https://www.github.com/grain-lang/grain/issues/786)) ([20b1a77](https://www.github.com/grain-lang/grain/commit/20b1a770fe4dd5302a92c763c4748fd501b41c96))
* **stdlib:** Premature free in toString ([#863](https://www.github.com/grain-lang/grain/issues/863)) ([eec0f09](https://www.github.com/grain-lang/grain/commit/eec0f092987cd6c9ddd7519b139596611dd84d7c))
* **stdlib:** Remove fd_sync calls that are not used ([#846](https://www.github.com/grain-lang/grain/issues/846)) ([3d8f70c](https://www.github.com/grain-lang/grain/commit/3d8f70cc8255075a462d892fdfcfe30d48c599f0))
* **stdlib:** Respect callee-owned convention in sys/xx modules ([35cd957](https://www.github.com/grain-lang/grain/commit/35cd957d9c04d84d9f12b54cd2882a6bbc67c175))
* **stdlib:** Return proper values from getClockTime for Sys/Time ([#705](https://www.github.com/grain-lang/grain/issues/705)) ([cab0ce7](https://www.github.com/grain-lang/grain/commit/cab0ce7ef9cd5eca7ce8e9302915f1b3e7283143))
* **stdlib:** String.writeUtf8CodePoint should write 3 bytes for code point 0xFFFF. ([#781](https://www.github.com/grain-lang/grain/issues/781)) ([e8f399c](https://www.github.com/grain-lang/grain/commit/e8f399c2388aa0a21531127a4ea7ea82c1e2ee7b))
* **stdlib:** Use random seed for hash module ([#854](https://www.github.com/grain-lang/grain/issues/854)) ([a1a42d8](https://www.github.com/grain-lang/grain/commit/a1a42d89893fe97e8557c34a8d8e1884735299bc))
* **tests:** Fix mistaken `==` in WasmI32 tests ([0f42544](https://www.github.com/grain-lang/grain/commit/0f42544597c628454bede14fd82542331211dab8))


### Miscellaneous Chores

* **stdlib:** Remove deprecated functions ([#812](https://www.github.com/grain-lang/grain/issues/812)) ([7b74208](https://www.github.com/grain-lang/grain/commit/7b7420860b588862e1947c6fc9860b5bbaf1ff75))

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
