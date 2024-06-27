# Changelog

## [0.6.4](https://github.com/grain-lang/grain/compare/stdlib-v0.6.3...stdlib-v0.6.4) (2024-06-27)


### Features

* **stdlib:** Faster memory allocator ([#2124](https://github.com/grain-lang/grain/issues/2124)) ([03e10c4](https://github.com/grain-lang/grain/commit/03e10c49d204a488f8bd56c7b7262e717ee61762))

## [0.6.3](https://github.com/grain-lang/grain/compare/stdlib-v0.6.2...stdlib-v0.6.3) (2024-04-06)


### Miscellaneous Chores

* **stdlib:** Synchronize Grain versions

## [0.6.2](https://github.com/grain-lang/grain/compare/stdlib-v0.6.1...stdlib-v0.6.2) (2024-04-01)


### Miscellaneous Chores

* **stdlib:** Synchronize Grain versions

## [0.6.1](https://github.com/grain-lang/grain/compare/stdlib-v0.6.0...stdlib-v0.6.1) (2024-03-29)


### Bug Fixes

* **stdlib:** Avoid WASI random_get in Hash stdlib during module startup ([#2078](https://github.com/grain-lang/grain/issues/2078)) ([7eadfb0](https://github.com/grain-lang/grain/commit/7eadfb097e64b043c860d27d21d36d4bfea1ea96))
* **stdlib:** Implement `print` using a single element io vec ([#2066](https://github.com/grain-lang/grain/issues/2066)) ([9eeb0f2](https://github.com/grain-lang/grain/commit/9eeb0f2edb14facc619c1ede27a5700a27e64e3f))

## [0.6.0](https://github.com/grain-lang/grain/compare/stdlib-v0.5.13...stdlib-v0.6.0) (2024-03-04)


### ⚠ BREAKING CHANGES

* **stdlib:** Replace `parseInt` error strings with structured error enum ([#1755](https://github.com/grain-lang/grain/issues/1755))
* **stdlib:** Move stdlib `sys` modules to `wasi` ([#2056](https://github.com/grain-lang/grain/issues/2056))
* **compiler:** Remove arbitrary per-file compiler flags, add acceptable options as module attributes ([#1804](https://github.com/grain-lang/grain/issues/1804))
* **compiler:** Require extension when including relative file paths ([#1842](https://github.com/grain-lang/grain/issues/1842))
* **compiler:** Update include syntax ([#2043](https://github.com/grain-lang/grain/issues/2043))
* **stdlib:** Remove `sin`, `cos`, `tan`, `gamma`, `factorial` from `Number` module ([#2046](https://github.com/grain-lang/grain/issues/2046))
* **compiler:** Update use syntax ([#2041](https://github.com/grain-lang/grain/issues/2041))
* **grainfmt:** Implement new formatter ([#1976](https://github.com/grain-lang/grain/issues/1976))
* **stdlib:** Use default arguments in more of stdlib ([#1772](https://github.com/grain-lang/grain/issues/1772))
* **stdlib:** Add `print` suffix default argument ([#1768](https://github.com/grain-lang/grain/issues/1768))
* **graindoc:** Improve docgen for labeled & default arguments ([#1776](https://github.com/grain-lang/grain/issues/1776))
* **stdlib:** Replace `Float64` arithmatic/comparison functions with operators ([#1957](https://github.com/grain-lang/grain/issues/1957))
* **stdlib:** Replace `Float32` arithmatic/comparison functions with operators ([#1954](https://github.com/grain-lang/grain/issues/1954))
* **stdlib:** Replace `Int64` arithmatic functions with operators ([#1935](https://github.com/grain-lang/grain/issues/1935))
* **stdlib:** Replace `Int32` arithmatic functions with operators ([#1936](https://github.com/grain-lang/grain/issues/1936))
* **stdlib:** Switch to using records for `getInternalStats` ([#1898](https://github.com/grain-lang/grain/issues/1898))
* **cli:** Allow specifying WASI environment variables and CLI args ([#1840](https://github.com/grain-lang/grain/issues/1840))
* **stdlib:** Reorder parameters to `List.insert` ([#1857](https://github.com/grain-lang/grain/issues/1857))
* **stdlib:** Handle printing of reference cycles ([#1844](https://github.com/grain-lang/grain/issues/1844))
* **compiler:** Change `->` to `=>` in type signatures ([#1855](https://github.com/grain-lang/grain/issues/1855))
* **compiler:** Make types nonrecursive by default ([#1826](https://github.com/grain-lang/grain/issues/1826))
* **compiler:** Remove static runtime pointers ([#1806](https://github.com/grain-lang/grain/issues/1806))
* **compiler:** Changed comma to `and` in mutually rec types
* **compiler:** Changed comma to `and` in recursive types and bindings ([#1827](https://github.com/grain-lang/grain/issues/1827))
* Remove js-runner ([#1585](https://github.com/grain-lang/grain/issues/1585))
* **stdlib:** Replace `Result`/`Option` `and`/`or` functions with operators ([#1821](https://github.com/grain-lang/grain/issues/1821))
* **compiler:** Reduce size of runtime heap ([#1807](https://github.com/grain-lang/grain/issues/1807))
* Require node version >=18.15 for WASI support ([#1612](https://github.com/grain-lang/grain/issues/1612))
* **stdlib:** Ensure `Array.fillRange` works with negative indexing & throws IndexOutOfBound ([#1761](https://github.com/grain-lang/grain/issues/1761))
* **stdlib:** Remove extra space when converting Bytes to String ([#1800](https://github.com/grain-lang/grain/issues/1800))
* **stdlib:** Convert unsafe `Wasm` functions to operators ([#1734](https://github.com/grain-lang/grain/issues/1734))
* **stdlib:** Use Array length as default end in `Array.slice` function ([#1762](https://github.com/grain-lang/grain/issues/1762))
* **stdlib:** Use String length as default end in `String.slice` function ([#1748](https://github.com/grain-lang/grain/issues/1748))
* Rework preopened directories ([#1656](https://github.com/grain-lang/grain/issues/1656))
* **stdlib:** Group mutable/immutable data structures ([#1652](https://github.com/grain-lang/grain/issues/1652))
* **compiler:** Optimize type metadata loading ([#1741](https://github.com/grain-lang/grain/issues/1741))
* **compiler:** Labeled and default arguments ([#1623](https://github.com/grain-lang/grain/issues/1623))
* **stdlib:** Replace bigint/number arithmetic functions with operators ([#1742](https://github.com/grain-lang/grain/issues/1742))
* **stdlib:** Update Operator `uint` operator Names ([#1738](https://github.com/grain-lang/grain/issues/1738))
* **stdlib:** Provide empty byte array from Bytes.make ([#1729](https://github.com/grain-lang/grain/issues/1729))
* **stdlib:** Add optimization for `Number.pow(Rational, Int)` ([#1716](https://github.com/grain-lang/grain/issues/1716))
* **compiler:** Custom box printing ([#1713](https://github.com/grain-lang/grain/issues/1713))
* **stdlib:** Update Buffer/Bytes to use new integer types ([#1704](https://github.com/grain-lang/grain/issues/1704))
* **compiler:** Require floats always have a digit on left & right of dot ([#1703](https://github.com/grain-lang/grain/issues/1703))
* **compiler:** Remove 32-bit numbers from `Number` type ([#1683](https://github.com/grain-lang/grain/issues/1683))
* **compiler:** Short integer values ([#1669](https://github.com/grain-lang/grain/issues/1669))
* **compiler:** Supply primitive types via the compiler ([#1667](https://github.com/grain-lang/grain/issues/1667))
* **stdlib:** Add `**` (pow) operator to Pervasives module ([#1690](https://github.com/grain-lang/grain/issues/1690))
* **compiler:** Remove Exclusive/Inclusive Ranges, provide as record via compiler ([#1616](https://github.com/grain-lang/grain/issues/1616))
* **stdlib:** Make queue and stack mutable & provide Immutable submodules ([#1479](https://github.com/grain-lang/grain/issues/1479))
* **compiler:** Disallow multiple `provide`s of the same value ([#1689](https://github.com/grain-lang/grain/issues/1689))
* **graindoc:** Only use original doc blocks when re-providing ([#1687](https://github.com/grain-lang/grain/issues/1687))
* **graindoc:** Support docblocks on submodules ([#1684](https://github.com/grain-lang/grain/issues/1684))
* **graindoc:** Remove section attribute ([#1681](https://github.com/grain-lang/grain/issues/1681))
* **compiler:** Explicit abstract types ([#1680](https://github.com/grain-lang/grain/issues/1680))
* **compiler:** Add Uint32 and Uint64 types ([#1531](https://github.com/grain-lang/grain/issues/1531))
* **stdlib:** Remove `cons` and `empty` from Pervasives ([#1657](https://github.com/grain-lang/grain/issues/1657))
* **stdlib:** Sys/File reading and writing operate on Bytes ([#1655](https://github.com/grain-lang/grain/issues/1655))
* **compiler:** Make List a language-supplied type ([#1648](https://github.com/grain-lang/grain/issues/1648))
* **stdlib:** Use correct casing for BigInt module name
* **graindoc:** Replace module attribute with docblock on module header ([#1647](https://github.com/grain-lang/grain/issues/1647))
* **compiler:** Module system ([#1584](https://github.com/grain-lang/grain/issues/1584))
* **compiler:** Change Char internal representation ([#1622](https://github.com/grain-lang/grain/issues/1622))
* **compiler:** Inline record constructors ([#1586](https://github.com/grain-lang/grain/issues/1586))
* **stdlib:** Replace Number.nan & Number.infinity constants with keywords ([#1618](https://github.com/grain-lang/grain/issues/1618))
* **compiler:** Remove support of single-argument tuples ([#1608](https://github.com/grain-lang/grain/issues/1608))
* **stdlib:** `List.rotate` wraparound for count > length ([#1558](https://github.com/grain-lang/grain/issues/1558))
* **compiler:** Include Option and Result as language-supplied types ([#1591](https://github.com/grain-lang/grain/issues/1591))
* **stdlib:** Change array rotation direction ([#1552](https://github.com/grain-lang/grain/issues/1552))
* **stdlib:** Support zipping arrays of different sizes ([#1402](https://github.com/grain-lang/grain/issues/1402))
* **compiler:** Refactor enum constructors ([#1211](https://github.com/grain-lang/grain/issues/1211))

### Features

* Add Hex Exponent Decimals ([8a69eb3](https://github.com/grain-lang/grain/commit/8a69eb31e4e22e6148c42ecd0efb677f3fbb222c))
* **cli:** Allow specifying WASI environment variables and CLI args ([#1840](https://github.com/grain-lang/grain/issues/1840)) ([fb8fbf2](https://github.com/grain-lang/grain/commit/fb8fbf2b8ca2a024e3983d25b52443337ec9746a))
* **compiler:** Add hex float syntax ([#1743](https://github.com/grain-lang/grain/issues/1743)) ([8a69eb3](https://github.com/grain-lang/grain/commit/8a69eb31e4e22e6148c42ecd0efb677f3fbb222c))
* **compiler:** Add Uint32 and Uint64 types ([#1531](https://github.com/grain-lang/grain/issues/1531)) ([42ffdc4](https://github.com/grain-lang/grain/commit/42ffdc408096901696abb6e3fa862b65d3d7e2a2))
* **compiler:** Change `-&gt;` to `=>` in type signatures ([#1855](https://github.com/grain-lang/grain/issues/1855)) ([b3d68a4](https://github.com/grain-lang/grain/commit/b3d68a4f2e99222a69d7066164892077f83ebf8c))
* **compiler:** Change Char internal representation ([#1622](https://github.com/grain-lang/grain/issues/1622)) ([58c9a51](https://github.com/grain-lang/grain/commit/58c9a5145a6c1c53e280657e4e41f79f2662eb8a))
* **compiler:** Changed comma to `and` in mutually rec types ([923625d](https://github.com/grain-lang/grain/commit/923625dfaf276be894546e67db66a17d00df9f14))
* **compiler:** Changed comma to `and` in recursive types and bindings ([#1827](https://github.com/grain-lang/grain/issues/1827)) ([923625d](https://github.com/grain-lang/grain/commit/923625dfaf276be894546e67db66a17d00df9f14))
* **compiler:** Custom box printing ([#1713](https://github.com/grain-lang/grain/issues/1713)) ([3c86e45](https://github.com/grain-lang/grain/commit/3c86e459235ad9bf0abbefb575021736ad45f310))
* **compiler:** Explicit abstract types ([#1680](https://github.com/grain-lang/grain/issues/1680)) ([58cd224](https://github.com/grain-lang/grain/commit/58cd2247441f16cfc95b415ad272da7a37b82fb5))
* **compiler:** Include Option and Result as language-supplied types ([#1591](https://github.com/grain-lang/grain/issues/1591)) ([bd5403f](https://github.com/grain-lang/grain/commit/bd5403f94f30366adc7d3307adc3c6c4fa5e1803))
* **compiler:** Inline record constructors ([#1586](https://github.com/grain-lang/grain/issues/1586)) ([43082f5](https://github.com/grain-lang/grain/commit/43082f52692d90b1419b6b72ebe66406a0a01d99))
* **compiler:** Labeled and default arguments ([#1623](https://github.com/grain-lang/grain/issues/1623)) ([28a38ac](https://github.com/grain-lang/grain/commit/28a38ac149d396cd034b6c53b0aa3eb478ed29ed))
* **compiler:** Make List a language-supplied type ([#1648](https://github.com/grain-lang/grain/issues/1648)) ([2ee1328](https://github.com/grain-lang/grain/commit/2ee13282427718d8849a334d085a3f833037127b))
* **compiler:** Make types nonrecursive by default ([#1826](https://github.com/grain-lang/grain/issues/1826)) ([46b8a5a](https://github.com/grain-lang/grain/commit/46b8a5aaa384eb52b34b82faea5b0a06adc22568))
* **compiler:** Module system ([#1584](https://github.com/grain-lang/grain/issues/1584)) ([752da69](https://github.com/grain-lang/grain/commit/752da69057b2b06a1415710d6da93fbb948e8185))
* **compiler:** Rational number type ([#1603](https://github.com/grain-lang/grain/issues/1603)) ([350f850](https://github.com/grain-lang/grain/commit/350f8503c3e6976275aaa8ee4d8c02554e7c238a))
* **compiler:** Reduce size of runtime heap ([#1807](https://github.com/grain-lang/grain/issues/1807)) ([246f894](https://github.com/grain-lang/grain/commit/246f894951be6ee4f2cb13df3d851583efc1498d))
* **compiler:** Refactor enum constructors ([#1211](https://github.com/grain-lang/grain/issues/1211)) ([8d465b7](https://github.com/grain-lang/grain/commit/8d465b7cd73d9549eeb89c7d52083d592ebd66fd))
* **compiler:** Remove 32-bit numbers from `Number` type ([#1683](https://github.com/grain-lang/grain/issues/1683)) ([50bf8ee](https://github.com/grain-lang/grain/commit/50bf8ee39d017805091fe1ae7536621e956c03ce))
* **compiler:** Remove arbitrary per-file compiler flags, add acceptable options as module attributes ([#1804](https://github.com/grain-lang/grain/issues/1804)) ([72b2139](https://github.com/grain-lang/grain/commit/72b21393b0c82669ff9005730cafb0b345a8a992))
* **compiler:** Remove Exclusive/Inclusive Ranges, provide as record via compiler ([#1616](https://github.com/grain-lang/grain/issues/1616)) ([49a399d](https://github.com/grain-lang/grain/commit/49a399dbcfc24957a3875ae094ad0a2c4b636a32))
* **compiler:** Remove Pervasives' dependency on toString when types are elided ([#1664](https://github.com/grain-lang/grain/issues/1664)) ([f703476](https://github.com/grain-lang/grain/commit/f703476db8d376d68a1c50fece48f335f455a791))
* **compiler:** Remove support of single-argument tuples ([#1608](https://github.com/grain-lang/grain/issues/1608)) ([509cd85](https://github.com/grain-lang/grain/commit/509cd85801eb165c0ce65347047a0380f2304b25))
* **compiler:** Require extension when including relative file paths ([#1842](https://github.com/grain-lang/grain/issues/1842)) ([dde62d3](https://github.com/grain-lang/grain/commit/dde62d39c501bd25cccac6569b426406711a5735))
* **compiler:** Short integer values ([#1669](https://github.com/grain-lang/grain/issues/1669)) ([fc4670d](https://github.com/grain-lang/grain/commit/fc4670de13c46dfac3def1ca59d718ffb36aca1c))
* **compiler:** Streamlined reference counting ([#1714](https://github.com/grain-lang/grain/issues/1714)) ([0711868](https://github.com/grain-lang/grain/commit/071186801ccf7f6dc3eef653382f6d82b8b23b6c))
* **compiler:** Supply primitive types via the compiler ([#1667](https://github.com/grain-lang/grain/issues/1667)) ([b41feb7](https://github.com/grain-lang/grain/commit/b41feb73976d4ef3d9c17c31f0dbcfc22df32d9d))
* **compiler:** Update include syntax ([#2043](https://github.com/grain-lang/grain/issues/2043)) ([5f44d4e](https://github.com/grain-lang/grain/commit/5f44d4e5a1cd432faddcfb1a138ac6a7797423fd))
* **compiler:** Update use syntax ([#2041](https://github.com/grain-lang/grain/issues/2041)) ([cd346ab](https://github.com/grain-lang/grain/commit/cd346ab761aaa5e8e7692ebce1bae4e5e7e47f45))
* **graindoc:** Allow doc comments on variants and record fields ([#1852](https://github.com/grain-lang/grain/issues/1852)) ([53f770c](https://github.com/grain-lang/grain/commit/53f770c24ef9057b7b5e63d3dda75b166c28536d))
* **graindoc:** Improve docgen for labeled & default arguments ([#1776](https://github.com/grain-lang/grain/issues/1776)) ([be7ff9d](https://github.com/grain-lang/grain/commit/be7ff9d9ec2237ad5028f8d9e3e2f10a6bd3f9dd))
* **graindoc:** Only use original doc blocks when re-providing ([#1687](https://github.com/grain-lang/grain/issues/1687)) ([97c7ce4](https://github.com/grain-lang/grain/commit/97c7ce4dfb7e4474358a62c3699dd3edc121c961))
* **graindoc:** Replace module attribute with docblock on module header ([#1647](https://github.com/grain-lang/grain/issues/1647)) ([2ff9d9e](https://github.com/grain-lang/grain/commit/2ff9d9ec49c727778c6d0ee74219f746df8a11e2))
* **graindoc:** Support docblocks on submodules ([#1684](https://github.com/grain-lang/grain/issues/1684)) ([bc13017](https://github.com/grain-lang/grain/commit/bc1301751c90380e53c0ca9048a928468245d13c))
* **grainfmt:** Implement new formatter ([#1976](https://github.com/grain-lang/grain/issues/1976)) ([1568aa0](https://github.com/grain-lang/grain/commit/1568aa06f625c4d91d9151e3f969ec648e8d4231))
* Remove js-runner ([#1585](https://github.com/grain-lang/grain/issues/1585)) ([e10d612](https://github.com/grain-lang/grain/commit/e10d61295c42237b7b472cd3c5d07f2c5f28d79b))
* Rework preopened directories ([#1656](https://github.com/grain-lang/grain/issues/1656)) ([7d3006d](https://github.com/grain-lang/grain/commit/7d3006d86d423a0bb03a600c6bf9726efc8394b9))
* **runtime:** Allow modulo on floating point numbers ([#1914](https://github.com/grain-lang/grain/issues/1914)) ([f90d8af](https://github.com/grain-lang/grain/commit/f90d8af2ee2373b5420655c9e80826f56fffaa91))
* **stdlib:** `List.rotate` wraparound for count &gt; length ([#1558](https://github.com/grain-lang/grain/issues/1558)) ([6dd9680](https://github.com/grain-lang/grain/commit/6dd968009b5d700f2e31ed6f4b1074dbdc4299e7))
* **stdlib:** Add `**` (pow) operator to Pervasives module ([#1690](https://github.com/grain-lang/grain/issues/1690)) ([b16b455](https://github.com/grain-lang/grain/commit/b16b4552952fd554657ac1b5bcc868dca0600476))
* **stdlib:** Add `**` operator to `Int32` module ([#1938](https://github.com/grain-lang/grain/issues/1938)) ([32b9639](https://github.com/grain-lang/grain/commit/32b9639be58408d587f9c9c29a4b56857e6f328f))
* **stdlib:** Add `**` operator to `Int64` module ([#1937](https://github.com/grain-lang/grain/issues/1937)) ([48de28b](https://github.com/grain-lang/grain/commit/48de28bba8be2e157cfb82e90e709759659ab3b4))
* **stdlib:** Add `==` operator to `Queue` module ([#1931](https://github.com/grain-lang/grain/issues/1931)) ([3c0ea18](https://github.com/grain-lang/grain/commit/3c0ea189298d9bc4f14824a76814e9181e57a879))
* **stdlib:** Add `addCharFromCodePoint` to Buffer module ([#1883](https://github.com/grain-lang/grain/issues/1883)) ([18b553a](https://github.com/grain-lang/grain/commit/18b553a6f69876941bc7153c4e44d4db5547d86b))
* **stdlib:** Add `asin`, `acos`, `atan`, `isClose` to Number module ([#1699](https://github.com/grain-lang/grain/issues/1699)) ([353b544](https://github.com/grain-lang/grain/commit/353b544948861d39cbeef97273f661eed2e9748d))
* **stdlib:** Add `atan2` to the `Number` module ([#2016](https://github.com/grain-lang/grain/issues/2016)) ([145b783](https://github.com/grain-lang/grain/commit/145b783f234d9f6d6bdbe730877a5a53d125cb93))
* **stdlib:** Add `chunk` function to Array module ([#1708](https://github.com/grain-lang/grain/issues/1708)) ([fba7c1d](https://github.com/grain-lang/grain/commit/fba7c1d18447917d7544e780353684452321b38e))
* **stdlib:** Add `fromArray` to `Queue` module ([#1932](https://github.com/grain-lang/grain/issues/1932)) ([1c35a94](https://github.com/grain-lang/grain/commit/1c35a9438008d40c8d3a49810131c69d9b8b97a0))
* **stdlib:** Add `isEmpty` to `List` module ([#1860](https://github.com/grain-lang/grain/issues/1860)) ([7362189](https://github.com/grain-lang/grain/commit/736218950e7433512554f1b5fea4bc5cbfe97160))
* **stdlib:** add `isEmpty` to `String` module ([#1861](https://github.com/grain-lang/grain/issues/1861)) ([e8cb932](https://github.com/grain-lang/grain/commit/e8cb93298cf8776876efc7972f09f0eef2d57c58))
* **stdlib:** Add `Json` module ([#1133](https://github.com/grain-lang/grain/issues/1133)) ([5a6e4c6](https://github.com/grain-lang/grain/commit/5a6e4c6111ff84b3249436d485d4c45ef0a89d61))
* **stdlib:** Add `linearInterpolate`, `linearMap` and `clamp` ([#1707](https://github.com/grain-lang/grain/issues/1707)) ([15842a1](https://github.com/grain-lang/grain/commit/15842a1030e9c0937d7cd697508b083ae06315a7))
* **stdlib:** Add `print` suffix default argument ([#1768](https://github.com/grain-lang/grain/issues/1768)) ([6701170](https://github.com/grain-lang/grain/commit/6701170a1ecd91a8b7cb566d2ff99402d24c18e1))
* **stdlib:** Add `toArray` to `Queue` ([#1930](https://github.com/grain-lang/grain/issues/1930)) ([7c865a4](https://github.com/grain-lang/grain/commit/7c865a46811ee078325f55ebcaa054c9f1920ff6))
* **stdlib:** Add `toIntegerRatio` and `fromIntegerRatio`  to Rational Library ([#1746](https://github.com/grain-lang/grain/issues/1746)) ([008a735](https://github.com/grain-lang/grain/commit/008a735051c02fbcd7bd47d7735937a61afd0cd6))
* **stdlib:** Add `toList` and `fromList` functions to `Queue` module ([#1866](https://github.com/grain-lang/grain/issues/1866)) ([7cdcf95](https://github.com/grain-lang/grain/commit/7cdcf953d451396e56122bcf062019171102d8d6))
* **stdlib:** Add `Uri` module ([#1970](https://github.com/grain-lang/grain/issues/1970)) ([5cf726e](https://github.com/grain-lang/grain/commit/5cf726edb06ef42a9c8c073fbd3407defe501a49))
* **stdlib:** Add ASCII utilities to String, Char ([#1975](https://github.com/grain-lang/grain/issues/1975)) ([f65002d](https://github.com/grain-lang/grain/commit/f65002d8c16c927577a4593351882c0734dd9897))
* **stdlib:** Add optimization for `Number.pow(Rational, Int)` ([#1716](https://github.com/grain-lang/grain/issues/1716)) ([67bee5c](https://github.com/grain-lang/grain/commit/67bee5c0982d35f0d6a1350e002b7062a498afe7))
* **stdlib:** Change array rotation direction ([#1552](https://github.com/grain-lang/grain/issues/1552)) ([bbe56ad](https://github.com/grain-lang/grain/commit/bbe56ade517bd685d52384d32aecb94d25d379f7))
* **stdlib:** Convert unsafe `Wasm` functions to operators ([#1734](https://github.com/grain-lang/grain/issues/1734)) ([114d17b](https://github.com/grain-lang/grain/commit/114d17be4463772bbc84ebc408e9cf2b482c6103))
* **stdlib:** Handle printing of reference cycles ([#1844](https://github.com/grain-lang/grain/issues/1844)) ([49c854e](https://github.com/grain-lang/grain/commit/49c854eb428ebbe57db5a63d5e7f87faaded3159))
* **stdlib:** Make queue and stack mutable & provide Immutable submodules ([#1479](https://github.com/grain-lang/grain/issues/1479)) ([979a20c](https://github.com/grain-lang/grain/commit/979a20ce6b734b90bc38c11ee6a4534ea8e555b8))
* **stdlib:** Move stdlib `sys` modules to `wasi` ([#2056](https://github.com/grain-lang/grain/issues/2056)) ([ca75e38](https://github.com/grain-lang/grain/commit/ca75e38a2bbadd1a5e51f7ab344994d62c96ca4f))
* **stdlib:** Provide empty byte array from Bytes.make ([#1729](https://github.com/grain-lang/grain/issues/1729)) ([fde3064](https://github.com/grain-lang/grain/commit/fde3064428fa2244e73932d5992ac722a26ebfe4))
* **stdlib:** Remove parseInt dependency on Pervasives ([#1649](https://github.com/grain-lang/grain/issues/1649)) ([9408568](https://github.com/grain-lang/grain/commit/9408568a3457c2084afc22f958334e87a4183ccf))
* **stdlib:** Reorder parameters to `List.insert` ([#1857](https://github.com/grain-lang/grain/issues/1857)) ([973f3f3](https://github.com/grain-lang/grain/commit/973f3f34c39a4ac7a552e378b84020d5c647ce86))
* **stdlib:** Replace `Float32` arithmatic/comparison functions with operators ([#1954](https://github.com/grain-lang/grain/issues/1954)) ([52cc15a](https://github.com/grain-lang/grain/commit/52cc15a5251cd0003fcfbe243c7b7c940a61d429))
* **stdlib:** Replace `Float64` arithmatic/comparison functions with operators ([#1957](https://github.com/grain-lang/grain/issues/1957)) ([dea4cb5](https://github.com/grain-lang/grain/commit/dea4cb54d10b9cecbf54ed30048c0937bc38cea2))
* **stdlib:** Replace `Int32` arithmatic functions with operators ([#1936](https://github.com/grain-lang/grain/issues/1936)) ([8a69dd3](https://github.com/grain-lang/grain/commit/8a69dd393e002585464029a42a4362b6edfee662))
* **stdlib:** Replace `Int64` arithmatic functions with operators ([#1935](https://github.com/grain-lang/grain/issues/1935)) ([cfb909c](https://github.com/grain-lang/grain/commit/cfb909c60d55d94eb869c389da28a8b3a1bc502f))
* **stdlib:** Replace `Result`/`Option` `and`/`or` functions with operators ([#1821](https://github.com/grain-lang/grain/issues/1821)) ([686de7e](https://github.com/grain-lang/grain/commit/686de7ec3c83d092c7a0407657695e1bc671abd7))
* **stdlib:** Simplify `equal` using `Memory.compare` ([#1972](https://github.com/grain-lang/grain/issues/1972)) ([dc21976](https://github.com/grain-lang/grain/commit/dc219763ebc7bcfcd6c411d7b2b1f152e5e87d6e))
* **stdlib:** Support zipping arrays of different sizes ([#1402](https://github.com/grain-lang/grain/issues/1402)) ([b8ae3d6](https://github.com/grain-lang/grain/commit/b8ae3d69c98ada45bc592fb3097f21bc8a0d40bd))
* **stdlib:** Use Array length as default end in `Array.slice` function ([#1762](https://github.com/grain-lang/grain/issues/1762)) ([a698fdc](https://github.com/grain-lang/grain/commit/a698fdc55ef45e8b0d156d5bb3cacc9e21d758ac))
* **stdlib:** Use String length as default end in `String.slice` function ([#1748](https://github.com/grain-lang/grain/issues/1748)) ([b7c41a6](https://github.com/grain-lang/grain/commit/b7c41a6421dd0392bfec9f1791d9314556af9318))


### Bug Fixes

* **compiler:** Disallow multiple `provide`s of the same value ([#1689](https://github.com/grain-lang/grain/issues/1689)) ([7ab7ddc](https://github.com/grain-lang/grain/commit/7ab7ddc1c7e9eae221edcf787b733ef7acf97904))
* **compiler:** Remove static runtime pointers ([#1806](https://github.com/grain-lang/grain/issues/1806)) ([8d76dc9](https://github.com/grain-lang/grain/commit/8d76dc917cf109e3b8496faca565fd9904fa0b22))
* Fix String.ReplaceAll ([#1705](https://github.com/grain-lang/grain/issues/1705)) ([d3af004](https://github.com/grain-lang/grain/commit/d3af0049f6910b10065ddd72c46e5364fe53c4ff))
* **stdlib:** Ensure `Array.fillRange` works with negative indexing & throws IndexOutOfBound ([#1761](https://github.com/grain-lang/grain/issues/1761)) ([3c3c4d9](https://github.com/grain-lang/grain/commit/3c3c4d9ced8168287ecbd23ee71948c5592e3b66))
* **stdlib:** Ensure consistent marshal representation ([#2045](https://github.com/grain-lang/grain/issues/2045)) ([029059f](https://github.com/grain-lang/grain/commit/029059fbeed185a2e467a014bbc313fda4b87c5e))
* **stdlib:** Error when `relativeTo` used on relative source and absolute dest ([#2054](https://github.com/grain-lang/grain/issues/2054)) ([1626a1f](https://github.com/grain-lang/grain/commit/1626a1fd8b621c6bcc359dab48b4495a0db21e14))
* **stdlib:** Fix overzealous Memory.fill in Buffer.truncate ([#1781](https://github.com/grain-lang/grain/issues/1781)) ([e1f24e9](https://github.com/grain-lang/grain/commit/e1f24e941a392f1ac6464683d71f87eab6787220))
* **stdlib:** Properly handle extremely large integer bases in `Number.(**)` ([#1950](https://github.com/grain-lang/grain/issues/1950)) ([84c076f](https://github.com/grain-lang/grain/commit/84c076fb3b7b34a3fec6dfd2c527ca4833414ff4))
* **stdlib:** Properly resize empty Queue on push ([#1865](https://github.com/grain-lang/grain/issues/1865)) ([f7727ef](https://github.com/grain-lang/grain/commit/f7727ef2158f63dc8c88a5b824b41e31495f0f51))
* **stdlib:** Properly resize empty Stack on push ([#1867](https://github.com/grain-lang/grain/issues/1867)) ([b90d924](https://github.com/grain-lang/grain/commit/b90d924a38f8a7f82a7c972f8213ab9ec8561820))
* **stdlib:** Remove extra space when converting Bytes to String ([#1800](https://github.com/grain-lang/grain/issues/1800)) ([543807b](https://github.com/grain-lang/grain/commit/543807b1156aecc93396c12121ada15f4d2bd046))
* **stdlib:** Return early from `Process.argv()` if length is zero ([#1817](https://github.com/grain-lang/grain/issues/1817)) ([8ccea28](https://github.com/grain-lang/grain/commit/8ccea288feb7f981adaecd29ba1324fefb192b69))
* **stdlib:** Sys/File reading and writing operate on Bytes ([#1655](https://github.com/grain-lang/grain/issues/1655)) ([17cb28d](https://github.com/grain-lang/grain/commit/17cb28d25f4b1f2cf8943429136c863f66447675))
* **stdlib:** Use correct casing for BigInt module name ([2ff9d9e](https://github.com/grain-lang/grain/commit/2ff9d9ec49c727778c6d0ee74219f746df8a11e2))


### Miscellaneous Chores

* **compiler:** Optimize type metadata loading ([#1741](https://github.com/grain-lang/grain/issues/1741)) ([a4519bc](https://github.com/grain-lang/grain/commit/a4519bc88d9fe12159495e995fcaa74834d5a361))
* **compiler:** Require floats always have a digit on left & right of dot ([#1703](https://github.com/grain-lang/grain/issues/1703)) ([b657e1c](https://github.com/grain-lang/grain/commit/b657e1cf68d4b20bfb06d662b1fc8687836e5104))
* **graindoc:** Remove section attribute ([#1681](https://github.com/grain-lang/grain/issues/1681)) ([e969ad7](https://github.com/grain-lang/grain/commit/e969ad75e017785d12decbbf87651bd8c7dd286d))
* Require node version &gt;=18.15 for WASI support ([#1612](https://github.com/grain-lang/grain/issues/1612)) ([331ffc2](https://github.com/grain-lang/grain/commit/331ffc28b57e7e52fc9e360d1d85f81d3a172d06))
* **stdlib:** Group mutable/immutable data structures ([#1652](https://github.com/grain-lang/grain/issues/1652)) ([f93afef](https://github.com/grain-lang/grain/commit/f93afef3785910ebbedcf780ce2d6109299c2e86))
* **stdlib:** Remove `cons` and `empty` from Pervasives ([#1657](https://github.com/grain-lang/grain/issues/1657)) ([f6f02bc](https://github.com/grain-lang/grain/commit/f6f02bcb5a196f7f611ee6604f8585d429368314))
* **stdlib:** Remove `sin`, `cos`, `tan`, `gamma`, `factorial` from `Number` module ([#2046](https://github.com/grain-lang/grain/issues/2046)) ([85c4389](https://github.com/grain-lang/grain/commit/85c4389dee9f0760e40639df5112294cb4cc44cb))
* **stdlib:** Replace `parseInt` error strings with structured error enum ([#1755](https://github.com/grain-lang/grain/issues/1755)) ([ea26d18](https://github.com/grain-lang/grain/commit/ea26d1814558730507517d4939abed6767aaaf24))
* **stdlib:** Replace bigint/number arithmetic functions with operators ([#1742](https://github.com/grain-lang/grain/issues/1742)) ([cbd46ee](https://github.com/grain-lang/grain/commit/cbd46ee99654d066690f1eb13bde8d882abed80f))
* **stdlib:** Replace Number.nan & Number.infinity constants with keywords ([#1618](https://github.com/grain-lang/grain/issues/1618)) ([b437c13](https://github.com/grain-lang/grain/commit/b437c131aedcc4395080179b8e0d41ff6b2c95b2))
* **stdlib:** Switch to using records for `getInternalStats` ([#1898](https://github.com/grain-lang/grain/issues/1898)) ([bdb119a](https://github.com/grain-lang/grain/commit/bdb119a8234409c5425a2f8678d53237eb7af850))
* **stdlib:** Update Buffer/Bytes to use new integer types ([#1704](https://github.com/grain-lang/grain/issues/1704)) ([d508e5a](https://github.com/grain-lang/grain/commit/d508e5af7d66e8419370000030ee40194052301f))
* **stdlib:** Update Operator `uint` operator Names ([#1738](https://github.com/grain-lang/grain/issues/1738)) ([decb053](https://github.com/grain-lang/grain/commit/decb0533e87cb8d617e27c0f5470a24c502dcfab))
* **stdlib:** Use default arguments in more of stdlib ([#1772](https://github.com/grain-lang/grain/issues/1772)) ([f5e934f](https://github.com/grain-lang/grain/commit/f5e934fc658427b698cfb3919c9b2ac411a0ce45))

### [0.5.13](https://github.com/grain-lang/grain/compare/stdlib-v0.5.12...stdlib-v0.5.13) (2023-01-07)


### Miscellaneous Chores

* **stdlib:** Synchronize Grain versions

### [0.5.12](https://github.com/grain-lang/grain/compare/stdlib-v0.5.11...stdlib-v0.5.12) (2023-01-05)


### Bug Fixes

* **stdlib:** Remove memory corruption in File.fdReaddir ([#1573](https://github.com/grain-lang/grain/issues/1573)) ([060fc7b](https://github.com/grain-lang/grain/commit/060fc7ba4e5c4d86098eafbee09bbce13bca32c3))

### [0.5.11](https://github.com/grain-lang/grain/compare/stdlib-v0.5.10...stdlib-v0.5.11) (2022-12-29)


### Miscellaneous Chores

* **stdlib:** Synchronize Grain versions

### [0.5.10](https://github.com/grain-lang/grain/compare/stdlib-v0.5.9...stdlib-v0.5.10) (2022-12-23)


### Bug Fixes

* **compiler:** Provide appropriate error during invalid array access ([#1556](https://github.com/grain-lang/grain/issues/1556)) ([3d7b9e3](https://github.com/grain-lang/grain/commit/3d7b9e3d99b2639b46080e1c564d1453cd80f666))

### [0.5.9](https://github.com/grain-lang/grain/compare/stdlib-v0.5.8...stdlib-v0.5.9) (2022-12-14)


### Miscellaneous Chores

* **stdlib:** Synchronize Grain versions

### [0.5.8](https://github.com/grain-lang/grain/compare/stdlib-v0.5.7...stdlib-v0.5.8) (2022-12-10)


### Features

* **runtime:** Optimize simple number comparison ([f7ceae7](https://github.com/grain-lang/grain/commit/f7ceae729260bc3c5eee017954aa7e242c3f8cb5))
* **stdlib:** Add `ImmutableArray` module ([#1477](https://github.com/grain-lang/grain/issues/1477)) ([a34d8b9](https://github.com/grain-lang/grain/commit/a34d8b9e8ce629c40d65b99561c0f41fa400542e))


### Bug Fixes

* **stdlib:** Fix NaN comparisons ([#1543](https://github.com/grain-lang/grain/issues/1543)) ([f7ceae7](https://github.com/grain-lang/grain/commit/f7ceae729260bc3c5eee017954aa7e242c3f8cb5))

### [0.5.7](https://github.com/grain-lang/grain/compare/stdlib-v0.5.6...stdlib-v0.5.7) (2022-12-06)


### Miscellaneous Chores

* **stdlib:** Synchronize Grain versions

### [0.5.6](https://github.com/grain-lang/grain/compare/stdlib-v0.5.5...stdlib-v0.5.6) (2022-12-05)


### Miscellaneous Chores

* **stdlib:** Synchronize Grain versions

### [0.5.5](https://github.com/grain-lang/grain/compare/stdlib-v0.5.4...stdlib-v0.5.5) (2022-12-05)


### Features

* **graindoc:** Add attribute for exceptions that may be thrown ([#1492](https://github.com/grain-lang/grain/issues/1492)) ([b2e75c7](https://github.com/grain-lang/grain/commit/b2e75c7452ef2544c768729c7a45e21ff31616d0))
* **graindoc:** Support deprecations on module docblocks ([#1498](https://github.com/grain-lang/grain/issues/1498)) ([b3dc85c](https://github.com/grain-lang/grain/commit/b3dc85c0fc311479de4e57774a075c3a922216ba))
* **stdlib:** Add `parse` function to Number module ([#1517](https://github.com/grain-lang/grain/issues/1517)) ([59e89d1](https://github.com/grain-lang/grain/commit/59e89d12b7fcf2626c8adb45c742a787171b7024))
* **stdlib:** Add `parseFloat` function to Number module ([#1288](https://github.com/grain-lang/grain/issues/1288)) ([e21f2b1](https://github.com/grain-lang/grain/commit/e21f2b137f7dcd67cccf9debf695db852dc2afc5))
* **stdlib:** Add `split` function to Regex module ([#1469](https://github.com/grain-lang/grain/issues/1469)) ([0c1eb73](https://github.com/grain-lang/grain/commit/0c1eb73d01e30f457138c6e3b603a9faddcf8e9b))
* **stdlib:** Add `splitAll` function to Regex module ([0c1eb73](https://github.com/grain-lang/grain/commit/0c1eb73d01e30f457138c6e3b603a9faddcf8e9b))
* **stdlib:** Add Path module for working with system paths ([#1452](https://github.com/grain-lang/grain/issues/1452)) ([900e976](https://github.com/grain-lang/grain/commit/900e976654565b3618e2215e9b7cefbda873d9a8))
* **stdlib:** Improve performance in Array & List modules ([#1487](https://github.com/grain-lang/grain/issues/1487)) ([2168f6a](https://github.com/grain-lang/grain/commit/2168f6ade151548bc655debeb8a1bc09ce87cb77))
* **stdlib:** Improve performance of `flatMap`, `some`, and `every` functions in Array module ([2168f6a](https://github.com/grain-lang/grain/commit/2168f6ade151548bc655debeb8a1bc09ce87cb77))
* **stdlib:** Improve performance of `some` and `every` functions in List module ([2168f6a](https://github.com/grain-lang/grain/commit/2168f6ade151548bc655debeb8a1bc09ce87cb77))


### Bug Fixes

* **grainfmt:** Add parentheses around some binops for precedence clarity ([#1514](https://github.com/grain-lang/grain/issues/1514)) ([3ac27cc](https://github.com/grain-lang/grain/commit/3ac27cc6e17b896dae0ef2cb5f5de510c7c2dd60))
* **grainfmt:** Handle multiple line items and comments better ([#1460](https://github.com/grain-lang/grain/issues/1460)) ([5395fd4](https://github.com/grain-lang/grain/commit/5395fd45b79fb3bcf3dd1ec52a1d5973a23a4bdc))
* **runtime:** Properly divide bigints in the number type ([59e89d1](https://github.com/grain-lang/grain/commit/59e89d12b7fcf2626c8adb45c742a787171b7024))

### [0.5.4](https://github.com/grain-lang/grain/compare/stdlib-v0.5.3...stdlib-v0.5.4) (2022-11-12)


### Features

* **stdlib:** Add `empty` constant to ImmutablePriorityQueue module ([427335f](https://github.com/grain-lang/grain/commit/427335fa5c211445f727a650ca06adacfe9c5310))
* **stdlib:** Add `empty` constant to Queue module ([427335f](https://github.com/grain-lang/grain/commit/427335fa5c211445f727a650ca06adacfe9c5310))
* **stdlib:** Add `empty` constant to Stack module ([427335f](https://github.com/grain-lang/grain/commit/427335fa5c211445f727a650ca06adacfe9c5310))
* **stdlib:** Add `exp` function to Number module ([5af9a99](https://github.com/grain-lang/grain/commit/5af9a99b2ec3b4a2d6745cb22b70defe2b366cfa))
* **stdlib:** Add `factorial` function to Number module ([5af9a99](https://github.com/grain-lang/grain/commit/5af9a99b2ec3b4a2d6745cb22b70defe2b366cfa))
* **stdlib:** Add `gamma` function to Number module ([5af9a99](https://github.com/grain-lang/grain/commit/5af9a99b2ec3b4a2d6745cb22b70defe2b366cfa))
* **stdlib:** Add `infinity` constant to the Number module ([c24f6c1](https://github.com/grain-lang/grain/commit/c24f6c1cfae87632a003c0337c29ec98a80cfda2))
* **stdlib:** Add `nan` constant to the Number module ([c24f6c1](https://github.com/grain-lang/grain/commit/c24f6c1cfae87632a003c0337c29ec98a80cfda2))
* **stdlib:** Add `pow` function to Number module ([5af9a99](https://github.com/grain-lang/grain/commit/5af9a99b2ec3b4a2d6745cb22b70defe2b366cfa))
* **stdlib:** Add `replaceAll` function to String module ([5606cd2](https://github.com/grain-lang/grain/commit/5606cd246583884175b135cbeb29024400651b34))
* **stdlib:** Add `replaceFirst` function to String module ([5606cd2](https://github.com/grain-lang/grain/commit/5606cd246583884175b135cbeb29024400651b34))
* **stdlib:** Add `replaceLast` function to String module ([5606cd2](https://github.com/grain-lang/grain/commit/5606cd246583884175b135cbeb29024400651b34))
* **stdlib:** Add `tan` function to Number module ([5af9a99](https://github.com/grain-lang/grain/commit/5af9a99b2ec3b4a2d6745cb22b70defe2b366cfa))
* **stdlib:** Add `toDegrees` function to Number module ([5af9a99](https://github.com/grain-lang/grain/commit/5af9a99b2ec3b4a2d6745cb22b70defe2b366cfa))
* **stdlib:** Add `toRadians` function to Number module ([5af9a99](https://github.com/grain-lang/grain/commit/5af9a99b2ec3b4a2d6745cb22b70defe2b366cfa))
* **stdlib:** Add additional functions to Number module ([#1443](https://github.com/grain-lang/grain/issues/1443)) ([5af9a99](https://github.com/grain-lang/grain/commit/5af9a99b2ec3b4a2d6745cb22b70defe2b366cfa))
* **stdlib:** Add replacement functions to String module ([#1441](https://github.com/grain-lang/grain/issues/1441)) ([5606cd2](https://github.com/grain-lang/grain/commit/5606cd246583884175b135cbeb29024400651b34))
* **stdlib:** Added `empty` constant to immutable data structures ([#1466](https://github.com/grain-lang/grain/issues/1466)) ([427335f](https://github.com/grain-lang/grain/commit/427335fa5c211445f727a650ca06adacfe9c5310))
* **stdlib:** Implement `fromArray` in PriorityQueue & ImmutablePriorityQueue modules ([#1451](https://github.com/grain-lang/grain/issues/1451)) ([d321f84](https://github.com/grain-lang/grain/commit/d321f84174fee2a340745a9f55994fbfa23f6c7a))
* **stdlib:** Implement ImmutableMap and ImmutableSet ([#1414](https://github.com/grain-lang/grain/issues/1414)) ([b31120d](https://github.com/grain-lang/grain/commit/b31120d41be668c48b9bca9f2b944616371a8ab4))
* **stdlib:** Improved efficiency of constructing a PriorityQueue from a List ([d321f84](https://github.com/grain-lang/grain/commit/d321f84174fee2a340745a9f55994fbfa23f6c7a))
* **stdlib:** Optimize string trimming ([#1442](https://github.com/grain-lang/grain/issues/1442)) ([0212247](https://github.com/grain-lang/grain/commit/0212247a7fbf0d54085959de2853f3fe66cd8b12))


### Bug Fixes

* **compiler:** Panic immediately when out of memory ([#1450](https://github.com/grain-lang/grain/issues/1450)) ([943d47d](https://github.com/grain-lang/grain/commit/943d47dddde2d88fd96727e9d7ed8501efec42ef))
* **grainfmt:** Handle chained value bindings properly ([#1467](https://github.com/grain-lang/grain/issues/1467)) ([07bfcd3](https://github.com/grain-lang/grain/commit/07bfcd3f15c34ef99b05531591b1473f206b7395))
* **grainfmt:** Indent lines when wrapping infix operators ([#1465](https://github.com/grain-lang/grain/issues/1465)) ([d705849](https://github.com/grain-lang/grain/commit/d705849ea8d9073e608576b77adeae834c454e0b))
* **runtime:** Handle bigint mul/div within Number correctly ([#1475](https://github.com/grain-lang/grain/issues/1475)) ([0fe8aa6](https://github.com/grain-lang/grain/commit/0fe8aa6a96a9c5ebf2f2bf2e1f28578badfb337f))
* **stdlib:** Fix anchoring behavior in Regex.replaceAll ([#1440](https://github.com/grain-lang/grain/issues/1440)) ([d513eff](https://github.com/grain-lang/grain/commit/d513effe569d0aa0d44c974596fd285f1ad8d57d))
* **stdlib:** Fix handling of `NaN` and `Infinity` in Number module ([#1457](https://github.com/grain-lang/grain/issues/1457)) ([c24f6c1](https://github.com/grain-lang/grain/commit/c24f6c1cfae87632a003c0337c29ec98a80cfda2))

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
