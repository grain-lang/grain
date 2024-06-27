# Changelog

## [0.6.4](https://github.com/grain-lang/grain/compare/grain-v0.6.3...grain-v0.6.4) (2024-06-27)


### Features

* **graindoc:** Cache module comments ([#2102](https://github.com/grain-lang/grain/issues/2102)) ([d45ddfa](https://github.com/grain-lang/grain/commit/d45ddfaac5067d7d6c08703653a31187d35914a5))
* **stdlib:** Faster memory allocator ([#2124](https://github.com/grain-lang/grain/issues/2124)) ([03e10c4](https://github.com/grain-lang/grain/commit/03e10c49d204a488f8bd56c7b7262e717ee61762))


### Bug Fixes

* **compiler:** Stabilize CRC checks ([#2101](https://github.com/grain-lang/grain/issues/2101)) ([fc19761](https://github.com/grain-lang/grain/commit/fc19761e9f7e6c3d1cb54a308de721b17d25e12e))

## [0.6.3](https://github.com/grain-lang/grain/compare/grain-v0.6.2...grain-v0.6.3) (2024-04-06)


### Features

* **compiler:** Enable debug info in binary output ([#2086](https://github.com/grain-lang/grain/issues/2086)) ([8555853](https://github.com/grain-lang/grain/commit/8555853c9f55afbe24d629c827521374e169c18c))


### Bug Fixes

* **compiler:** Properly encode hex values in Bytes literals ([#2088](https://github.com/grain-lang/grain/issues/2088)) ([263993d](https://github.com/grain-lang/grain/commit/263993dedf4e13bb74e562133c52db7ebf8ba1c0))

## [0.6.2](https://github.com/grain-lang/grain/compare/grain-v0.6.1...grain-v0.6.2) (2024-04-01)


### Bug Fixes

* **grainlsp:** Prevent LSP crash on startup when running in JS ([#2084](https://github.com/grain-lang/grain/issues/2084)) ([e99cf1f](https://github.com/grain-lang/grain/commit/e99cf1f7b160d54930e14479c55cc365c86678c8))

## [0.6.1](https://github.com/grain-lang/grain/compare/grain-v0.6.0...grain-v0.6.1) (2024-03-29)


### Features

* **compiler:** Add warning for non-integer array indices ([#2077](https://github.com/grain-lang/grain/issues/2077)) ([520dd8a](https://github.com/grain-lang/grain/commit/520dd8a22068970139420d22185d65d196fc951d))
* **compiler:** Allow non-block bodies with loops ([#2064](https://github.com/grain-lang/grain/issues/2064)) ([6fd1603](https://github.com/grain-lang/grain/commit/6fd16037cd6ab34cb6dce18e6945c7fad0a54611))
* **compiler:** Improve warning when omitting fields in record pattern ([#2079](https://github.com/grain-lang/grain/issues/2079)) ([4a929fc](https://github.com/grain-lang/grain/commit/4a929fc05cb1f7fd10007f30a94ecf1c228c6dee))


### Bug Fixes

* **compiler:** Allow proper disambiguation of enum variants ([#2068](https://github.com/grain-lang/grain/issues/2068)) ([f3007b4](https://github.com/grain-lang/grain/commit/f3007b479c85d408dd93d90e968300459c823cf3))
* **compiler:** Fix array infix assign semantics ([#2080](https://github.com/grain-lang/grain/issues/2080)) ([bea360d](https://github.com/grain-lang/grain/commit/bea360d2527d8ae8904a2dc0b5a8a96b26338314))
* **compiler:** Fix disambiguation of record label access ([#2072](https://github.com/grain-lang/grain/issues/2072)) ([1f8a20d](https://github.com/grain-lang/grain/commit/1f8a20df9b5d34d91a0c241e7975798dc7a3bba8))
* **compiler:** Set maximum wasm table size ([#2061](https://github.com/grain-lang/grain/issues/2061)) ([e8d643c](https://github.com/grain-lang/grain/commit/e8d643c39ea905afc6a4d7ceba215bf7876f15cf))
* **grainfmt:** Ensure constraints and keyword functions group properly ([#2070](https://github.com/grain-lang/grain/issues/2070)) ([12281ad](https://github.com/grain-lang/grain/commit/12281ade6e209fcceb8dbe3afc59a1c996e2d216))
* **grainfmt:** Fix formatting of `default` argument types ([#2081](https://github.com/grain-lang/grain/issues/2081)) ([0c15e4a](https://github.com/grain-lang/grain/commit/0c15e4ab9b924489a28a9cd105c38d9388ba6524))
* **grainfmt:** Fix printing of qualified infix operators ([#2071](https://github.com/grain-lang/grain/issues/2071)) ([c22932d](https://github.com/grain-lang/grain/commit/c22932d57e41c5ac9cc622aa58ab7c0ef37c6441))
* **stdlib:** Avoid WASI random_get in Hash stdlib during module startup ([#2078](https://github.com/grain-lang/grain/issues/2078)) ([7eadfb0](https://github.com/grain-lang/grain/commit/7eadfb097e64b043c860d27d21d36d4bfea1ea96))
* **stdlib:** Implement `print` using a single element io vec ([#2066](https://github.com/grain-lang/grain/issues/2066)) ([9eeb0f2](https://github.com/grain-lang/grain/commit/9eeb0f2edb14facc619c1ede27a5700a27e64e3f))

## [0.6.0](https://github.com/grain-lang/grain/compare/grain-v0.5.13...grain-v0.6.0) (2024-03-04)


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
* **graindoc:** Allow `@since` and `@returns` once per export ([#1946](https://github.com/grain-lang/grain/issues/1946))
* **graindoc:** Improve docgen for labeled & default arguments ([#1776](https://github.com/grain-lang/grain/issues/1776))
* **stdlib:** Replace `Float64` arithmatic/comparison functions with operators ([#1957](https://github.com/grain-lang/grain/issues/1957))
* **stdlib:** Replace `Float32` arithmatic/comparison functions with operators ([#1954](https://github.com/grain-lang/grain/issues/1954))
* **stdlib:** Replace `Int64` arithmatic functions with operators ([#1935](https://github.com/grain-lang/grain/issues/1935))
* **stdlib:** Replace `Int32` arithmatic functions with operators ([#1936](https://github.com/grain-lang/grain/issues/1936))
* **stdlib:** Switch to using records for `getInternalStats` ([#1898](https://github.com/grain-lang/grain/issues/1898))
* **cli:** Ensure `--use-start-section` can only be used with compile command ([#1871](https://github.com/grain-lang/grain/issues/1871))
* **cli:** Allow specifying WASI environment variables and CLI args ([#1840](https://github.com/grain-lang/grain/issues/1840))
* **compiler:** Require `module` prefix on `use`/`provide` for modules ([#1868](https://github.com/grain-lang/grain/issues/1868))
* **stdlib:** Reorder parameters to `List.insert` ([#1857](https://github.com/grain-lang/grain/issues/1857))
* **stdlib:** Handle printing of reference cycles ([#1844](https://github.com/grain-lang/grain/issues/1844))
* **compiler:** Change `->` to `=>` in type signatures ([#1855](https://github.com/grain-lang/grain/issues/1855))
* **compiler:** Make types nonrecursive by default ([#1826](https://github.com/grain-lang/grain/issues/1826))
* **compiler:** Remove static runtime pointers ([#1806](https://github.com/grain-lang/grain/issues/1806))
* **compiler:** Remove memory import from _genv ([#1830](https://github.com/grain-lang/grain/issues/1830))
* **compiler:** Changed comma to `and` in mutually rec types
* **compiler:** Changed comma to `and` in recursive types and bindings ([#1827](https://github.com/grain-lang/grain/issues/1827))
* Remove js-runner ([#1585](https://github.com/grain-lang/grain/issues/1585))
* **stdlib:** Replace `Result`/`Option` `and`/`or` functions with operators ([#1821](https://github.com/grain-lang/grain/issues/1821))
* **compiler:** Reduce size of runtime heap ([#1807](https://github.com/grain-lang/grain/issues/1807))
* Require node version >=18.15 for WASI support ([#1612](https://github.com/grain-lang/grain/issues/1612))
* **stdlib:** Ensure `Array.fillRange` works with negative indexing & throws IndexOutOfBound ([#1761](https://github.com/grain-lang/grain/issues/1761))
* **compiler:** Disallow enum variants with parens but no types ([#1809](https://github.com/grain-lang/grain/issues/1809))
* **stdlib:** Remove extra space when converting Bytes to String ([#1800](https://github.com/grain-lang/grain/issues/1800))
* **stdlib:** Convert unsafe `Wasm` functions to operators ([#1734](https://github.com/grain-lang/grain/issues/1734))
* **stdlib:** Use Array length as default end in `Array.slice` function ([#1762](https://github.com/grain-lang/grain/issues/1762))
* **compiler:** Remove recursive_types and principal config ([#1751](https://github.com/grain-lang/grain/issues/1751))
* **stdlib:** Use String length as default end in `String.slice` function ([#1748](https://github.com/grain-lang/grain/issues/1748))
* Rework preopened directories ([#1656](https://github.com/grain-lang/grain/issues/1656))
* **stdlib:** Group mutable/immutable data structures ([#1652](https://github.com/grain-lang/grain/issues/1652))
* **compiler:** Optimize type metadata loading ([#1741](https://github.com/grain-lang/grain/issues/1741))
* **compiler:** Labeled and default arguments ([#1623](https://github.com/grain-lang/grain/issues/1623))
* **compiler:** Remove built-in FileDescriptor type ([#1747](https://github.com/grain-lang/grain/issues/1747))
* **compiler:** Enable tail calls by default ([#1589](https://github.com/grain-lang/grain/issues/1589))
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
* **compiler:** Allocate closures only when necessary ([#1660](https://github.com/grain-lang/grain/issues/1660))
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
* **compiler:** Apply correct rules for parsing Unicode whitespace ([#1554](https://github.com/grain-lang/grain/issues/1554))
* **compiler:** Module system ([#1584](https://github.com/grain-lang/grain/issues/1584))
* **compiler:** Change Char internal representation ([#1622](https://github.com/grain-lang/grain/issues/1622))
* **compiler:** Inline record constructors ([#1586](https://github.com/grain-lang/grain/issues/1586))
* **stdlib:** Replace Number.nan & Number.infinity constants with keywords ([#1618](https://github.com/grain-lang/grain/issues/1618))
* **compiler:** Remove support of single-argument tuples ([#1608](https://github.com/grain-lang/grain/issues/1608))
* **compiler:** Rename `_grainEnv` to `_genv` ([#1607](https://github.com/grain-lang/grain/issues/1607))
* **compiler:** Add `yield` and `macro` reserved keywords ([#1471](https://github.com/grain-lang/grain/issues/1471))
* **compiler:** Implement `Infinity` and `NaN` syntax ([#1472](https://github.com/grain-lang/grain/issues/1472))
* **stdlib:** `List.rotate` wraparound for count > length ([#1558](https://github.com/grain-lang/grain/issues/1558))
* **compiler:** Early return ([#1464](https://github.com/grain-lang/grain/issues/1464))
* **compiler:** Include Option and Result as language-supplied types ([#1591](https://github.com/grain-lang/grain/issues/1591))
* **stdlib:** Change array rotation direction ([#1552](https://github.com/grain-lang/grain/issues/1552))
* **stdlib:** Support zipping arrays of different sizes ([#1402](https://github.com/grain-lang/grain/issues/1402))
* Remove --parser-debug-level ([#1311](https://github.com/grain-lang/grain/issues/1311)) (#1447)
* **compiler:** Refactor enum constructors ([#1211](https://github.com/grain-lang/grain/issues/1211))

### Features

* Add `exception` opentype implementation to `Oprint` ([#1896](https://github.com/grain-lang/grain/issues/1896)) ([51d53f7](https://github.com/grain-lang/grain/commit/51d53f7e1f3f3bdb6508a8ad925887f9854cd84f))
* Add Hex Exponent Decimals ([8a69eb3](https://github.com/grain-lang/grain/commit/8a69eb31e4e22e6148c42ecd0efb677f3fbb222c))
* **ci:** Add builds for preview binaries ([#1777](https://github.com/grain-lang/grain/issues/1777)) ([d512c5a](https://github.com/grain-lang/grain/commit/d512c5ae7c314d4dd34394a81c8746c0641aff38))
* **cli:** Allow specifying WASI environment variables and CLI args ([#1840](https://github.com/grain-lang/grain/issues/1840)) ([fb8fbf2](https://github.com/grain-lang/grain/commit/fb8fbf2b8ca2a024e3983d25b52443337ec9746a))
* **cli:** Improve error handling around `grain run` ([#1913](https://github.com/grain-lang/grain/issues/1913)) ([fc9b434](https://github.com/grain-lang/grain/commit/fc9b434995a5521b6346534afe58cd38fdd7c59e))
* **compiler:** Add `magic` primitive ([#1766](https://github.com/grain-lang/grain/issues/1766)) ([a28ebb4](https://github.com/grain-lang/grain/commit/a28ebb4e7404b3badb01dad40dc74c2919a4f79b))
* **compiler:** Add `yield` and `macro` reserved keywords ([#1471](https://github.com/grain-lang/grain/issues/1471)) ([f0e29c5](https://github.com/grain-lang/grain/commit/f0e29c5e6d2fc3a4deb884816014fa4d90ee8f1b))
* **compiler:** Add ability to match on low level wasm types ([#1588](https://github.com/grain-lang/grain/issues/1588)) ([0d30888](https://github.com/grain-lang/grain/commit/0d3088846bcf865d8c3aa4298db9148b071664fc))
* **compiler:** Add hex float syntax ([#1743](https://github.com/grain-lang/grain/issues/1743)) ([8a69eb3](https://github.com/grain-lang/grain/commit/8a69eb31e4e22e6148c42ecd0efb677f3fbb222c))
* **compiler:** Add stack allocation information to small ints ([#1780](https://github.com/grain-lang/grain/issues/1780)) ([6f1a1a9](https://github.com/grain-lang/grain/commit/6f1a1a9adf2b7a9e732f7503a0955938c6e65543))
* **compiler:** Add Uint32 and Uint64 types ([#1531](https://github.com/grain-lang/grain/issues/1531)) ([42ffdc4](https://github.com/grain-lang/grain/commit/42ffdc408096901696abb6e3fa862b65d3d7e2a2))
* **compiler:** Allocate closures only when necessary ([#1660](https://github.com/grain-lang/grain/issues/1660)) ([c40df12](https://github.com/grain-lang/grain/commit/c40df12b95f6f3a74bf89d637d60c64714b1d31e))
* **compiler:** Allow a newline character after 'as' when aliasing ([#1641](https://github.com/grain-lang/grain/issues/1641)) ([2719034](https://github.com/grain-lang/grain/commit/27190343160fe99957a9ca7169c48c0d673c81ef))
* **compiler:** Allow arrays to do binary operation assignment ([#1928](https://github.com/grain-lang/grain/issues/1928)) ([8a335ae](https://github.com/grain-lang/grain/commit/8a335ae1e3d9874131f1c6402e3bc4de6a9cfdb3))
* **compiler:** Allow importing a memory ([#1661](https://github.com/grain-lang/grain/issues/1661)) ([3140ba2](https://github.com/grain-lang/grain/commit/3140ba249f47de8e66af236ddc681f4e9f481e1b))
* **compiler:** Bytes literals ([#1662](https://github.com/grain-lang/grain/issues/1662)) ([3d8e4c5](https://github.com/grain-lang/grain/commit/3d8e4c5ee074e393eebeed82d2c4bd0590a0ad31))
* **compiler:** Change `-&gt;` to `=>` in type signatures ([#1855](https://github.com/grain-lang/grain/issues/1855)) ([b3d68a4](https://github.com/grain-lang/grain/commit/b3d68a4f2e99222a69d7066164892077f83ebf8c))
* **compiler:** Change Char internal representation ([#1622](https://github.com/grain-lang/grain/issues/1622)) ([58c9a51](https://github.com/grain-lang/grain/commit/58c9a5145a6c1c53e280657e4e41f79f2662eb8a))
* **compiler:** Changed comma to `and` in mutually rec types ([923625d](https://github.com/grain-lang/grain/commit/923625dfaf276be894546e67db66a17d00df9f14))
* **compiler:** Changed comma to `and` in recursive types and bindings ([#1827](https://github.com/grain-lang/grain/issues/1827)) ([923625d](https://github.com/grain-lang/grain/commit/923625dfaf276be894546e67db66a17d00df9f14))
* **compiler:** Custom box printing ([#1713](https://github.com/grain-lang/grain/issues/1713)) ([3c86e45](https://github.com/grain-lang/grain/commit/3c86e459235ad9bf0abbefb575021736ad45f310))
* **compiler:** Early return ([#1464](https://github.com/grain-lang/grain/issues/1464)) ([1bffc82](https://github.com/grain-lang/grain/commit/1bffc821002e10c6fcbfb0fcf65750cda5c1e29f))
* **compiler:** Enable tail calls by default ([#1589](https://github.com/grain-lang/grain/issues/1589)) ([f6e5b00](https://github.com/grain-lang/grain/commit/f6e5b002475f62e04e9f1feb452ff8e9262dacbb))
* **compiler:** Explicit abstract types ([#1680](https://github.com/grain-lang/grain/issues/1680)) ([58cd224](https://github.com/grain-lang/grain/commit/58cd2247441f16cfc95b415ad272da7a37b82fb5))
* **compiler:** Implement `Infinity` and `NaN` syntax ([#1472](https://github.com/grain-lang/grain/issues/1472)) ([74a196d](https://github.com/grain-lang/grain/commit/74a196d947dea8a31e3f80154f952903f1f8bb89))
* **compiler:** Include Option and Result as language-supplied types ([#1591](https://github.com/grain-lang/grain/issues/1591)) ([bd5403f](https://github.com/grain-lang/grain/commit/bd5403f94f30366adc7d3307adc3c6c4fa5e1803))
* **compiler:** Inline record constructors ([#1586](https://github.com/grain-lang/grain/issues/1586)) ([43082f5](https://github.com/grain-lang/grain/commit/43082f52692d90b1419b6b72ebe66406a0a01d99))
* **compiler:** Labeled and default arguments ([#1623](https://github.com/grain-lang/grain/issues/1623)) ([28a38ac](https://github.com/grain-lang/grain/commit/28a38ac149d396cd034b6c53b0aa3eb478ed29ed))
* **compiler:** Make List a language-supplied type ([#1648](https://github.com/grain-lang/grain/issues/1648)) ([2ee1328](https://github.com/grain-lang/grain/commit/2ee13282427718d8849a334d085a3f833037127b))
* **compiler:** Make types nonrecursive by default ([#1826](https://github.com/grain-lang/grain/issues/1826)) ([46b8a5a](https://github.com/grain-lang/grain/commit/46b8a5aaa384eb52b34b82faea5b0a06adc22568))
* **compiler:** Module system ([#1584](https://github.com/grain-lang/grain/issues/1584)) ([752da69](https://github.com/grain-lang/grain/commit/752da69057b2b06a1415710d6da93fbb948e8185))
* **compiler:** Propagate attribute locations to Anftree ([#1642](https://github.com/grain-lang/grain/issues/1642)) ([cce2821](https://github.com/grain-lang/grain/commit/cce2821e3b4bedbe8eef5a9814fed3eb24d7eccd))
* **compiler:** Providing, including, reproviding exceptions ([#1849](https://github.com/grain-lang/grain/issues/1849)) ([687e747](https://github.com/grain-lang/grain/commit/687e747fb5d4c242d3bfceec6541ade58f6863a8))
* **compiler:** Rational number type ([#1603](https://github.com/grain-lang/grain/issues/1603)) ([350f850](https://github.com/grain-lang/grain/commit/350f8503c3e6976275aaa8ee4d8c02554e7c238a))
* **compiler:** Reduce size of runtime heap ([#1807](https://github.com/grain-lang/grain/issues/1807)) ([246f894](https://github.com/grain-lang/grain/commit/246f894951be6ee4f2cb13df3d851583efc1498d))
* **compiler:** Refactor enum constructors ([#1211](https://github.com/grain-lang/grain/issues/1211)) ([8d465b7](https://github.com/grain-lang/grain/commit/8d465b7cd73d9549eeb89c7d52083d592ebd66fd))
* **compiler:** Remove 32-bit numbers from `Number` type ([#1683](https://github.com/grain-lang/grain/issues/1683)) ([50bf8ee](https://github.com/grain-lang/grain/commit/50bf8ee39d017805091fe1ae7536621e956c03ce))
* **compiler:** Remove arbitrary per-file compiler flags, add acceptable options as module attributes ([#1804](https://github.com/grain-lang/grain/issues/1804)) ([72b2139](https://github.com/grain-lang/grain/commit/72b21393b0c82669ff9005730cafb0b345a8a992))
* **compiler:** Remove Exclusive/Inclusive Ranges, provide as record via compiler ([#1616](https://github.com/grain-lang/grain/issues/1616)) ([49a399d](https://github.com/grain-lang/grain/commit/49a399dbcfc24957a3875ae094ad0a2c4b636a32))
* **compiler:** Remove Pervasives' dependency on toString when types are elided ([#1664](https://github.com/grain-lang/grain/issues/1664)) ([f703476](https://github.com/grain-lang/grain/commit/f703476db8d376d68a1c50fece48f335f455a791))
* **compiler:** Remove support of single-argument tuples ([#1608](https://github.com/grain-lang/grain/issues/1608)) ([509cd85](https://github.com/grain-lang/grain/commit/509cd85801eb165c0ce65347047a0380f2304b25))
* **compiler:** Require `module` prefix on `use`/`provide` for modules ([#1868](https://github.com/grain-lang/grain/issues/1868)) ([5efb54c](https://github.com/grain-lang/grain/commit/5efb54c199e674a3715779f97239b7b47744d88f))
* **compiler:** Require extension when including relative file paths ([#1842](https://github.com/grain-lang/grain/issues/1842)) ([dde62d3](https://github.com/grain-lang/grain/commit/dde62d39c501bd25cccac6569b426406711a5735))
* **compiler:** Short integer values ([#1669](https://github.com/grain-lang/grain/issues/1669)) ([fc4670d](https://github.com/grain-lang/grain/commit/fc4670de13c46dfac3def1ca59d718ffb36aca1c))
* **compiler:** Streamlined reference counting ([#1714](https://github.com/grain-lang/grain/issues/1714)) ([0711868](https://github.com/grain-lang/grain/commit/071186801ccf7f6dc3eef653382f6d82b8b23b6c))
* **compiler:** Supply primitive types via the compiler ([#1667](https://github.com/grain-lang/grain/issues/1667)) ([b41feb7](https://github.com/grain-lang/grain/commit/b41feb73976d4ef3d9c17c31f0dbcfc22df32d9d))
* **compiler:** Suppress warnings about using fromNumber for Int32/64 and Float32/64 tests ([#1638](https://github.com/grain-lang/grain/issues/1638)) ([b687e31](https://github.com/grain-lang/grain/commit/b687e31d6e5d82260cb31f355253934da407d3c5))
* **compiler:** Update include syntax ([#2043](https://github.com/grain-lang/grain/issues/2043)) ([5f44d4e](https://github.com/grain-lang/grain/commit/5f44d4e5a1cd432faddcfb1a138ac6a7797423fd))
* **compiler:** Update use syntax ([#2041](https://github.com/grain-lang/grain/issues/2041)) ([cd346ab](https://github.com/grain-lang/grain/commit/cd346ab761aaa5e8e7692ebce1bae4e5e7e47f45))
* **graindoc:** Add test harness ([#1767](https://github.com/grain-lang/grain/issues/1767)) ([8a93ebe](https://github.com/grain-lang/grain/commit/8a93ebeebafe5938448e7e2968c6a48b90d43e3c))
* **graindoc:** Allow `[@since](https://github.com/since)` and `[@returns](https://github.com/returns)` once per export ([#1946](https://github.com/grain-lang/grain/issues/1946)) ([ad977aa](https://github.com/grain-lang/grain/commit/ad977aa3aa191692606e59e4971f93058ede42c9))
* **graindoc:** Allow doc comments on variants and record fields ([#1852](https://github.com/grain-lang/grain/issues/1852)) ([53f770c](https://github.com/grain-lang/grain/commit/53f770c24ef9057b7b5e63d3dda75b166c28536d))
* **graindoc:** Improve docgen for labeled & default arguments ([#1776](https://github.com/grain-lang/grain/issues/1776)) ([be7ff9d](https://github.com/grain-lang/grain/commit/be7ff9d9ec2237ad5028f8d9e3e2f10a6bd3f9dd))
* **graindoc:** Only use original doc blocks when re-providing ([#1687](https://github.com/grain-lang/grain/issues/1687)) ([97c7ce4](https://github.com/grain-lang/grain/commit/97c7ce4dfb7e4474358a62c3699dd3edc121c961))
* **graindoc:** Refactored Graindoc parser ([#1658](https://github.com/grain-lang/grain/issues/1658)) ([b447ced](https://github.com/grain-lang/grain/commit/b447cedc648ece5c5c892ff66172d2f264831647))
* **graindoc:** Replace module attribute with docblock on module header ([#1647](https://github.com/grain-lang/grain/issues/1647)) ([2ff9d9e](https://github.com/grain-lang/grain/commit/2ff9d9ec49c727778c6d0ee74219f746df8a11e2))
* **graindoc:** Support docblocks on submodules ([#1684](https://github.com/grain-lang/grain/issues/1684)) ([bc13017](https://github.com/grain-lang/grain/commit/bc1301751c90380e53c0ca9048a928468245d13c))
* **grainfmt:** Implement new formatter ([#1976](https://github.com/grain-lang/grain/issues/1976)) ([1568aa0](https://github.com/grain-lang/grain/commit/1568aa06f625c4d91d9151e3f969ec648e8d4231))
* **grainlsp:** Implement goto definition ([#1787](https://github.com/grain-lang/grain/issues/1787)) ([3cb862a](https://github.com/grain-lang/grain/commit/3cb862a9b7c046aeab22796ebd88de63d4853525))
* **grainlsp:** Implement inlay hints for module includes ([#1793](https://github.com/grain-lang/grain/issues/1793)) ([408da76](https://github.com/grain-lang/grain/commit/408da761bd5945892ea19bb21833a15c1506cf6d))
* **grainlsp:** Provide hovers on use statements ([#1688](https://github.com/grain-lang/grain/issues/1688)) ([c5cd21f](https://github.com/grain-lang/grain/commit/c5cd21fc3a6b703bb9de2c7a4c7834bd27a743a9))
* **lsp:** Add hover for submodules ([#2028](https://github.com/grain-lang/grain/issues/2028)) ([85d8768](https://github.com/grain-lang/grain/commit/85d876864b7ae5d69609138b1a33904689347743))
* **lsp:** Support goto definition on exceptions ([#1895](https://github.com/grain-lang/grain/issues/1895)) ([218b2f0](https://github.com/grain-lang/grain/commit/218b2f0f7f5f69d81c111abf9cbc2656e60a18be))
* **lsp:** Support hover on `include` statements ([#1963](https://github.com/grain-lang/grain/issues/1963)) ([b6e1570](https://github.com/grain-lang/grain/commit/b6e15705c218b1f42b71da15e045e6472fad43cb))
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

* **cli:** Correct typo in printing stack ([#1924](https://github.com/grain-lang/grain/issues/1924)) ([a4016f1](https://github.com/grain-lang/grain/commit/a4016f187e4d8148811459c0a8638c36bdff9349))
* **cli:** Ensure `--use-start-section` can only be used with compile command ([#1871](https://github.com/grain-lang/grain/issues/1871)) ([d9227d7](https://github.com/grain-lang/grain/commit/d9227d7770b73338362d5699d2382e029fe3a5dc))
* **cli:** Show correct error when module traps ([#2050](https://github.com/grain-lang/grain/issues/2050)) ([88560f2](https://github.com/grain-lang/grain/commit/88560f266a04403716fe49e87156c466a5a36802))
* **compiler:** Allow early return in let bindings ([#1727](https://github.com/grain-lang/grain/issues/1727)) ([43adefe](https://github.com/grain-lang/grain/commit/43adefe7ff7e4ca7ba6f6f80889f66b557451888))
* **compiler:** Allow providing values of types provided later ([#1897](https://github.com/grain-lang/grain/issues/1897)) ([1c43bd6](https://github.com/grain-lang/grain/commit/1c43bd60395b9ff024d9e8ebbbe6d517c2e30051))
* **compiler:** Allow reproviding from submodules ([#1888](https://github.com/grain-lang/grain/issues/1888)) ([97c00e1](https://github.com/grain-lang/grain/commit/97c00e1e47bc783c6dad71b605f4a0d2ae3d11b9))
* **compiler:** Apply correct rules for parsing Unicode whitespace ([#1554](https://github.com/grain-lang/grain/issues/1554)) ([4f19d71](https://github.com/grain-lang/grain/commit/4f19d71b6bbe7f2094fadbb9fbd99f1fd62e80ae))
* **compiler:** Correct locations for data constructor idents ([#1752](https://github.com/grain-lang/grain/issues/1752)) ([d015a97](https://github.com/grain-lang/grain/commit/d015a978d7969994d09b7050f9f358fc5bba9d23))
* **compiler:** Correct the warning suggestion for Float32 And Float64 values ([#1671](https://github.com/grain-lang/grain/issues/1671)) ([32e0397](https://github.com/grain-lang/grain/commit/32e03972660b451ad6f068281aee08ad0d958c27))
* **compiler:** Correctly handle and backpatch mutually recursive closure scope functions ([#1808](https://github.com/grain-lang/grain/issues/1808)) ([c2a0c5b](https://github.com/grain-lang/grain/commit/c2a0c5baa7664a38ded3977455b8b9c2588c5034))
* **compiler:** Disallow enum variants with parens but no types ([#1809](https://github.com/grain-lang/grain/issues/1809)) ([25e4946](https://github.com/grain-lang/grain/commit/25e494680ac048125748e9f696df83de974b9e72))
* **compiler:** Disallow multiple `provide`s of the same value ([#1689](https://github.com/grain-lang/grain/issues/1689)) ([7ab7ddc](https://github.com/grain-lang/grain/commit/7ab7ddc1c7e9eae221edcf787b733ef7acf97904))
* **compiler:** Ensure constant number locations are propagated correctly when parsing ([#2044](https://github.com/grain-lang/grain/issues/2044)) ([53a33e6](https://github.com/grain-lang/grain/commit/53a33e6c97d5ddf17fc88480622d993b1c86e631))
* **compiler:** Ensure refcounts are maintained when tail calls use arguments multiple times ([#1993](https://github.com/grain-lang/grain/issues/1993)) ([86c6e12](https://github.com/grain-lang/grain/commit/86c6e12e0ab1f48347cfd57d3cb328fe1f6fac15))
* **compiler:** Ensure reprovided types unify ([#2051](https://github.com/grain-lang/grain/issues/2051)) ([4c3203c](https://github.com/grain-lang/grain/commit/4c3203c9fd842f6f10bae8fcd41b889733ac3d7d))
* **compiler:** Ensure the extension is chopped in the case of module name error ([#2060](https://github.com/grain-lang/grain/issues/2060)) ([5c79c84](https://github.com/grain-lang/grain/commit/5c79c8487b522c4f89ea4994105426b46d2bd04a))
* **compiler:** Fix memory bug in universal exports ([#1663](https://github.com/grain-lang/grain/issues/1663)) ([87724f0](https://github.com/grain-lang/grain/commit/87724f0a2c1e0697a138ebb3f2ba98a337be88f5))
* **compiler:** Inline record exception constructors ([#1709](https://github.com/grain-lang/grain/issues/1709)) ([ffd4d44](https://github.com/grain-lang/grain/commit/ffd4d44d0d341f9c835d24cbebe0ac4e317fe93b))
* **compiler:** Properly apply well-formedness checks on patterns ([#1989](https://github.com/grain-lang/grain/issues/1989)) ([00abee8](https://github.com/grain-lang/grain/commit/00abee89e5dd40d658ff44b72c8c9ffc576cd628))
* **compiler:** Properly handle parsing return of negative number ([#1611](https://github.com/grain-lang/grain/issues/1611)) ([0a5a97c](https://github.com/grain-lang/grain/commit/0a5a97c8fb7cfd3f6ac068fb343e66457381fc13))
* **compiler:** Provide appropriate error for empty char literals ([#1728](https://github.com/grain-lang/grain/issues/1728)) ([e4dcda8](https://github.com/grain-lang/grain/commit/e4dcda8615b0c611ec201ac5f315e99b3ba1bfb3))
* **compiler:** Provide full module interface in intermediate wasm files ([#1756](https://github.com/grain-lang/grain/issues/1756)) ([683f803](https://github.com/grain-lang/grain/commit/683f803885e1ea073b7693dc1446b379fc49d311))
* **compiler:** Re-providing types ([#1874](https://github.com/grain-lang/grain/issues/1874)) ([6df9eb2](https://github.com/grain-lang/grain/commit/6df9eb24b740975be179cccee2bf70bac2414f33))
* **compiler:** Remove number constant folding optimization ([#1676](https://github.com/grain-lang/grain/issues/1676)) ([181798d](https://github.com/grain-lang/grain/commit/181798db69e708c69ca08a24785de7c03b9c45ab))
* **compiler:** Remove static runtime pointers ([#1806](https://github.com/grain-lang/grain/issues/1806)) ([8d76dc9](https://github.com/grain-lang/grain/commit/8d76dc917cf109e3b8496faca565fd9904fa0b22))
* **compiler:** Supply correct locations for attributes ([#2048](https://github.com/grain-lang/grain/issues/2048)) ([b19df94](https://github.com/grain-lang/grain/commit/b19df94191e325def4c1ec8f9afb8cac4dcf56e0))
* **compiler:** Support deeply nested types in modules ([#1673](https://github.com/grain-lang/grain/issues/1673)) ([b6651bb](https://github.com/grain-lang/grain/commit/b6651bbc589810dcea123920d9257d339307ed3d))
* **compiler:** Type aliasing in `use` statements ([#1887](https://github.com/grain-lang/grain/issues/1887)) ([79ab814](https://github.com/grain-lang/grain/commit/79ab81404f38cc347c719ecd1c50b9658d424aaa))
* Fix String.ReplaceAll ([#1705](https://github.com/grain-lang/grain/issues/1705)) ([d3af004](https://github.com/grain-lang/grain/commit/d3af0049f6910b10065ddd72c46e5364fe53c4ff))
* **graindoc:** Escape * characters that would close markdown bold incorrectly ([3b6b5ca](https://github.com/grain-lang/grain/commit/3b6b5ca74e3b40a41206ba5f3d527166ea267ada))
* **graindoc:** Escape * chars that would close markdown bold incorrectly ([#1685](https://github.com/grain-lang/grain/issues/1685)) ([3b6b5ca](https://github.com/grain-lang/grain/commit/3b6b5ca74e3b40a41206ba5f3d527166ea267ada))
* **graindoc:** Use Markdown bold helper for each case of bolding ([#1695](https://github.com/grain-lang/grain/issues/1695)) ([d798433](https://github.com/grain-lang/grain/commit/d798433fbb99eace2f100c0cd94c78eb99b0cf93))
* **lsp:** Fix lsp crash when hovering on function with default arg ([#1969](https://github.com/grain-lang/grain/issues/1969)) ([dd25d02](https://github.com/grain-lang/grain/commit/dd25d02675b93b36e17c1bfa37030c9cbff1dbb7))
* **lsp:** Prevent lsp crash when module cannot be found ([#2003](https://github.com/grain-lang/grain/issues/2003)) ([e4b97ea](https://github.com/grain-lang/grain/commit/e4b97ea51a6a8aab92b1cf43de578ef7eb65415d))
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
* **compiler:** Remove built-in FileDescriptor type ([#1747](https://github.com/grain-lang/grain/issues/1747)) ([d40e84a](https://github.com/grain-lang/grain/commit/d40e84a74479dfb2676005e8496369d36e4eaaba))
* **compiler:** Remove memory import from _genv ([#1830](https://github.com/grain-lang/grain/issues/1830)) ([c6693dd](https://github.com/grain-lang/grain/commit/c6693dde9ce99fc8606754b859a5cc41dda4a52f))
* **compiler:** Remove recursive_types and principal config ([#1751](https://github.com/grain-lang/grain/issues/1751)) ([534bdca](https://github.com/grain-lang/grain/commit/534bdcafe463128eca499c558459a9882478fb2a))
* **compiler:** Rename `_grainEnv` to `_genv` ([#1607](https://github.com/grain-lang/grain/issues/1607)) ([73c9499](https://github.com/grain-lang/grain/commit/73c94997c7bd08324398ea87498d2b498c5bf658))
* **compiler:** Require floats always have a digit on left & right of dot ([#1703](https://github.com/grain-lang/grain/issues/1703)) ([b657e1c](https://github.com/grain-lang/grain/commit/b657e1cf68d4b20bfb06d662b1fc8687836e5104))
* **graindoc:** Remove section attribute ([#1681](https://github.com/grain-lang/grain/issues/1681)) ([e969ad7](https://github.com/grain-lang/grain/commit/e969ad75e017785d12decbbf87651bd8c7dd286d))
* Remove --parser-debug-level ([#1311](https://github.com/grain-lang/grain/issues/1311)) ([#1447](https://github.com/grain-lang/grain/issues/1447)) ([f78587a](https://github.com/grain-lang/grain/commit/f78587a734538d66dfae94abbb7566b25810352b))
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

### [0.5.13](https://github.com/grain-lang/grain/compare/grain-v0.5.12...grain-v0.5.13) (2023-01-07)


### Features

* **compiler:** Record spread syntax ([#1565](https://github.com/grain-lang/grain/issues/1565)) ([ab60e12](https://github.com/grain-lang/grain/commit/ab60e1269f008daf1af343364e706312060a9122))


### Bug Fixes

* **compiler:** Fix error location reporting of unbound modules and labels ([#1577](https://github.com/grain-lang/grain/issues/1577)) ([5146b14](https://github.com/grain-lang/grain/commit/5146b145468f280ea39af276d8b345c17a0595da))

### [0.5.12](https://github.com/grain-lang/grain/compare/grain-v0.5.11...grain-v0.5.12) (2023-01-05)


### Bug Fixes

* **grainfmt:** Handle comments after braces properly ([654ab8c](https://github.com/grain-lang/grain/commit/654ab8cd946ea99261e37d85ea76bc9a49325311))
* **grainfmt:** Handle comments after braces properly when on one line ([#1578](https://github.com/grain-lang/grain/issues/1578)) ([654ab8c](https://github.com/grain-lang/grain/commit/654ab8cd946ea99261e37d85ea76bc9a49325311))
* **grainfmt:** Properly handle coalesing operator ([#1568](https://github.com/grain-lang/grain/issues/1568)) ([4595f97](https://github.com/grain-lang/grain/commit/4595f97d8013a57bccfd09138d3351e94887a6ba))
* **stdlib:** Remove memory corruption in File.fdReaddir ([#1573](https://github.com/grain-lang/grain/issues/1573)) ([060fc7b](https://github.com/grain-lang/grain/commit/060fc7ba4e5c4d86098eafbee09bbce13bca32c3))

### [0.5.11](https://github.com/grain-lang/grain/compare/grain-v0.5.10...grain-v0.5.11) (2022-12-29)


### Bug Fixes

* **compiler:** Fix dependency compilation of wasi polyfill ([#1563](https://github.com/grain-lang/grain/issues/1563)) ([fa8b209](https://github.com/grain-lang/grain/commit/fa8b209629deeb9f088d086311504dbde8371189))
* **grainfmt:** Fix grouping of expressions on LHS of fn application, record access etc ([#1562](https://github.com/grain-lang/grain/issues/1562)) ([6c46015](https://github.com/grain-lang/grain/commit/6c460156335cd3e864eb71f05f83fe6a8f8593ae))

### [0.5.10](https://github.com/grain-lang/grain/compare/grain-v0.5.9...grain-v0.5.10) (2022-12-23)


### Features

* **compiler:** Allow lists and arrays on the left side of access expressions ([#1557](https://github.com/grain-lang/grain/issues/1557)) ([8c00750](https://github.com/grain-lang/grain/commit/8c00750c099587f8131ea30e346841137c52bd83))
* **docs:** Update approvals process using CODEOWNERS ([#1541](https://github.com/grain-lang/grain/issues/1541)) ([3eb9f7d](https://github.com/grain-lang/grain/commit/3eb9f7de8a85f1d200cce6092826233257ae2579))


### Bug Fixes

* **compiler:** Provide appropriate error during invalid array access ([#1556](https://github.com/grain-lang/grain/issues/1556)) ([3d7b9e3](https://github.com/grain-lang/grain/commit/3d7b9e3d99b2639b46080e1c564d1453cd80f666))

### [0.5.9](https://github.com/grain-lang/grain/compare/grain-v0.5.8...grain-v0.5.9) (2022-12-14)


### Features

* **compiler:** Cache parsetrees during compilation process ([#1548](https://github.com/grain-lang/grain/issues/1548)) ([7b53aab](https://github.com/grain-lang/grain/commit/7b53aaba74c6eb809cddcde5374f63eaa4a00d36))


### Bug Fixes

* **compiler:** Properly parse large hex numbers ([#1546](https://github.com/grain-lang/grain/issues/1546)) ([5f8d770](https://github.com/grain-lang/grain/commit/5f8d770ec60b686c3183bd8a200800c3f03e8e30))

### [0.5.8](https://github.com/grain-lang/grain/compare/grain-v0.5.7...grain-v0.5.8) (2022-12-10)


### Features

* **compiler:** Lower stack usage ([#1542](https://github.com/grain-lang/grain/issues/1542)) ([ef43141](https://github.com/grain-lang/grain/commit/ef431412c310b4adffa4b5786581a6b18942a32e))
* **runtime:** Optimize simple number comparison ([f7ceae7](https://github.com/grain-lang/grain/commit/f7ceae729260bc3c5eee017954aa7e242c3f8cb5))
* **stdlib:** Add `ImmutableArray` module ([#1477](https://github.com/grain-lang/grain/issues/1477)) ([a34d8b9](https://github.com/grain-lang/grain/commit/a34d8b9e8ce629c40d65b99561c0f41fa400542e))


### Bug Fixes

* **stdlib:** Fix NaN comparisons ([#1543](https://github.com/grain-lang/grain/issues/1543)) ([f7ceae7](https://github.com/grain-lang/grain/commit/f7ceae729260bc3c5eee017954aa7e242c3f8cb5))

### [0.5.7](https://github.com/grain-lang/grain/compare/grain-v0.5.6...grain-v0.5.7) (2022-12-06)


### Bug Fixes

* **compiler:** Prevent stack overflows when compiling long blocks ([#1534](https://github.com/grain-lang/grain/issues/1534)) ([dc6d699](https://github.com/grain-lang/grain/commit/dc6d699cffeefa71ef2898c484f840c991f66c7a))

### [0.5.6](https://github.com/grain-lang/grain/compare/grain-v0.5.5...grain-v0.5.6) (2022-12-05)


### Bug Fixes

* **compiler:** Increase JS stack size ([#1532](https://github.com/grain-lang/grain/issues/1532)) ([9d1ff1f](https://github.com/grain-lang/grain/commit/9d1ff1f427f2b2a92aa7958cf84b2d1026b1bac5))

### [0.5.5](https://github.com/grain-lang/grain/compare/grain-v0.5.4...grain-v0.5.5) (2022-12-05)


### Features

* **compiler:** Perform partial match checking on `let` bindings ([#1486](https://github.com/grain-lang/grain/issues/1486)) ([3678eb5](https://github.com/grain-lang/grain/commit/3678eb5dc4c4ef199c1313f8f280ce3de458005a))
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

* **compiler:** Fix precedence of `>>` operator ([#1515](https://github.com/grain-lang/grain/issues/1515)) ([1565c16](https://github.com/grain-lang/grain/commit/1565c16e98dd5c137fb852d23f890f8d7c70d352))
* **compiler:** Raise appropriate error on unbound export ([#1528](https://github.com/grain-lang/grain/issues/1528)) ([43d512d](https://github.com/grain-lang/grain/commit/43d512dbcfb61cd10674a83658ed23b4e0ea8898))
* **compiler:** Raise appropriate exception when modules are missing during dependency graph construction ([#1485](https://github.com/grain-lang/grain/issues/1485)) ([d8cd8f1](https://github.com/grain-lang/grain/commit/d8cd8f11aec66eb257bd1cfc9f7884dcd787efd6))
* **grainfmt:** Add parentheses around some binops for precedence clarity ([#1514](https://github.com/grain-lang/grain/issues/1514)) ([3ac27cc](https://github.com/grain-lang/grain/commit/3ac27cc6e17b896dae0ef2cb5f5de510c7c2dd60))
* **grainfmt:** Avoid duplicating comments inside records ([#1428](https://github.com/grain-lang/grain/issues/1428)) ([b993030](https://github.com/grain-lang/grain/commit/b99303013698312ff936519645698c05e1bfd807))
* **grainfmt:** Handle comment in or near if conditionals better ([#1513](https://github.com/grain-lang/grain/issues/1513)) ([8a60985](https://github.com/grain-lang/grain/commit/8a609854ffe40f55e0d147655f6991b7efc3be50))
* **grainfmt:** Handle multiple line items and comments better ([#1460](https://github.com/grain-lang/grain/issues/1460)) ([5395fd4](https://github.com/grain-lang/grain/commit/5395fd45b79fb3bcf3dd1ec52a1d5973a23a4bdc))
* **grainfmt:** Maintain comments in function arguments ([#1527](https://github.com/grain-lang/grain/issues/1527)) ([6ad6dc1](https://github.com/grain-lang/grain/commit/6ad6dc12a15d2166277d990bca49f66b5177e0f9))
* **grainfmt:** Reduce binop format time ([#1482](https://github.com/grain-lang/grain/issues/1482)) ([e850455](https://github.com/grain-lang/grain/commit/e850455ff8dff5844de128637b42e8b8dcb7daf0))
* **grainlsp:** Properly surface errors in other files ([#1490](https://github.com/grain-lang/grain/issues/1490)) ([6a09953](https://github.com/grain-lang/grain/commit/6a099533ffecb801f3ccbcc9c66a57a86737a90c))
* **runtime:** Properly divide bigints in the number type ([59e89d1](https://github.com/grain-lang/grain/commit/59e89d12b7fcf2626c8adb45c742a787171b7024))

### [0.5.4](https://github.com/grain-lang/grain/compare/grain-v0.5.3...grain-v0.5.4) (2022-11-12)


### Features

* **compiler:** Allow custom infix operators ([#1419](https://github.com/grain-lang/grain/issues/1419)) ([cc9b8b5](https://github.com/grain-lang/grain/commit/cc9b8b5f251268c59ff6739436bd3b8881e8fd56))
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

* **compiler:** Fully expand types when finding concrete representation ([#1448](https://github.com/grain-lang/grain/issues/1448)) ([a5ff379](https://github.com/grain-lang/grain/commit/a5ff379f15a0839671d8d0e10c3334658e3fbafc))
* **compiler:** Panic immediately when out of memory ([#1450](https://github.com/grain-lang/grain/issues/1450)) ([943d47d](https://github.com/grain-lang/grain/commit/943d47dddde2d88fd96727e9d7ed8501efec42ef))
* **compiler:** Use option converter for `--profile` argument ([#1432](https://github.com/grain-lang/grain/issues/1432)) ([57776f2](https://github.com/grain-lang/grain/commit/57776f224904f2d422923a1a947b5acbe435cfa4))
* **grainfmt:** Group expressions inside array accessors ([#1462](https://github.com/grain-lang/grain/issues/1462)) ([dd11dd4](https://github.com/grain-lang/grain/commit/dd11dd4191f9369bff195dc71bd1660b57d222af))
* **grainfmt:** Handle chained value bindings properly ([#1467](https://github.com/grain-lang/grain/issues/1467)) ([07bfcd3](https://github.com/grain-lang/grain/commit/07bfcd3f15c34ef99b05531591b1473f206b7395))
* **grainfmt:** Handle comments within lists ([#1429](https://github.com/grain-lang/grain/issues/1429)) ([05b5f52](https://github.com/grain-lang/grain/commit/05b5f525fea50db20b49b4b5cb88c4b0259d2805))
* **grainfmt:** Handle source files with no code, only comments ([#1411](https://github.com/grain-lang/grain/issues/1411)) ([a435d60](https://github.com/grain-lang/grain/commit/a435d60f4c8f8c85ec28ea8a29e76d0ceea0284b))
* **grainfmt:** Indent lines when wrapping infix operators ([#1465](https://github.com/grain-lang/grain/issues/1465)) ([d705849](https://github.com/grain-lang/grain/commit/d705849ea8d9073e608576b77adeae834c454e0b))
* **grainfmt:** Print comments found between comma-separated data statements ([#1430](https://github.com/grain-lang/grain/issues/1430)) ([be4b38d](https://github.com/grain-lang/grain/commit/be4b38d87b0ee6ff4544264b8bad4e36820808a2))
* **grainfmt:** Properly format comments within a record ([#1435](https://github.com/grain-lang/grain/issues/1435)) ([3c56097](https://github.com/grain-lang/grain/commit/3c56097e867a83698f26e287dbaeaee7e831efb8))
* **grainfmt:** Stop adding an extra line after block comments ([#1436](https://github.com/grain-lang/grain/issues/1436)) ([4e4903f](https://github.com/grain-lang/grain/commit/4e4903f238a3804e687b1a430ec7896f7a145271))
* **runtime:** Handle bigint mul/div within Number correctly ([#1475](https://github.com/grain-lang/grain/issues/1475)) ([0fe8aa6](https://github.com/grain-lang/grain/commit/0fe8aa6a96a9c5ebf2f2bf2e1f28578badfb337f))
* **stdlib:** Fix anchoring behavior in Regex.replaceAll ([#1440](https://github.com/grain-lang/grain/issues/1440)) ([d513eff](https://github.com/grain-lang/grain/commit/d513effe569d0aa0d44c974596fd285f1ad8d57d))
* **stdlib:** Fix handling of `NaN` and `Infinity` in Number module ([#1457](https://github.com/grain-lang/grain/issues/1457)) ([c24f6c1](https://github.com/grain-lang/grain/commit/c24f6c1cfae87632a003c0337c29ec98a80cfda2))

### [0.5.3](https://github.com/grain-lang/grain/compare/grain-v0.5.2...grain-v0.5.3) (2022-08-05)


### Features

* Arm64 docker builds ([#1368](https://github.com/grain-lang/grain/issues/1368)) ([5837df6](https://github.com/grain-lang/grain/commit/5837df6f39a35416e23f162b42f2f0a9088e1807))
* **compiler:** Unicode-aware lexing ([#1342](https://github.com/grain-lang/grain/issues/1342)) ([6dc0dd1](https://github.com/grain-lang/grain/commit/6dc0dd1bc80bbfa9da41f82e9f89569d5f0e6d01))
* Implement Pervasives.compare ([#1399](https://github.com/grain-lang/grain/issues/1399)) ([ebd87e4](https://github.com/grain-lang/grain/commit/ebd87e4308a8950fd95f060ebc446833b064237c))
* **stdlib:** Add `charCodeAt` function to String module ([#1376](https://github.com/grain-lang/grain/issues/1376)) ([c3abbc9](https://github.com/grain-lang/grain/commit/c3abbc991c8b05e3de20e670d2bc3e491feebf8a))
* **stdlib:** Add `lastIndexOf` function to String module ([#1372](https://github.com/grain-lang/grain/issues/1372)) ([b73d9bf](https://github.com/grain-lang/grain/commit/b73d9bf9ff3291b83e6f4263e392395d04dc9995))
* **stdlib:** Implement `isFloat`, `isInteger` & `isRational` in Number module ([#1393](https://github.com/grain-lang/grain/issues/1393)) ([0af0669](https://github.com/grain-lang/grain/commit/0af066993a2b80e417d2c625b27fd11cb1f1f55f))
* **stdlib:** Implement List.zip, List.unzip, List.zipWith, Array.reduceRight, Array.zipWith ([#1363](https://github.com/grain-lang/grain/issues/1363)) ([3e7c147](https://github.com/grain-lang/grain/commit/3e7c147fea2d2fb8b7c5a3d6b3eb1453f2861e36))
* **stdlib:** Implement mutable/immutable priority queues ([#1397](https://github.com/grain-lang/grain/issues/1397)) ([244be1b](https://github.com/grain-lang/grain/commit/244be1b7254caf0b451902ac56413382eb196747))
* **stdlib:** Marshal ([#1352](https://github.com/grain-lang/grain/issues/1352)) ([d659de2](https://github.com/grain-lang/grain/commit/d659de2d92260f7726164876827c639bfd9d0590))


### Bug Fixes

* **compiler:** Allow defining _start when using --use-start-section ([#1349](https://github.com/grain-lang/grain/issues/1349)) ([944f71c](https://github.com/grain-lang/grain/commit/944f71ca8080b8a3228d27893b2fd5b84e4b6189))
* **compiler:** Apply list location to outer node ([#1387](https://github.com/grain-lang/grain/issues/1387)) ([06e79a8](https://github.com/grain-lang/grain/commit/06e79a80e5e61e5a8654654dfa15310a1bc363ea))
* **compiler:** Correctly handle underscores in bigint literals ([0af0669](https://github.com/grain-lang/grain/commit/0af066993a2b80e417d2c625b27fd11cb1f1f55f))
* **graindoc:** Enable printing of arrow types ([#1367](https://github.com/grain-lang/grain/issues/1367)) ([e2b28ab](https://github.com/grain-lang/grain/commit/e2b28ab66ecd9169c27a2680ba5e0fb79e589787))
* **graindoc:** Use defined module name throughout generated doc ([#1406](https://github.com/grain-lang/grain/issues/1406)) ([c33a777](https://github.com/grain-lang/grain/commit/c33a777a93d5e40a081991db5e6ea61ade4fbabc))
* **grainfmt:** Format rational numbers with parens when needed for precedence ([#1385](https://github.com/grain-lang/grain/issues/1385)) ([931bb42](https://github.com/grain-lang/grain/commit/931bb42b4a8bc610765d871947f7deaef354511b))
* **grainfmt:** Use only the correct comments for constraints ([#1388](https://github.com/grain-lang/grain/issues/1388)) ([91d5cdf](https://github.com/grain-lang/grain/commit/91d5cdf63d7be8a68cf613c6ba217c5810cf3920))
* **grainlsp:** Send empty response if file cannot be formatted ([#1386](https://github.com/grain-lang/grain/issues/1386)) ([38a3f58](https://github.com/grain-lang/grain/commit/38a3f58b5cf4b61654cc07f3fad0b9be10abb983))
* **stdlib:** Correctly promote numbers to bigints when left-shifting ([#1354](https://github.com/grain-lang/grain/issues/1354)) ([5280e98](https://github.com/grain-lang/grain/commit/5280e98a91a57fae074299fc7bad3c41f69fa2a3))
* **stdlib:** Prevent addBytesSlice throwing error on empty buffer ([#1394](https://github.com/grain-lang/grain/issues/1394)) ([bdd4be4](https://github.com/grain-lang/grain/commit/bdd4be46730290908b7b939f41679acce7834167))

### [0.5.2](https://github.com/grain-lang/grain/compare/grain-v0.5.1...grain-v0.5.2) (2022-06-29)


### Features

* **compiler:** Allow compilation of empty programs ([#1336](https://github.com/grain-lang/grain/issues/1336)) ([32df14b](https://github.com/grain-lang/grain/commit/32df14b33325c39fd98c452fc2687b788ec35264))
* **compiler:** Allow newline in or-patterns ([#1323](https://github.com/grain-lang/grain/issues/1323)) ([8fe8247](https://github.com/grain-lang/grain/commit/8fe8247ec6c08e04575b42281716eb34c46af97f))
* **compiler:** Support record field updating assignment ([#1320](https://github.com/grain-lang/grain/issues/1320)) ([cc5519c](https://github.com/grain-lang/grain/commit/cc5519ce36418623b05d297d577c2325ad9cbc7a))
* **stdlib:** Add number constants to number libraries ([#1331](https://github.com/grain-lang/grain/issues/1331)) ([f640ec2](https://github.com/grain-lang/grain/commit/f640ec20aa507c83c9cde290b911d0adcb4e8254))
* **stdlib:** Implement Number.sin and Number.cos ([#1343](https://github.com/grain-lang/grain/issues/1343)) ([9357126](https://github.com/grain-lang/grain/commit/93571267b7df53e1cb9f61eaebf8748885e7392c))


### Bug Fixes

* **compiler:** Correct error message for unbound type variables ([#1327](https://github.com/grain-lang/grain/issues/1327)) ([762362a](https://github.com/grain-lang/grain/commit/762362a06e1676540cd33df8200ea2a6b7006a5e))
* **compiler:** Fix compilation of closure scope mutable destructuring ([#1346](https://github.com/grain-lang/grain/issues/1346)) ([bdcdb3c](https://github.com/grain-lang/grain/commit/bdcdb3c66a9a60fc44956225491c12cc14250009))
* **compiler:** Properly handle types not imported ([#1329](https://github.com/grain-lang/grain/issues/1329)) ([75bbb93](https://github.com/grain-lang/grain/commit/75bbb9367a43c923fc97b25ea0ed9315190f5702))
* **compiler:** Support printing tuple types in oprint ([#1324](https://github.com/grain-lang/grain/issues/1324)) ([c1d8907](https://github.com/grain-lang/grain/commit/c1d89076cad153ed4aa02593f628f4d152ff7139))
* **grainfmt:** Support PPatOr and PPatAlias ([#1322](https://github.com/grain-lang/grain/issues/1322)) ([20050d2](https://github.com/grain-lang/grain/commit/20050d2c975f60ae7ead41c76871562f7636c499))
* **lsp:** Always respond to hover request ([#1328](https://github.com/grain-lang/grain/issues/1328)) ([fb947b5](https://github.com/grain-lang/grain/commit/fb947b5dc47f306f6399d2b4564fe8f33ffd80b1))
* **stdlib:** Make toNumber functions respect Number invariants ([#1347](https://github.com/grain-lang/grain/issues/1347)) ([78db882](https://github.com/grain-lang/grain/commit/78db8820cf5667a4d6737c9109f4223c1348b245))

### [0.5.1](https://github.com/grain-lang/grain/compare/grain-v0.5.0...grain-v0.5.1) (2022-06-08)


### Bug Fixes

* **compiler:** Properly handle corrupted CMIs ([#1314](https://github.com/grain-lang/grain/issues/1314)) ([7f97410](https://github.com/grain-lang/grain/commit/7f9741016fac55bd63aa74d9bd2fef49dd34d03b))

## [0.5.0](https://github.com/grain-lang/grain/compare/grain-v0.4.7...grain-v0.5.0) (2022-06-05)


### ⚠ BREAKING CHANGES

* **stdlib:** Use explicit exports for Pervasives (#1301)
* **stdlib:** Remove `sum` function from the List module (#1300)
* **stdlib:** Ensure Void return for forEach functions in List module (#1307)
* **stdlib:** Provide correct types for BigInt operations (#1297)
* **compiler:** Remove `--lsp` flag from grainc executable
* **lsp:** Replaced one-off LSP command with persistent LSP server (#1131)
* **cli:** Simplify version output (#1291)
* **cli:** Show all global options within help for every command (#1285)
* **cli:** Remove graceful flag & behavior (#1275)
* **compiler:** Refactor HeapAllocated/StackAllocated into Managed/Unmanaged (#1268)
* **compiler:** Replace optimization levels with compilation profiles (#1270)
* **grainfmt:** Replace `--in-place` flag with `-o` flag
* **grainfmt:** Remove stdin formatting support
* **compiler:** Arbitrary-Precision Integer Arithmetic (#1167)
* Switch from yarn to npm (#1226)
* Drop node 14 support (#1092)
* **stdlib:** Add explicit void return type on Set.forEach (#1225)
* **stdlib:** Add explicit void return type on Map.forEach (#1220)
* **compiler:** Selectively include functions in the global function table (#1183)
* **compiler:** Add interface for compile module to hide resume
* **compiler:** Disallow end-of-line before `=>` (#1178)
* **stdlib:** Add type aliases to regex lib (#1036)
* **compiler:** Stack-allocated Chars (#1103)
* **stdlib:** Align Buffer's `addStringSlice` API with String's `slice` (#1136)
* **graindoc:** Add `--current-version` flag, required for since/history attributes (#1116)
* **compiler:** Remove decRefIgnoreZeros (#1068)
* **compiler:** Add `--memory-base` flag (#1115)
* **compiler:** Re-implement Grain parser (#1033)

### Features

* **ci:** Link all versions during release ([#1290](https://github.com/grain-lang/grain/issues/1290)) ([ceb8dac](https://github.com/grain-lang/grain/commit/ceb8dac2e3d3bf7eb92f91daf29173a779a4aa7e))
* **compiler:** Add `--memory-base` flag ([#1115](https://github.com/grain-lang/grain/issues/1115)) ([0680826](https://github.com/grain-lang/grain/commit/068082663c4387c3ab54c052869e9b9a06b87e26))
* **compiler:** Add `[@unsafe](https://github.com/unsafe)` attribute for low-level code ([#1074](https://github.com/grain-lang/grain/issues/1074)) ([212faca](https://github.com/grain-lang/grain/commit/212faca9a8363002cfc0cb4e3ea293180cbe5b1c))
* **compiler:** Add warning for calls to IntXX.fromNumber and FloatXX.fromNumber with literal integers/floats ([#1218](https://github.com/grain-lang/grain/issues/1218)) ([2fb86e5](https://github.com/grain-lang/grain/commit/2fb86e538a9d38241d6c149cec1a290817795b4b))
* **compiler:** Allow function re-exports to use regular call instruction ([#1176](https://github.com/grain-lang/grain/issues/1176)) ([afce3aa](https://github.com/grain-lang/grain/commit/afce3aaa5382cde54b1e8fd9be3d2ee86daedbbb))
* **compiler:** Arbitrary-Precision Integer Arithmetic ([#1167](https://github.com/grain-lang/grain/issues/1167)) ([6f34de2](https://github.com/grain-lang/grain/commit/6f34de214b28358ea1df553685fa3a19336ddba9))
* **compiler:** Call known functions across module boundaries ([#1175](https://github.com/grain-lang/grain/issues/1175)) ([b2d7440](https://github.com/grain-lang/grain/commit/b2d744034ec7e0601554531c910e9d0f5451d464))
* **compiler:** Cleaner wasm output for low-level wasm types ([#1158](https://github.com/grain-lang/grain/issues/1158)) ([88060dd](https://github.com/grain-lang/grain/commit/88060ddb8119e2998d91f4d6770a1cfd101936cf))
* **compiler:** Consolidate exe & js modes ([fc61950](https://github.com/grain-lang/grain/commit/fc6195013457dd29f78951322bfaf2ae27c1bdd2))
* **compiler:** Convert `runtime/dataStructures.gr` to primitives ([#1145](https://github.com/grain-lang/grain/issues/1145)) ([2d43b28](https://github.com/grain-lang/grain/commit/2d43b286141df75f6b92300e48d2bc4804014872))
* **compiler:** Convert Sys libraries to [@unsafe](https://github.com/unsafe) ([#1272](https://github.com/grain-lang/grain/issues/1272)) ([fcdfc2a](https://github.com/grain-lang/grain/commit/fcdfc2a815be889d5f0424a04dd5b2373dbd983b))
* **compiler:** Don't close over global values ([#1134](https://github.com/grain-lang/grain/issues/1134)) ([e8caec6](https://github.com/grain-lang/grain/commit/e8caec6c2a5892e955c8827b18d8d436bebe6073))
* **compiler:** Name globals in wasm output ([#1184](https://github.com/grain-lang/grain/issues/1184)) ([51170e7](https://github.com/grain-lang/grain/commit/51170e7c892680f9c730b7f93cf744ffd6b7c15a))
* **compiler:** Re-implement Grain parser ([#1033](https://github.com/grain-lang/grain/issues/1033)) ([9dc3c96](https://github.com/grain-lang/grain/commit/9dc3c96f87a0b2affe9db36e1b03360d198f79f1))
* **compiler:** Reduce closure sizes by utilizing `$self` argument when possible ([#1152](https://github.com/grain-lang/grain/issues/1152)) ([ba6a84c](https://github.com/grain-lang/grain/commit/ba6a84cea191cf3c7932287c29b198001d490146))
* **compiler:** Refactor exports ([#1244](https://github.com/grain-lang/grain/issues/1244)) ([4637667](https://github.com/grain-lang/grain/commit/4637667cd669c0988cd01c2957534c6a35a3146c))
* **compiler:** Replace optimization levels with compilation profiles ([#1270](https://github.com/grain-lang/grain/issues/1270)) ([1a27c12](https://github.com/grain-lang/grain/commit/1a27c127e8f0318c21fec7ab358ee8e1ad2378e9))
* **compiler:** Selectively include functions in the global function table ([#1183](https://github.com/grain-lang/grain/issues/1183)) ([67575f7](https://github.com/grain-lang/grain/commit/67575f712557bc36531f6e40044d1bbfff2454ff))
* **compiler:** Stack-allocated Chars ([#1103](https://github.com/grain-lang/grain/issues/1103)) ([095385e](https://github.com/grain-lang/grain/commit/095385e7c67bbc7a417a21acaf6f1c498c75ce63))
* **compiler:** Support pattern aliases ([#1174](https://github.com/grain-lang/grain/issues/1174)) ([9ed093b](https://github.com/grain-lang/grain/commit/9ed093be353895bdde8282ee1681089d5fac68ab))
* **compiler:** Support pattern matching "or" patterns ([#1173](https://github.com/grain-lang/grain/issues/1173)) ([0fb29c4](https://github.com/grain-lang/grain/commit/0fb29c4016e1b24b9e3b3b640cc9914b921ec376))
* **compiler:** Upgrade binaryen to 0.15.0 to support Mac M1 arch ([#1151](https://github.com/grain-lang/grain/issues/1151)) ([fc61950](https://github.com/grain-lang/grain/commit/fc6195013457dd29f78951322bfaf2ae27c1bdd2))
* **compiler:** Use symbol for equals sign ([#1128](https://github.com/grain-lang/grain/issues/1128)) ([3d7fc57](https://github.com/grain-lang/grain/commit/3d7fc5742ad3016d60e14112fdd68de02fc147f1))
* **graindoc:** Add `--current-version` flag, required for since/history attributes ([#1116](https://github.com/grain-lang/grain/issues/1116)) ([0f681ea](https://github.com/grain-lang/grain/commit/0f681ea140749395f3ce99a460f30778537183ac))
* **graindoc:** Allow directory input & output ([#1263](https://github.com/grain-lang/grain/issues/1263)) ([d4cb8ab](https://github.com/grain-lang/grain/commit/d4cb8abcb4accafeb3cae0bac77eee9a365e464d))
* **grainfmt:** Allow directory input & output ([#1274](https://github.com/grain-lang/grain/issues/1274)) ([d3e7a33](https://github.com/grain-lang/grain/commit/d3e7a33b01352a9c2bcc3b17a5b2995451d92221))
* **grainfmt:** Replace `--in-place` flag with `-o` flag ([d3e7a33](https://github.com/grain-lang/grain/commit/d3e7a33b01352a9c2bcc3b17a5b2995451d92221))
* **lsp:** Replaced one-off LSP command with persistent LSP server ([#1131](https://github.com/grain-lang/grain/issues/1131)) ([df91849](https://github.com/grain-lang/grain/commit/df91849bd65a729fe4e0b03f51bc6d28017935cb))
* **stdlib:** Add module for pseudo-random number generation ([#921](https://github.com/grain-lang/grain/issues/921)) ([db1fa4e](https://github.com/grain-lang/grain/commit/db1fa4e491d35bb582beaba12157884647384a77))
* **stdlib:** Add unsigned versions of Int32/Int64 comparison operations ([#831](https://github.com/grain-lang/grain/issues/831)) ([5f20868](https://github.com/grain-lang/grain/commit/5f20868e7b6e3f52d62c8531d99d1130ca84961e))
* **stdlib:** Added Bytes.clear function. Avoid allocation in Buffer.clear ([#1124](https://github.com/grain-lang/grain/issues/1124)) ([4afd17a](https://github.com/grain-lang/grain/commit/4afd17a91e59027fa7af0fe58977bdbe942f8072))
* **stdlib:** Convert runtime printing utils to [@unsafe](https://github.com/unsafe) ([#1135](https://github.com/grain-lang/grain/issues/1135)) ([403e1d2](https://github.com/grain-lang/grain/commit/403e1d20e2082fe2dcd2721f83f6c2b36d4154bf))


### Bug Fixes

* **ci:** Add a last-release-sha to release-please ([#1296](https://github.com/grain-lang/grain/issues/1296)) ([abf9749](https://github.com/grain-lang/grain/commit/abf9749d8188970a27bd6f60213261af352f2e6f))
* **cli:** Ensure parent flags are inherited by the format command ([d3e7a33](https://github.com/grain-lang/grain/commit/d3e7a33b01352a9c2bcc3b17a5b2995451d92221))
* **cli:** Show all global options within help for every command ([#1285](https://github.com/grain-lang/grain/issues/1285)) ([1357e16](https://github.com/grain-lang/grain/commit/1357e162f7e939db21468186d16e6d720b557a57))
* **compiler:** Apply correct allocation type to numbers ([#1140](https://github.com/grain-lang/grain/issues/1140)) ([b9e9d59](https://github.com/grain-lang/grain/commit/b9e9d59143f529f7c0ae100b048988e6ba0e8d54))
* **compiler:** Avoid module aliases of themselves ([df91849](https://github.com/grain-lang/grain/commit/df91849bd65a729fe4e0b03f51bc6d28017935cb))
* **compiler:** Fix compilation of functions annotated with alias ([#1293](https://github.com/grain-lang/grain/issues/1293)) ([b3e1882](https://github.com/grain-lang/grain/commit/b3e188299be6af22bb64cf5ae9a41afa05e4c16f))
* **compiler:** fix formatter multiple data bug ([#1282](https://github.com/grain-lang/grain/issues/1282)) ([f6fd962](https://github.com/grain-lang/grain/commit/f6fd96250dbe95d6b6c56d663f6d7f2704b792b3))
* **compiler:** Iterate over guard clauses in typedTreeIter/typedTreeMap ([#1283](https://github.com/grain-lang/grain/issues/1283)) ([96e8ecb](https://github.com/grain-lang/grain/commit/96e8ecb9e84e54d0640bc775e23e25b93c4dda7c))
* **compiler:** Move Filename usage to Filepath.String & normalize separators ([584bcad](https://github.com/grain-lang/grain/commit/584bcad942f91d86c9b328e61f34af6e3cfbd050))
* **compiler:** Preserve all configs when compiling ([#1207](https://github.com/grain-lang/grain/issues/1207)) ([d8ff903](https://github.com/grain-lang/grain/commit/d8ff9037fc3148384d2157f76e4394322ba4ed58))
* **compiler:** Read custom sections in full ([#1243](https://github.com/grain-lang/grain/issues/1243)) ([f8a0891](https://github.com/grain-lang/grain/commit/f8a0891f253e33153feb2ec20afe6443391a1aa8))
* **compiler:** Refactor WASI polyfill resolution ([#1261](https://github.com/grain-lang/grain/issues/1261)) ([a39b48f](https://github.com/grain-lang/grain/commit/a39b48ff95f4ef7ce5a4b5454dbc620ae4044c17))
* **compiler:** Report binop locations properly ([#1271](https://github.com/grain-lang/grain/issues/1271)) ([4b9dd9c](https://github.com/grain-lang/grain/commit/4b9dd9c35969cf86f1d2d9a8013b7f6d347c508e))
* **compiler:** Report Win32 instead of Cygwin in JS compiler ([#1251](https://github.com/grain-lang/grain/issues/1251)) ([584bcad](https://github.com/grain-lang/grain/commit/584bcad942f91d86c9b328e61f34af6e3cfbd050))
* **compiler:** Respect stack type when dropping statements ([#1138](https://github.com/grain-lang/grain/issues/1138)) ([5a44e93](https://github.com/grain-lang/grain/commit/5a44e93ab646f1e9998ce6c23245e41c4bf0019a))
* **compiler:** Supply correct error for unbound record labels ([#1200](https://github.com/grain-lang/grain/issues/1200)) ([86e1bc0](https://github.com/grain-lang/grain/commit/86e1bc0ba099c5b48ba72e98530a634e15f6bb77))
* **graindoc:** Add parens around infix operators in titles ([#1303](https://github.com/grain-lang/grain/issues/1303)) ([acba9c1](https://github.com/grain-lang/grain/commit/acba9c1757688756c3ca98b22a0a159b8d8f9e7d))
* **graindoc:** Avoid singletons when building ordered comments ([#1208](https://github.com/grain-lang/grain/issues/1208)) ([3f28e6e](https://github.com/grain-lang/grain/commit/3f28e6e226bbd951aedc6f8783203152919c7d08))
* **graindoc:** Fix type printing for types and abstract types ([#1238](https://github.com/grain-lang/grain/issues/1238)) ([aea3ea9](https://github.com/grain-lang/grain/commit/aea3ea9f6b72182dfffb8af4c9af83a444fb6a83))
* **graindoc:** Improve location lookup so re-exports do not crash it ([#1280](https://github.com/grain-lang/grain/issues/1280)) ([6e782ee](https://github.com/grain-lang/grain/commit/6e782ee146e16aefd3b88441de500d82a20508be))
* **graindoc:** Preserve indentation in Doc comments during trim ([#1119](https://github.com/grain-lang/grain/issues/1119)) ([b8a6d57](https://github.com/grain-lang/grain/commit/b8a6d57cce274bfbc2cc16c5b25215b042d4264c))
* **graindoc:** Remove spaces between parens and infix idents ([#1302](https://github.com/grain-lang/grain/issues/1302)) ([95e596f](https://github.com/grain-lang/grain/commit/95e596fa3fdae5a8a0e07d76ff8c11eeab99e8d7))
* **graindoc:** Use value_descriptions and type_declarations defined by the module signature ([#1241](https://github.com/grain-lang/grain/issues/1241)) ([5896242](https://github.com/grain-lang/grain/commit/5896242c324622f3329c144bd2c9642aade9d049))
* **grainfmt:** Preserve the EOL characters of file ([#1216](https://github.com/grain-lang/grain/issues/1216)) ([ef2835a](https://github.com/grain-lang/grain/commit/ef2835a5608cb1d5de52b280b6d4fd5b563ce725))
* **grainfmt:** Properly handle if/then/else line breaks ([#1217](https://github.com/grain-lang/grain/issues/1217)) ([93fd3b5](https://github.com/grain-lang/grain/commit/93fd3b5d0a5ccbc11baaab08d0f65ff9ec98aead))
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

* **cli:** Remove graceful flag & behavior ([#1275](https://github.com/grain-lang/grain/issues/1275)) ([df55898](https://github.com/grain-lang/grain/commit/df5589882d12ed35ba448de44e06f434bcf59b07))
* **cli:** Simplify version output ([#1291](https://github.com/grain-lang/grain/issues/1291)) ([97f99f4](https://github.com/grain-lang/grain/commit/97f99f4a53c9434175d8bd8bac3c08cdd4257c77))
* **compiler:** Add interface for compile module to hide resume ([d8ff903](https://github.com/grain-lang/grain/commit/d8ff9037fc3148384d2157f76e4394322ba4ed58))
* **compiler:** Disallow end-of-line before `=>` ([#1178](https://github.com/grain-lang/grain/issues/1178)) ([8261e73](https://github.com/grain-lang/grain/commit/8261e733c9345c690ffc107616512be4eb4c469e))
* **compiler:** Refactor HeapAllocated/StackAllocated into Managed/Unmanaged ([#1268](https://github.com/grain-lang/grain/issues/1268)) ([1590a37](https://github.com/grain-lang/grain/commit/1590a37ca39e39ee40e0f27986fe8195458c3cb9))
* **compiler:** Remove `--lsp` flag from grainc executable ([df91849](https://github.com/grain-lang/grain/commit/df91849bd65a729fe4e0b03f51bc6d28017935cb))
* **compiler:** Remove decRefIgnoreZeros ([#1068](https://github.com/grain-lang/grain/issues/1068)) ([3ae8eaa](https://github.com/grain-lang/grain/commit/3ae8eaabad4467304c500c2f0cc9c40749d8513b))
* Drop node 14 support ([#1092](https://github.com/grain-lang/grain/issues/1092)) ([ef4358f](https://github.com/grain-lang/grain/commit/ef4358ff7de14a35edf3e971e04513d497fe1574))
* **grainfmt:** Remove stdin formatting support ([d3e7a33](https://github.com/grain-lang/grain/commit/d3e7a33b01352a9c2bcc3b17a5b2995451d92221))
* **stdlib:** Add type aliases to regex lib ([#1036](https://github.com/grain-lang/grain/issues/1036)) ([a926ea5](https://github.com/grain-lang/grain/commit/a926ea5cafa18e46487a585abbdf5460b15b4f48))
* **stdlib:** Remove `sum` function from the List module ([#1300](https://github.com/grain-lang/grain/issues/1300)) ([9101615](https://github.com/grain-lang/grain/commit/9101615688f20310ae32573f93f36cfcf5c69be1))
* Switch from yarn to npm ([#1226](https://github.com/grain-lang/grain/issues/1226)) ([5ea9274](https://github.com/grain-lang/grain/commit/5ea92743a05fffb4298deda64100a3d7fc2259cb))

### [0.4.7](https://www.github.com/grain-lang/grain/compare/grain-v0.4.6...grain-v0.4.7) (2022-01-17)


### Bug Fixes

* **compiler:** Account for config that is not digestable ([#1098](https://www.github.com/grain-lang/grain/issues/1098)) ([b58323f](https://www.github.com/grain-lang/grain/commit/b58323f1764f4d870875b8c4ef19f9861b4188ad))
* **compiler:** Apply optimization flag at link stage ([#1099](https://www.github.com/grain-lang/grain/issues/1099)) ([49dac79](https://www.github.com/grain-lang/grain/commit/49dac797aa4833cc141f7c58d18d80d7cbc6b1c7))
* **compiler:** Inform the compiler that lsp mode is running on a root file ([b58323f](https://www.github.com/grain-lang/grain/commit/b58323f1764f4d870875b8c4ef19f9861b4188ad))
* **compiler:** Move lsp flag into grainc to remove it as a recompilation flag ([b58323f](https://www.github.com/grain-lang/grain/commit/b58323f1764f4d870875b8c4ef19f9861b4188ad))
* **graindoc:** Remove special idents from receiving parens ([#1104](https://www.github.com/grain-lang/grain/issues/1104)) ([5cb0fda](https://www.github.com/grain-lang/grain/commit/5cb0fda652cad0644f48d8196089430dca1cac0d))
* **grainfmt:** Indent function application args when adding parens ([#1095](https://www.github.com/grain-lang/grain/issues/1095)) ([64af7d3](https://www.github.com/grain-lang/grain/commit/64af7d387dca2fddb9b3d190ccdf5790ec3d8e65))
* **grainfmt:** Inform the formatter that it is running against root files ([b58323f](https://www.github.com/grain-lang/grain/commit/b58323f1764f4d870875b8c4ef19f9861b4188ad))
* **grainfmt:** Scope the comments in callback lambda closure ([#1090](https://www.github.com/grain-lang/grain/issues/1090)) ([0975580](https://www.github.com/grain-lang/grain/commit/09755801d8598691e4a2884a732c0ed9a9e10a2d))

### [0.4.6](https://www.github.com/grain-lang/grain/compare/grain-v0.4.5...grain-v0.4.6) (2021-12-31)


### Features

* **compiler:** Inline all primitives ([#1076](https://www.github.com/grain-lang/grain/issues/1076)) ([c227130](https://www.github.com/grain-lang/grain/commit/c2271307b7220d54dc1c252dfc4ee5532f68dd29))
* **grainfmt:** Drastically improve performance ([#1042](https://www.github.com/grain-lang/grain/issues/1042)) ([9bc7a55](https://www.github.com/grain-lang/grain/commit/9bc7a55a525c8018e9d915cf94af2f2486a21e27))
* **stdlib:** Add sign function to Number module ([#1079](https://www.github.com/grain-lang/grain/issues/1079)) ([b6483d5](https://www.github.com/grain-lang/grain/commit/b6483d5046cd1b6b89a717a925594d3b20b05837))


### Bug Fixes

* **compiler:** Correctly supply locations for types & variants ([9bc7a55](https://www.github.com/grain-lang/grain/commit/9bc7a55a525c8018e9d915cf94af2f2486a21e27))
* **grainfmt:** Improve correctness of formatting ([9bc7a55](https://www.github.com/grain-lang/grain/commit/9bc7a55a525c8018e9d915cf94af2f2486a21e27))

### [0.4.5](https://www.github.com/grain-lang/grain/compare/grain-v0.4.4...grain-v0.4.5) (2021-12-11)


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
* **stdlib:** Fix String.encode GC ([#1067](https://www.github.com/grain-lang/grain/issues/1067)) ([0ab38c9](https://www.github.com/grain-lang/grain/commit/0ab38c9f4aa0ee84688ba5c6bec1521b380d38b1))
* **stdlib:** Removed memory leak in Hash module ([#1045](https://www.github.com/grain-lang/grain/issues/1045)) ([01a81c6](https://www.github.com/grain-lang/grain/commit/01a81c6a2573cca94b2d57d0fc70693d39f810a1))
* **stdlib:** Removed memory leaks in Buffer module ([#1047](https://www.github.com/grain-lang/grain/issues/1047)) ([d33017b](https://www.github.com/grain-lang/grain/commit/d33017b37e988d3facbca2e30e3de4fb8c7b5b8a))
* **stdlib:** Support arrays of any type in Array rotate ([#1048](https://www.github.com/grain-lang/grain/issues/1048)) ([3ceb1cf](https://www.github.com/grain-lang/grain/commit/3ceb1cf04c1604f49077e8733dcccb6cdaaf9f3a))
* **stdlib:** Support empty arrays in Array rotate ([3ceb1cf](https://www.github.com/grain-lang/grain/commit/3ceb1cf04c1604f49077e8733dcccb6cdaaf9f3a))

### [0.4.4](https://www.github.com/grain-lang/grain/compare/grain-v0.4.3...grain-v0.4.4) (2021-10-27)


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
* **stdlib:** Fixed memory leak in String.explode ([#1001](https://www.github.com/grain-lang/grain/issues/1001)) ([c479a05](https://www.github.com/grain-lang/grain/commit/c479a05f48abcc4c9e98d5cf0ba698230d41031b))

### [0.4.3](https://www.github.com/grain-lang/grain/compare/grain-v0.4.2...grain-v0.4.3) (2021-10-11)


### Features

* Add npm releases to workflow ([#948](https://www.github.com/grain-lang/grain/issues/948)) ([d55bbc0](https://www.github.com/grain-lang/grain/commit/d55bbc081c5dffb4532b0abca838716d969a919b))
* **cli:** Refactor forwardable options ([#908](https://www.github.com/grain-lang/grain/issues/908)) ([9ecb49b](https://www.github.com/grain-lang/grain/commit/9ecb49b7fc29ab8cb09baefe512471c07b9dbdc6))
* **compiler:** Inline not `(!)` operator ([#937](https://www.github.com/grain-lang/grain/issues/937)) ([3f5e9a9](https://www.github.com/grain-lang/grain/commit/3f5e9a962ec8565f4fb79a1ee36d7b492da5cf11))
* **grainfmt:** Add format in place or output to a new file ([#904](https://www.github.com/grain-lang/grain/issues/904)) ([0d18935](https://www.github.com/grain-lang/grain/commit/0d1893576bf3ad9e4d5c3aca5bfa963966b84b66))
* **stdlib:** Add Conv.wasmI32ToNumber function ([#978](https://www.github.com/grain-lang/grain/issues/978)) ([c93ade8](https://www.github.com/grain-lang/grain/commit/c93ade873b51acca556e19a7483a5f29b4b6caa9))
* **stdlib:** Add string trim functions to String module ([#951](https://www.github.com/grain-lang/grain/issues/951)) ([e55de8f](https://www.github.com/grain-lang/grain/commit/e55de8fc0335b45252da9741a80b46a702cdb5b6))
* **stdlib:** Regular Expressions ([#680](https://www.github.com/grain-lang/grain/issues/680)) ([9601e16](https://www.github.com/grain-lang/grain/commit/9601e1655f1ffc76b700efef317366457c5614ef))


### Bug Fixes

* **cli:** Forward stdlib option to lsp and graindoc ([#916](https://www.github.com/grain-lang/grain/issues/916)) ([07c5237](https://www.github.com/grain-lang/grain/commit/07c5237795c9717cf925c6889e165f4eb8a04f8d))
* **cli:** Only add --experimental-wasm-bigint flag on node 14 or 15 ([#899](https://www.github.com/grain-lang/grain/issues/899)) ([1cc0c5c](https://www.github.com/grain-lang/grain/commit/1cc0c5c03d38957c90f71e99a188c448923a481c))
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
* **stdlib:** Fix memory leaks in Char stdlib ([#929](https://www.github.com/grain-lang/grain/issues/929)) ([99cc94b](https://www.github.com/grain-lang/grain/commit/99cc94b3e2010c7fa6b2c244758dbbe13e43f903))

### [0.4.2](https://www.github.com/grain-lang/grain/compare/grain-v0.4.1...grain-v0.4.2) (2021-09-07)


### Bug Fixes

* **grainfmt:** Remove extra spacing from type signatures ([#877](https://www.github.com/grain-lang/grain/issues/877)) ([c8cf421](https://www.github.com/grain-lang/grain/commit/c8cf421940fb21c06c5e3f9a11c72670bccccfb2))
* **stdlib:** Annotate generic types in Buffer ([#876](https://www.github.com/grain-lang/grain/issues/876)) ([ad46b9e](https://www.github.com/grain-lang/grain/commit/ad46b9e6b0d2d9d562b56e5cbf1ae6751028d93a))
* **stdlib:** Correct fdPwrite return value ([#875](https://www.github.com/grain-lang/grain/issues/875)) ([ceaf6af](https://www.github.com/grain-lang/grain/commit/ceaf6af52889f5d7c8a16df9a5a9ae5ff6752105))
* **stdlib:** Correct type signatures on some Array functions ([#880](https://www.github.com/grain-lang/grain/issues/880)) ([04bf4d3](https://www.github.com/grain-lang/grain/commit/04bf4d3066cf80de8cb3da834124e29558c6d21a))

### [0.4.1](https://www.github.com/grain-lang/grain/compare/grain-v0.4.0...grain-v0.4.1) (2021-09-06)


### Bug Fixes

* **cli:** Manually bump CLI dependencies ([#871](https://www.github.com/grain-lang/grain/issues/871)) ([1e4a7a1](https://www.github.com/grain-lang/grain/commit/1e4a7a171c8dd41caf54e6218d59dffafef8f226))

## [0.4.0](https://www.github.com/grain-lang/grain/compare/grain-v0.3.2...grain-v0.4.0) (2021-09-06)


### ⚠ BREAKING CHANGES

* **compiler:** Add typed well-formedness pass which forbids usage of WasmXX values outside of `@disableGC` context (#772)
* **stdlib:** Use random seed for hash module (#854)
* **stdlib:** Remove deprecated functions (#812)
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
* **stdlib:** Export coerceNumberToWasmI32/I64/F32/F64 from Number runtime (#713)
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
* Support mutually recursive data definitions ([#725](https://www.github.com/grain-lang/grain/issues/725)) ([4474f88](https://www.github.com/grain-lang/grain/commit/4474f887514caa35132724ad2c658cc5328807c5))


### Bug Fixes

* Add [@disable](https://www.github.com/disable)GC annotation to WasmXX-using getSize function in Bytes ([#771](https://www.github.com/grain-lang/grain/issues/771)) ([bc4146b](https://www.github.com/grain-lang/grain/commit/bc4146b426860f519d4fb7bb4e8345e227dffc69))
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
* **js-runner:** Add default object for destructuring ([#833](https://www.github.com/grain-lang/grain/issues/833)) ([ab96fd6](https://www.github.com/grain-lang/grain/commit/ab96fd6c29f297f0e5423f3a6011fe07cf488df8))
* **js-runner:** Monkeypatch fs_write in browser environment ([#832](https://www.github.com/grain-lang/grain/issues/832)) ([330b56f](https://www.github.com/grain-lang/grain/commit/330b56f3bdda17bf993e88bb35532831f95c63b3))
* **lsp:** Pattern-match the phase to match the stop hook ([#760](https://www.github.com/grain-lang/grain/issues/760)) ([c4da4d9](https://www.github.com/grain-lang/grain/commit/c4da4d938ffa15a21dbdef7b2648b37e2278b079))
* Properly initialize malloc free list ([#700](https://www.github.com/grain-lang/grain/issues/700)) ([0576fd9](https://www.github.com/grain-lang/grain/commit/0576fd9be8dbfecc5d7081731a1c5bedee32f9f1))
* Record function-call associativity ([#701](https://www.github.com/grain-lang/grain/issues/701)) ([a6ca63f](https://www.github.com/grain-lang/grain/commit/a6ca63fd8b3830b076d649e8c78cdde7b86507ca))
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

* **compiler:** Avoid building JS bundles by default ([#739](https://www.github.com/grain-lang/grain/issues/739)) ([2925fb8](https://www.github.com/grain-lang/grain/commit/2925fb8149cb09659e0267a581f0390b73b0f119))
* **compiler:** Rename `grainRuntime` to `_grainEnv` ([2d99c5a](https://www.github.com/grain-lang/grain/commit/2d99c5ab8fa527328f18d471e8a9128561af7056))
* Refactor out references to "runtime" that aren't related to `stdlib/runtime` ([#755](https://www.github.com/grain-lang/grain/issues/755)) ([2d99c5a](https://www.github.com/grain-lang/grain/commit/2d99c5ab8fa527328f18d471e8a9128561af7056))
* Rename JS "runtime" to "js-runner"/"runner" throughout project ([2d99c5a](https://www.github.com/grain-lang/grain/commit/2d99c5ab8fa527328f18d471e8a9128561af7056))
* **stdlib:** Remove deprecated functions ([#812](https://www.github.com/grain-lang/grain/issues/812)) ([7b74208](https://www.github.com/grain-lang/grain/commit/7b7420860b588862e1947c6fc9860b5bbaf1ff75))

### [0.3.2](https://www.github.com/grain-lang/grain/compare/grain-v0.3.1...grain-v0.3.2) (2021-05-29)


### Features

* **compiler:** add better hover to LSP ([#667](https://www.github.com/grain-lang/grain/issues/667)) ([7c7e9d7](https://www.github.com/grain-lang/grain/commit/7c7e9d7a568cc350f6323a8941858c04bb46cac2))
* **compiler:** Upgrade to binaryen.ml v0.10.0 ([#623](https://www.github.com/grain-lang/grain/issues/623)) ([940f0f1](https://www.github.com/grain-lang/grain/commit/940f0f17bc75e6aa861798a765215db0955bbe72))
* Export number runtime functions as operators & deprecate old identifiers ([#629](https://www.github.com/grain-lang/grain/issues/629)) ([b99441a](https://www.github.com/grain-lang/grain/commit/b99441aebc8c6d643be460177277112428e7cf58))
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

### [0.3.1](https://www.github.com/grain-lang/grain/compare/grain-v0.3.0...grain-v0.3.1) (2021-04-29)


### ⚠ BREAKING CHANGES

* rename Queue/Stack lib methods (#625)

### Bug Fixes

* **cli:** Correct typo in lsp catch block ([#618](https://www.github.com/grain-lang/grain/issues/618)) ([b903ced](https://www.github.com/grain-lang/grain/commit/b903ced32ad7a6a2055a73473921e791adfd6ae4))
* Workaround for Windows drive letters with jsoo & pkg ([#636](https://www.github.com/grain-lang/grain/issues/636)) ([d205cde](https://www.github.com/grain-lang/grain/commit/d205cde76c97d9b830653c48004b97ec9dc25840))


### Reverts

* "chore!: rename Queue/Stack lib methods ([#625](https://www.github.com/grain-lang/grain/issues/625))" ([#632](https://www.github.com/grain-lang/grain/issues/632)) ([c9af90e](https://www.github.com/grain-lang/grain/commit/c9af90e77e5c5bb9649b8072b9cdff16ae2d1e60))


### Miscellaneous Chores

* rename Queue/Stack lib methods ([#625](https://www.github.com/grain-lang/grain/issues/625)) ([8117592](https://www.github.com/grain-lang/grain/commit/8117592dbaa09b4443003f234b6a2dcadb235a8b))

## [0.3.0](https://www.github.com/grain-lang/grain/compare/grain-v0.2.0...grain-v0.3.0) (2021-04-21)


### ⚠ BREAKING CHANGES

* **cli:** Remove `-f` CLI option
* **compiler:** Remove --principal-types and --recursive-types compiler flags
* **compiler:** Rename --cdebug to --verbose
* **cli:** Pass compiler flags from the CLI to grainc directly (#613)
* Correct type signature for `_start`
* Introduce `_gmain` for old behavior of `_start`
* Tail calls must be enabled explicitly via `--experimental-wasm-tail-call`
* Static linking of Grain modules (#584)
* **stdlib:** Replace JS entrypoint with locator file (#586)
* Streamline runtime dependencies & entrypoints (#585)
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
* Add support for loading Grain modules directly from an ArrayBuffer and running them ([f07f305](https://www.github.com/grain-lang/grain/commit/f07f30541b5d5d4e579db2d5e41b15841fc4ebbc))
* add support for LSP lenses ([#416](https://www.github.com/grain-lang/grain/issues/416)) ([753a0dc](https://www.github.com/grain-lang/grain/commit/753a0dc7fa02766a930238077a5deea66b0bf158))
* add update method to map stdlib ([#421](https://www.github.com/grain-lang/grain/issues/421)) ([292b5f9](https://www.github.com/grain-lang/grain/commit/292b5f9bbf1ea84b0540fe828fb1e3dba712340f))
* Allow newline when creating array ([#447](https://www.github.com/grain-lang/grain/issues/447)) ([114cb1a](https://www.github.com/grain-lang/grain/commit/114cb1a85b55086bb72e95ab886ff93eaf5d7162))
* Allow pattern matching on unsafe wasm i32s ([#532](https://www.github.com/grain-lang/grain/issues/532)) ([8839c55](https://www.github.com/grain-lang/grain/commit/8839c5542718f35d10f3958559862faab506a0b6))
* allow setting the initial memory via CLI option ([#448](https://www.github.com/grain-lang/grain/issues/448)) ([213ee5a](https://www.github.com/grain-lang/grain/commit/213ee5a2736a71eb6a62a28bd60f338e196ea265))
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
* Grain implementation of number libraries ([#542](https://www.github.com/grain-lang/grain/issues/542)) ([d90d38b](https://www.github.com/grain-lang/grain/commit/d90d38b8e5756ea15f90504652af5698cb8822f8))
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
* Can't use memory.data ([#480](https://www.github.com/grain-lang/grain/issues/480)) ([1a4c3b9](https://www.github.com/grain-lang/grain/commit/1a4c3b9ff32d261aaf1a3670b14904c90ed22f76))
* **cli:** Rework JSOO hacks to ensure stdin always gets data ([#614](https://www.github.com/grain-lang/grain/issues/614)) ([b064119](https://www.github.com/grain-lang/grain/commit/b064119fe2a10dca68e30f4d66db455d37991ead))
* **compiler:** Always get little endian i64 from bytes ([#587](https://www.github.com/grain-lang/grain/issues/587)) ([1b9853a](https://www.github.com/grain-lang/grain/commit/1b9853ad3ebc91c1022efbf484532254c67c845e))
* **compiler:** Implement proper dependency resolver ([#590](https://www.github.com/grain-lang/grain/issues/590)) ([d9ce392](https://www.github.com/grain-lang/grain/commit/d9ce392341acebd0b46a0e6fe78469e23e73ee92))
* **compiler:** Resolve names when doing CRC checks ([#615](https://www.github.com/grain-lang/grain/issues/615)) ([15ea9a1](https://www.github.com/grain-lang/grain/commit/15ea9a18c0c5a8762a4e232720a2654c33881dac))
* **compiler:** Update binaryen.ml bindings to include JS fixes ([#600](https://www.github.com/grain-lang/grain/issues/600)) ([7bd9ac6](https://www.github.com/grain-lang/grain/commit/7bd9ac69a1ba9c9506e8d65b1f51831a63fdf294))
* Correct type signature for `_start` ([3d4ac6e](https://www.github.com/grain-lang/grain/commit/3d4ac6edd1b805d7cada5e6fa951bb6afb0467d9))
* **docker:** Build our own image of ubuntu+node14 ([#617](https://www.github.com/grain-lang/grain/issues/617)) ([3053d64](https://www.github.com/grain-lang/grain/commit/3053d643642c79a35d767e3292f69cde9f8726fc))
* Don't optimize away expressions which could possibly contain side effects ([#523](https://www.github.com/grain-lang/grain/issues/523)) ([acc7d65](https://www.github.com/grain-lang/grain/commit/acc7d655621a1397481a10e8840c547ae5690d2b))
* ensure Dockerfile works with `yarn prepare` command ([#418](https://www.github.com/grain-lang/grain/issues/418)) ([af40e03](https://www.github.com/grain-lang/grain/commit/af40e03fcef58eb7360cff5d23d1eec8b2001d40))
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
* replace shebang trick with flagged-respawn to support Windows ([#430](https://www.github.com/grain-lang/grain/issues/430)) ([4ea2602](https://www.github.com/grain-lang/grain/commit/4ea26021d270adc542736efa90093d2f2cdbef5b))
* Reset type vars when printing types ([#420](https://www.github.com/grain-lang/grain/issues/420)) ([bb17787](https://www.github.com/grain-lang/grain/commit/bb17787ce8b183569053b51f7d0ef0e88a1550bd))
* Rework memory management ([#461](https://www.github.com/grain-lang/grain/issues/461)) ([84318b0](https://www.github.com/grain-lang/grain/commit/84318b01a21137492e9728f346680225f1d1ea9a))
* **runtime:** Properly decRef record and array values ([#500](https://www.github.com/grain-lang/grain/issues/500)) ([88adb94](https://www.github.com/grain-lang/grain/commit/88adb942ecf5425b74540a9cddbcb775128d97eb))
* **runtime:** Return correct pointer to morecore ([#602](https://www.github.com/grain-lang/grain/issues/602)) ([af39cc4](https://www.github.com/grain-lang/grain/commit/af39cc46c8e12d869aaf5c42c0a3fd2afbc61638))
* set test sharding to 1 in attempt to avoid test race condition ([#440](https://www.github.com/grain-lang/grain/issues/440)) ([3783ec6](https://www.github.com/grain-lang/grain/commit/3783ec62efbe5b7b76d14cd070e17e99f7737cca))
* temporarily disable inline tests ([#434](https://www.github.com/grain-lang/grain/issues/434)) ([0bc446a](https://www.github.com/grain-lang/grain/commit/0bc446a4149942416e8d439a223c0d39f7c028d4))
* Throwing an exception now traps immediately in all cases ([61c2a94](https://www.github.com/grain-lang/grain/commit/61c2a94dbe38ff074e6b53395c403d30996b60a0))
* Tidy up type printing ([#417](https://www.github.com/grain-lang/grain/issues/417)) ([14c7bd6](https://www.github.com/grain-lang/grain/commit/14c7bd6cbfa497e4a0f691288415865e865beb7d))
* use direct node call instead of executable file ([#431](https://www.github.com/grain-lang/grain/issues/431)) ([747ee8d](https://www.github.com/grain-lang/grain/commit/747ee8d8938b112befc018d81111f244c6884c18))
* Use Is instead of Eq for match variant comparison ([1f1cd4a](https://www.github.com/grain-lang/grain/commit/1f1cd4a90f853a2c6290e736043d008643f768f7))
* Use proper return type for calls to external functions ([3d4ac6e](https://www.github.com/grain-lang/grain/commit/3d4ac6edd1b805d7cada5e6fa951bb6afb0467d9))
* utilize path.join to create filesystem paths in default locator ([#432](https://www.github.com/grain-lang/grain/issues/432)) ([8aa4fa0](https://www.github.com/grain-lang/grain/commit/8aa4fa0e454dfbfc828e27f6b0cc6ed042d22a3b))
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
* **stdlib:** Replace JS entrypoint with locator file ([#586](https://www.github.com/grain-lang/grain/issues/586)) ([a9ec7e2](https://www.github.com/grain-lang/grain/commit/a9ec7e237f22fe4eb507919972f4c41aa5eed55b))
* Streamline runtime dependencies & entrypoints ([#585](https://www.github.com/grain-lang/grain/issues/585)) ([bbdd38d](https://www.github.com/grain-lang/grain/commit/bbdd38d5f4e5958aa10d602ba6b57a588024b050))
* Tail calls must be enabled explicitly via `--experimental-wasm-tail-call` ([3d4ac6e](https://www.github.com/grain-lang/grain/commit/3d4ac6edd1b805d7cada5e6fa951bb6afb0467d9))
