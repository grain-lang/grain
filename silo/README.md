# Silo

Silo is Grain's project manager. It compiles Grain projects to WebAssembly code, and (eventually) manages dependencies, runs code, and more.

## Config

A project's Silo configuration lives in a file called `silo.toml`. [TOML files](https://toml.io/en/) consist of key-value pairs inside of tables.

Below is an overview of all settings, followed by detailed descriptions.

```toml
[package]
name = "my-package" # the name of this package

[build]
elide-type-info = false # compiles without Grain type info
include-dirs = ["../dir1", "../dir2"] # add paths to compiler code lookup
import-memory = false # imports the wasm memory instead of exporting one
initial-memory-pages = 64 # the number of wasm memory pages to start with
maximum-memory-pages = 128 # the limit of wasm memory pages to grow to
memory-base = 4096 # the memory address to begin Grain allocations
gc = true # enable or disable Grain's garbage collector
stdlib = "../custom-stdlib" # set the location of the standard library to use
use-start-section = false # use a wasm (start) section to run code on startup
wasi-polyfill = "src/wasi-polyfill.gr" # code to polyfill wasi imports

[build.wasm-features]
bulk-memory = true # enable or disable wasm bulk memory
tail-call = true # enable or disable wasm tail calls

[bin]
name = "my-name.wasm" # the name of the resulting wasm file
path = "src/main.gr" # the entrypoint of compilation
source-map = false # produce a source map file
wat = false # produce a WebAssembly Text (.wat) file
```

## Config Sections

### [package]

Package configuration.

#### package.name

The name of the package. Unless otherwise set, also the name of the resulting wasm binary.

### [build]

Compiler and other build configuration.

#### build.elide-type-info

- Type: bool
- Default: false

Removes Grain type info from the resulting binary. This is often done to reduce binary size, but causes `print`, `toString`, and thrown exceptions to produce dummy values for records and enums.

#### build.include-dirs

- Type: string array
- Default: []

Adds directories to the compiler's `include` search path.

#### build.import-memory

- Type: bool
- Default: false

Instead of exporting a memory, the resulting module will import its memory from `env.memory`.

#### build.initial-memory-pages

- Type: integer
- Default: 64

The number of WebAssembly pages to start with.

#### build.maximum-memory-pages

- Type: integer
- Default: none

The maximum number of WebAssembly pages to grow to.

#### build.memory-base

- Type: integer
- Default: 0

The memory address to begin the Grain heap. This leaves memory available for the host runtime to use freely.

#### build.gc

- Type: bool
- Default: true

Enables the Grain garbage collector. If the program is short-lived, garbage collection can be turned off for additional performance.

#### build.stdlib

- Type: string
- Default: none

The path to the Grain standard library to use. This could be the official Grain standard library or your own implementation.

#### build.use-start-section

- Type: bool
- Default: false

Uses a WebAssembly `(start)` section for module startup code instead of exporting `_start` or `_initialize` for the host to call.

#### build.wasi-polyfill

- Type: string
- Default: none

The path to a Grain implementation of the wasi-snapshot-preview1 interface.

#### build.wasm-features.bulk-memory

- Type: bool
- Default: true

Enables or disables use of wasm bulk memory instructions.

#### build.wasm-features.tail-call

- Type: bool
- Default: true

Enables or disables use of wasm tail call instructions.

### [bin]

Configuration for compiler input and output.

#### bin.name

- Type: string
- Default: "<package name>.wasm"

The name of the resulting wasm artifact.

#### bin.path

- Type: string
- Default: "src/main.gr"

The path to the program's entrypoint.

#### bin.wat

- Type: bool
- Default: false

Produce a WebAssembly Text (.wat) file in addition to the wasm file.

#### bin.source-map

- Type: bool
- Default: false

Produce a sourcemap in addition to to the wasm file.
