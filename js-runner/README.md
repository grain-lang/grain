# @grain/js-runner

The JavaScript runner for the Grain language.

## Why

This project makes it easier to get your Grain programs running in a JavaScript environment—either Node.js or the browser. If your Grain code is compiled in unlinked mode (`--no-link`), `@grain/js-runner` will perform module resolution and load all necessary modules.

## Installation

To install the package, install it from npm:

```sh
npm install @grain/js-runner
```

## Usage

The package provides two versions of the runner. One for the browser and one for node.

In the browser, without a bundler, add this as a script to your `index.html`:

```html
<script src="node_modules/@grain/js-runner/dist/grain-runner-browser.js"></script>
<script>
  // You'll also need to install the @grain/stdlib package
  let locator = Grain.defaultURLLocator(["/", "node_modules/@grain/stdlib"]);
  let GrainRunner = Grain.buildGrainRunner(locator);
  GrainRunner.runURL("hello.gr.wasm");
</script>
```

The easiest way to get running in the browser is to fork the [grain-web-example](https://github.com/grain-lang/grain-web-example).

In node, you can require the runner:

```js
let Grain = require("@grain/js-runner");
let locator = Grain.defaultFileLocator([
  __dirname,
  require.resolve("@grain/stdlib"),
]);
let GrainRunner = Grain.buildGrainRunner(locator);
GrainRunner.runFile("hello.gr.wasm");
```

You can also just use the Grain CLI to run your files, like `grain run hello.gr.wasm`.

## License

MIT
