const webpack = require("webpack");
const merge = require("webpack-merge");

const common = {
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /(node_modules|wasmer-js)/,
        use: {
          loader: "babel-loader",
          options: {
            presets: [],
            plugins: ["transform-object-rest-spread"],
          },
        },
      },
    ],
  },
  plugins: [
    new webpack.DefinePlugin({
      __DEBUG: JSON.stringify(false),
    }),
  ],
};

const browserConfig = merge(common, {
  entry: "./src/index.js",
  output: {
    filename: "grain-runner-browser.js",
    path: __dirname + "/dist",
    library: "Grain",
    libraryTarget: "var",
  },
  node: {
    fs: "empty",
  },
  plugins: [
    new webpack.ProvidePlugin({
      wasiBindings: "@wasmer/wasi/lib/bindings/browser",
    }),
    new webpack.DefinePlugin({
      __RUNNER_BROWSER: JSON.stringify(true),
    }),
  ],
});

const nodeConfig = merge(common, {
  entry: "./src/runner.js",
  output: {
    filename: "grain-runner.js",
    path: __dirname + "/dist",
    libraryTarget: "commonjs2",
  },
  externals: ["fs", "crypto", "path", "tty"],
  node: false,
  plugins: [
    new webpack.ProvidePlugin({
      wasiBindings: "@wasmer/wasi/lib/bindings/node",
    }),
    new webpack.DefinePlugin({
      __RUNNER_BROWSER: JSON.stringify(false),
    }),
  ],
});

module.exports = [browserConfig, nodeConfig];
