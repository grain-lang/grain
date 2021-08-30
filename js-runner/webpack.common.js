const webpack = require("webpack");
const { merge } = require("webpack-merge");

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
            plugins: ["@babel/plugin-proposal-object-rest-spread"],
          },
        },
      },
    ],
  },
};

const browserConfig = merge(common, {
  entry: "./src/index.js",
  output: {
    filename: "grain-runner-browser.js",
    library: "Grain",
    libraryTarget: "var",
  },
  resolve: {
    fallback: {
      crypto: require.resolve("crypto-browserify"),
      fs: false,
      path: require.resolve("path-browserify"),
      stream: require.resolve("stream-browserify"),
      tty: require.resolve("tty-browserify"),
      url: require.resolve("url/"),
      util: require.resolve("util/"),
    },
  },
  plugins: [
    new webpack.ProvidePlugin({
      wasiBindings: "@wasmer/wasi/lib/bindings/browser",
      process: [require.resolve("process/browser")],
      Buffer: [require.resolve("buffer/"), "Buffer"],
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
    libraryTarget: "commonjs2",
  },
  externalsPresets: {
    node: true,
  },
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
