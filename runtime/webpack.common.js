const webpack = require('webpack');

const common = {
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /(node_modules|wasmer-js)/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: [],
            plugins: [
              "transform-object-rest-spread"
            ]
          }
        }
      }
    ]
  },
  externals: ['fs'],
  plugins: [
    new webpack.DefinePlugin({
      __DEBUG: JSON.stringify(false)
    })
  ]
};

const browserConfig = {
  ...common,
  entry: './src/index.js',
  output: {
    filename: 'grain-runtime-browser.js',
    path: __dirname + '/dist',
    library: 'Grain',
    libraryTarget: 'var'
  }
}

const commonjsConfig = {
  ...common,
  entry: './src/runtime.js',
  output: {
    filename: 'grain-runtime.js',
    path: __dirname + '/dist',
    libraryTarget: 'commonjs2'
  },
  externals: ['fs', 'crypto', 'path', 'tty'],
  node: false
}

module.exports = [
  browserConfig, commonjsConfig
]