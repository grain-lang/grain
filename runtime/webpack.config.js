const common = {
  devtool: 'source-map',
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /(node_modules)/,
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
  externals: ['fs']
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
  }
}

module.exports = [
  browserConfig, commonjsConfig
]