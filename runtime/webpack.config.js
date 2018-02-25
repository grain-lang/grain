module.exports = {
  entry: './src/runtime.js',
  output: {
    filename: 'grain-runtime.js',
    path: __dirname + '/dist',
    libraryTarget: 'commonjs2'
  },
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
}
