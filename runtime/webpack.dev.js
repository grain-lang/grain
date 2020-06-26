const webpack = require('webpack');
const merge = require('webpack-merge');
const common = require('./webpack.common.js');

module.exports = common.map(config => merge(config, {
  devtool: 'inline-source-map',
  mode: 'development',
  plugins: [
    new webpack.DefinePlugin({
      __DEBUG: JSON.stringify(true)
    })
  ]
}));
