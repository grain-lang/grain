const webpack = require('webpack');
const merge = require('webpack-merge');
const common = require('./webpack.common.js');

module.exports = common.map(config => merge(config, {
  mode: 'production',
  plugins: [
    new webpack.DefinePlugin({
      __DEBUG: JSON.stringify(false)
    })
  ]
}));
