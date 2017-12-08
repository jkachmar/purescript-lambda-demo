const path = require('path');
const webpack = require('webpack');

module.exports = {
  devtool: 'eval',
  entry: './dist/index.js',
  target: 'node',
  output: {
    library: 'pureinvent',
    libraryTarget: 'commonjs-module',
    path: path.join(__dirname, 'dist'),
    pathinfo: true,
    filename: 'index.js'
  }
};
