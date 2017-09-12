const path = require("path");
const glob = require("glob");

module.exports = {
  // どのファイルをビルドするのかを指定。複数可。
  entry: [
    './all-test.js'
  ],
  // 出力するファイル名と出力先パス
  output: {
    path: path.join(__dirname + '/lib/js'),
    filename: 'test_bundle.js'
  },
  devtool: 'eval',
  // requireで読み込むときのrootのpathを指定
  resolve: {
    extensions: ['.js'],
  },
}
