const path = require("path");
const glob = require("glob");

let entries = {}
glob.sync("./lib/js/test/**/*.js").map((file) => {
  entries[file] = file;
});

module.exports = {
  // どのファイルをビルドするのかを指定。複数可。
  entry:  entries,
  // 出力するファイル名と出力先パス
  output: {
    path: path.join(__dirname + '/lib/js'),
    filename: 'test_bundle.js'
  },
  devtool: 'inline-source-map',
  // requireで読み込むときのrootのpathを指定
  resolve: {
    extensions: ['.js'],
  },
}
