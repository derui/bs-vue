let context = require.context('./lib/js/test/', true, /\.js$/);

context.keys().forEach(context);

module.exports = context;
