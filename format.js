const shell = require('shelljs');

shell.exec("ocp-indent -i ./src/*.ml");
shell.exec("ocp-indent -i ./test/*.ml");
