const path = require('path');

const relativePath = '_build/install/default/lib/grain/stdlib';
const absolutePath = path.resolve(__dirname, '../../', relativePath);

module.exports = absolutePath;