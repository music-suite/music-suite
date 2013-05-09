
var fs = require('fs');

var txt = fs.readFileSync('test.json', 'utf8');
var val = JSON.parse(txt);                     
console.log(val);