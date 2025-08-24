var fs = require('fs');
let data = fs.readFileSync('./model.json');

const keparser = require('popjson');
const popjson = new keparser.PopJSON();
var out = popjson.parse_json(data);
if (out["error"]) {
    console.log("/* *** ERROR *** */")
    console.log(out["error"])
    console.log("/* ************* */")
}
console.log(out["model"]);
