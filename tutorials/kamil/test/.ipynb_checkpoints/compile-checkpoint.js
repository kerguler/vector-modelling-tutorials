var fs = require('fs');
const { execSync } = require('child_process');
const keparser = require('popjson');

const filename = process.argv[2] || './model';
const filejson = `${filename}.json`;
const filec = `${filename}.c`;
const filedylib = `${filename}.dylib`;

console.log(`Reading model file: ${filejson}`);
let data = fs.readFileSync(filejson, "utf8");

const popjson = new keparser.PopJSON();
var out = popjson.parse_json(data);

if (out["error"]) {
    console.log("/* *** ERROR *** */");
    console.log(out["error"]);
    console.log("/* ************* */");
} else {
    tmp = fs.writeFileSync(filec, out["model"]);
    if (!tmp) {
        console.log(`Model translated to ${filec}`);
        try {
            let cmd = `gcc ${filec} -L/usr/local/lib -I/usr/local/include -w -lm -lpopulation -lgsl -lgslcblas -shared -fPIC -o ${filedylib}`;
            let result = execSync(cmd, { stdio: 'inherit' });
            console.log(`Model file created: ${filedylib}`);
        } catch (err) {
            console.error("Compilation failed:", err.message);
        }    
    }
}