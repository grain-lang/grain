//#!node --expose-wasm
//const fs = require('fs');

function printNumber(n) {
  var res;
  if (n & 1) {
    if (n === -1) {
      res = "true";
    } else {
      res = "false";
    }
  } else {
    res = (n >> 1).toString();
  }
  document.getElementById('output').innerHTML = res;
  console.log(res);
}

const importObj = { console: { log: printNumber }, js: { mem: new WebAssembly.Memory({initial: 1}) } };

/*
if (process.argv.length != 3) {
  console.error(`give file pls`);
  process.exit(1);
}*/

function fetchFileAndInstantiate(url, importObject) {

  return new Promise((resolve, reject) => fs.readFile(url, (err, data) => {
    if (err) reject(err);
    else resolve(data);
  })).then(bytes =>
    Wasm.instantiateModule(bytes, importObject)
  ).then(results =>
    results
  );
}

function fetchAndInstantiate(url, importObject) {
  return fetch(url).then(response =>
    response.arrayBuffer()
  ).then(bytes =>
    WebAssembly.instantiate(bytes, importObject)
  ).then(results =>
    results
  );
}

fetchAndInstantiate("t.wasm", importObj).then((instance) => {
  console.log(JSON.stringify(instance, null, 2));
});
