var q = require('q');

/*
  
  (createZip ())
  ||> createFiles fileList
  ||> zipGenerate "test.zip"
  ||> fun blob -> vdom.post (OfferDownload blob)
  ||| fun error -> vdom.post (CreateZipError error)

type 'a Promise = Unused0 of 'a

let promiseComplete : 'a Promise -> ('a -> 'b) -> 'b Promise
let promiseError : 'a Promise -> (string -> 'b) -> 'b Promise

let (||>) promise f =
  promiseComplete promise f

let (|||) promise f =
  promiseError promise f

*/
module.exports.promise = function(c) {
    return new q(c);
}

module.exports.complete = function(p,f) {
    return p.then(f);
}

module.exports.error = function(p,f) {
    return p.fail(f);
}

