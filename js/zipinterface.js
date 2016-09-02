var q = require('q');
var jszip = require('jszip');
var jsziputils = require('jszip-utils');

module.exports.createZip = function() {
    return new jszip();
}

module.exports.readZip = function(blob) {
    return (new jszip()).loadAsync(blob);
}

module.exports.createZipFile = function(zip,filename,filedata) {
    zip.file(filename,filedata);
    return new q(zip);    
}

module.exports.createZipDir = function(zip,dirname) {
    return zip.folder(dirname);
}

module.exports.createZipDirFile = function(dir,filename,filedata) {
    dir.file(filename,filedata);
    return new q(dir);
}

module.exports.removeZipFile = function(zip,filename) {
    zip.remove(filename);
    return new q(zip);
}

module.exports.zipGenerate = function(zip) {
    return zip.generateAsync(responder);
}

module.exports.zipContentFileString = function(zip,name) {
    return zip.file(name).async("string");
}
