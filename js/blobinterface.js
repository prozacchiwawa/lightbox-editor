var q = require('q');

module.exports.create = function(mimetype,inputs) {
    var inputArray = [];
    while (inputs.head) {
        if (inputs.head.tag == 'Base64Data') {
            var str = atob(inputs.head.data0);
            var ar = new Uint8Array(str.length);
            for (var i = 0; i < str.length; i++) {
                ar[i] = str.charCodeAt(i);
            }
            inputArray.push(ar);
        } else {
            inputArray.push(inputs.head.data0);
        }
        inputs = inputs.tail;
    }
    var options = { type: mimetype };
    return new Blob(inputArray, options);
}

module.exports.toBase64 = function(blob) {
    var d = q.defer();
    var reader = new FileReader();
    reader.addEventListener('load', function(url) {
        console.log(url);
        var split = url.split(',');
        console.log(split);
        d.resolve(split[1]);
    });
    reader.readAsDataURL(blob);
    return d.promise;
}
