module.exports.keysOf = function(j) {
    var keys = []; 
    for (var k in j) { 
        keys.push(k); 
    } 
    console.log(j,'=>',keys);
    return keys;
}

module.exports.getKey = function(k,j) {
    var r = (j)[k];
    console.log('get',k,'from',j,'=>',r);
    return r ? r : null;
}

module.exports.jsonToSubkey = function(ctors,v) {
    if (v === null) {
        return ctors.jsnull(v);
    }
    switch (typeof(v)) { 
    case 'string': return (ctors.string)(v); 
    case 'number': return (ctors.float)(v); 
    case 'boolean': return (ctors.bool)(v); 
    default: 
        if (v.constructor.name == 'Array') {
            return (ctors.list)(v);
        } else {
            return (ctors.dict)(v); 
        } 
    }
}

module.exports.jsonMap = function(m) {
    var res = {};
    console.log(m);
    for (var i = 0; i < m.length; i++) {
        var e = m[i];
        res[e[0]] = e[1];
    }
    return res;
}
