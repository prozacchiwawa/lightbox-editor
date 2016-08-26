var vdi = require('./vdominterface');
var $ = require('jquery');

function mapNode(vdom, node) {
    console.log(JSON.stringify(node));
    if (typeof(node) == 'string') {
        return vdom.vtext(node);
    } else {
        var namespace = node.namespace ? node.namespace : null;
        var key = node.key;
        var children = [];
        var c = node.children || {head:null, tail:null};
        var cout = {head:null, tail:null};
        while (c.head !== null) {
            cout = {head:mapNode(vdom, c.head), tail:cout};
            c = c.tail;
        }
        var attributes = node.attributes || {head:null, tail:null};
        var tag = node.tag;
        return vdom.vnode(tag)(attributes)({head:null, tail:null})(cout);
    }
}

function sizeQuery(elem) {
    var children = [];
    for (var i = 0; i < elem.childNodes.length; i++) {
        var child = elem.childNodes[i];
        if (child.nodeType === Node.ELEMENT_NODE) {
            children.push(sizeQuery(child));
        }
    }
    var position = elem.getBoundingClientRect();
    var size = {
        'type': 'panel',
        'key': elem.getAttribute('key'),
        'bounds': { 
            x: position.left, 
            y: position.top, 
            width: position.right - position.left, 
            height: position.bottom - position.top
        },
        'children': children
    };
    return size;
}

vdi.run(document.getElementById('app'), function(vdom) {
    function init(arg) {
        return { root: null };
    };
    function update(evt, state) {
        if (evt.type === 'render') {
            console.log('evt',evt);
            return { root: evt.data };
        }
        return state;
    };
    function view(state) {
        var root = state.root || {};
        var elem = mapNode(vdom, root);
        var scrollTop = $('#app').scrollTop();
        setTimeout(function() {
            window.parent.postMessage({
                type: 'measure', 
                scrollTop: scrollTop, 
                data: sizeQuery(document.getElementById('app'))
            }, window.location.href);
        }, 0);
        return elem;
    };
    window.addEventListener('message', function(evt) {
        if (evt.data.type === 'render') {
            console.log('msg', evt.data);
            vdom.post(evt.data);
        }
    });
    return { init: init, update: update, view: view };
}, null);
