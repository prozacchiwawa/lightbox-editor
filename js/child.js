var vdi = require('./vdominterface');
var $ = require('jquery');

function mapNode(vdom, node) {
    console.log(JSON.stringify(node));
    if (typeof(node) == 'string') {
        return vdom.vtext(node);
    } else {
        var namespace = node.xmlns ? node.xmlns : null;
        var key = node.key;
        var c = node.children || {head:null, tail:null};
        var cout = {head:null, tail:null};
        while (c.head) {
            cout = {head:mapNode(vdom, c.head), tail:cout};
            c = c.tail;
        }
        var attributes = node.attributes || {head:null, tail:null};
        var tag = node.tag;
        return vdom.vnode(tag)(attributes)({head:null, tail:null})(cout);
    }
}

function sizeQuery(elem) {
    var children = {head:null, tail:null};
    for (var i = 0; i < elem.childNodes.length; i++) {
        var child = elem.childNodes[i];
        if (child.nodeType === Node.ELEMENT_NODE) {
            children = {head: sizeQuery(child), tail: children};
        }
    }
    var position = elem.getBoundingClientRect();
    var size = {
        'ty': 'panel',
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
        if (evt.ty === 'render') {
            return { root: evt };
        }
        return state;
    };
    function view(state) {
        var root = state.root || {};
        var elem = mapNode(vdom, root);
        var scrollTop = $('#app').scrollTop();
        setTimeout(function() {
            window.parent.postMessage({
                ty: 'measure', 
                scrollTop: scrollTop, 
                data: sizeQuery(document.getElementById('app'))
            }, window.location.href);
        }, 0);
        return elem;
    };
    vdomAddEventListener('render', function(evt) {
        vdom.post(evt);
    });
    return { init: init, update: update, view: view };
}, null);
