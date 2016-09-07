var vdi = require('./vdominterface');
var $ = require('jquery');

function mapNode(vdom, root, node) {
    if (typeof(node) == 'string') {
        return vdom.vtext(node);
    } else {
        var namespace = node.xmlns ? node.xmlns : null;
        var key = node.key;
        var c = node.children || {head:null, tail:null};
        var cout = {head:null, tail:null};
        while (c.head) {
            cout = {head:mapNode(vdom, false, c.head), tail:cout};
            c = c.tail;
        }
        var attributes = node.attributes || {head:null, tail:null};
        attributes = {head:{name:'__key', value:key}, tail:attributes};
        var tag = node.tag;
        // Prepend text
        var text = node.text;
        if (text) {
            cout = {head:text, tail:cout};
        }
        return vdom.vnode(tag)(attributes)({head:null, tail:null})(cout);
    }
}

function sizeQuery(elem) {
    var children = [];
    for (var i = 0; i < elem.childNodes.length; i++) {
        var child = elem.childNodes[i];
        if (child.nodeType === Node.ELEMENT_NODE && 
            child.getAttribute('__key')) {
            children.push(sizeQuery(child));
        }
    }
    var position = elem.getBoundingClientRect();
    var size = {
        'ty': 'panel',
        'key': elem.getAttribute('__key'),
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

var world = document.getElementById('app');
vdi.run(world, function(vdom) {
    world.setAttribute('__key','__primordial');
    function init(arg) {
        window.parent.postMessage({ty: 'init'}, window.location.href);
        return { root: null };
    };
    function update(evt) {
        return function(state) {
            if (evt.ty === 'render') {
                return { root: evt };
            }
            return state;
        };
    };
    function view(state) {
        var root = state.root || {key: 'root', tag: 'div'};
        var elem = mapNode(vdom, true, root);
        var scrollTop = $(world).scrollTop();
        function getSubElement(element) {
            for (var i = 0; i < element.childNodes.length; i++) {
                if (element.childNodes[i].nodeType == Node.ELEMENT_NODE) {
                    return element.childNodes[i];
                }
            }
            return element;
        }
        setTimeout(function() {
            window.parent.postMessage({
                ty: 'measure', 
                scrollTop: scrollTop, 
                data: sizeQuery(getSubElement(world)),
            }, window.location.href);
        }, 0);
        return elem;
    };
    vdomAddEventListener('render', function(evt) {
        vdom.post(evt);
    });
    return { init: init, update: update, view: view };
}, null);
