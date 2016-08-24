var h = require('virtual-dom/h');
var diff = require('virtual-dom/diff');
var patch = require('virtual-dom/patch');
var createElement = require('virtual-dom/create-element');
var VText = require('virtual-dom/vnode/vtext');
var VNode = require('virtual-dom/vnode/vnode');
var Bacon = require('baconjs');

// The type of event handler hooks.  Despite the name, every active element in
// virtual dom that runs some user code is either a hook or a widget.  Since
// we're given a filled out vnode tree from the consumer, we use hooks to translate
// event handler specifications to idempotent event listener attachements.
var eventHookType = function(eventfun) { this.eventfun = eventfun; };

// Ensure that we have exactly one binding to the indicated event.
// Use an unlikely property name on the node to collect our hooks to ensure
// that they're bound only once.
eventHookType.prototype.hook = function(node, propertyName, previousValue) {
    var eventName = propertyName.substr(2);
    node.__eventHandlers = node.__eventHandlers || {};
    if (node.__eventHandlers[eventName]) {
        node.removeEventListener(eventName, node.__eventHandlers[eventName]);
    }
    var self = this;
    var eventHandler = node.__eventHandlers[eventName] = function(event) {
        self.eventfun(event);
    };
    node.addEventListener(eventName, eventHandler);
};
// Ensure that a hooked up event is unhooked.
eventHookType.prototype.unhook = function(node, propertyName) {
    var eventName = propertyName.substr(2);
    node.__eventHandlers = node.__eventHandlers || {};
    if (node.__eventHandlers[eventName]) {
        node.removeEventListener(eventName, node.__eventHandlers[eventName]);
    }
}

function Attribute(pname, value) {
    this.pname = pname;
    this.value = value;
}

Attribute.prototype.toString = function() {
    return [this.pname,this.value];
}

Attribute.prototype.hook = function (elem, propName, previous) {
    if (!previous || previous.value !== this.value) {
        if (this.value !== undefined) {
            elem.setAttribute(this.pname, this.value);
        } else {
            elem.removeAttribute(this.pname);
        }
    }
}

// Create a text VNode
var vtext = function(t) { return new VText(t); }

// Create a proper VNode.
//
// Arguments (curried)
//
// tag -- The element name
// props -- The html attributes
// on -- The event handlers
//
// A proper implementation will include directly set properties as well,
// but we don't need them yet.
var vnode = function(tag) {
    return function(props) {
        return function(on) {
            return function(children) {
                var propdata = {};
                // Lists in fable have .head and .tail members.
                // The nil list item has head and tail falsey.
                var namespace = null;
                while (props.head) {
                    if (props.head.name !== 'xmlns') {
                        propdata[props.head.name] = new Attribute(props.head.name, props.head.value);
                    } else {
                        namespace = props.head.value;
                    }
                    props = props.tail;
                }
                while (on.head) {
                    propdata["on" + on.head.name] = new eventHookType(on.head.response);
                    on = on.tail;
                }
                var childdata = [];
                while (children.head) {
                    childdata.push(children.head);
                    children = children.tail;
                }
                return new VNode(tag, propdata, childdata, null, namespace);
            }
        }
    }
}

// Post a message to a bacon bus, which is a lot like Signal in elm and
// purescript.
var post = function(stream) {
    return function(msg) {
        stream.push(msg);
    };
}

// Given an element and an elm-architecture-like main function, run the
// given component.  Main takes an argument like the VDom type in vdom.fs,
// and a passthrough argument to init.
function run(element, main, pass) {
    var rootElement = null;
    var oldTree = null;
    var msgStream = Bacon.Bus();
    var program = main({ vnode: vnode, vtext, vtext, post: post(msgStream) });
    // Bacon's scan method is used here like foldp on a signal, taking an init
    // state and calling update for each message.
    var stateStream = msgStream.scan(program.init(pass), function(state,msg) {
        return program.update(msg, state);
    });
    // domStream is a stream of VNode describing the UI.
    var domStream = stateStream.map(function (state) {
        return program.view(state);
    });
    // Each time an event comes down domStream, the UI rerenders.
    domStream.subscribe(function(msg) {
        var tree = msg.value();
        if (!rootElement) {
            rootElement = createElement(tree);
            element.appendChild(rootElement);
        } else {
            var patches = diff(oldTree, tree);
            rootElement = patch(rootElement, patches);
        }
        oldTree = tree;
    });
}
exports.run = run;
