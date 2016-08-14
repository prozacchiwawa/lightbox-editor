// Our virtualdom interface exposes run(element, component-desc);
var vdi = require('./vdominterface');
var shortid = require('shortid');
var test = require('./test');

window.generateId = shortid.generate;

vdi.run(document.getElementById('app'), test.default.main, location.href.toString());
