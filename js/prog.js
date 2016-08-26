// Our virtualdom interface exposes run(element, component-desc);
var vdi = require('./vdominterface');
var shortid = require('shortid');
var index = require('./index');

window.generateId = shortid.generate;

vdi.run(document.getElementById('app'), index.default.main, location.href.toString());