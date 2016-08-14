// Our virtualdom interface exposes run(element, component-desc);
var vdi = require('./vdominterface');
var test = require('./test');

vdi.run(document.getElementById('app'), test.default.main, location.href.toString());
