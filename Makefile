JS_SOURCES=$(wildcard js/*interface.js) $(wildcard js/*.prog.js)

all: out out/index.js out/child.js out/storagetestprog.js out/ziptestprog.js out/jsontestprog.js

clean:
	rm -rf out js/index.js

out:
	mkdir $@

js/index.js: index.fsx *.fs
	fable --projFile $< --outDir js

out/index.js: js/index.js js/vdominterface.js $(JS_SOURCES)
	browserify -e js/prog.js -o $@

out/child.js: js/child.js js/vdominterface.js $(JS_SOURCES)
	browserify -e js/child.js -o $@

js/storagetest.js: storagetest.fsx *.fs
	fable --projFile $< --outDir js

js/ziptest.js: ziptest.fsx *.fs
	fable --projFile $< --outDir js

js/jsontest.js: jsontest.fsx *.fs
	fable --projFile $< --outDir js

out/storagetestprog.js: js/storagetestprog.js js/storagetest.js js/qinterface.js $(JS_SOURCES)
	browserify -e js/storagetestprog.js -o $@

out/ziptestprog.js: js/ziptestprog.js js/ziptest.js js/qinterface.js $(JS_SOURCES)
	browserify -e js/ziptestprog.js -o $@

out/jsontestprog.js: js/jsontestprog.js js/jsontest.js $(JS_SOURCES)
	browserify -e js/jsontestprog.js -o $@

