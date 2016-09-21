JS_SOURCES=$(wildcard js/*interface.js) $(wildcard js/*.prog.js)

all: out/index.js out/child.js out/storagetestprog.js out/ziptestprog.js out/jsontestprog.js out/dragtestprog.js out/paneltestprog.js

clean:
	rm -rf out js/fs

out:
	mkdir -p $@

js/fs:
	mkdir -p $@

js/fs/storagetest.js: storagetest.fsx *.fs
	fable --projFile storagetest.fsx --outDir js/fs

js/fs/ziptest.js: ziptest.fsx *.fs
	fable --projFile ziptest.fsx --outDir js/fs

js/fs/jsontest.js: jsontest.fsx *.fs
	fable --projFile jsontest.fsx --outDir js/fs

js/fs/dragtest.js: dragtest.fsx *.fs
	fable --projFile dragtest.fsx --outDir js/fs

js/fs/paneltest.js: paneltest.fsx *.fs
	fable --projFile paneltest.fsx --outDir js/fs

js/fs/index.js: index.fsx *.fs
	fable --projFile $< --outDir js/fs

out/index.js: out js/fs/index.js js/vdominterface.js $(JS_SOURCES)
	browserify -e js/prog.js -o $@

out/child.js: out js/child.js js/vdominterface.js $(JS_SOURCES)
	browserify -e js/child.js -o $@

out/storagetestprog.js: out js/storagetestprog.js js/fs/storagetest.js js/qinterface.js $(JS_SOURCES)
	browserify -e js/storagetestprog.js -o $@

out/ziptestprog.js: out js/ziptestprog.js js/fs/ziptest.js js/qinterface.js $(JS_SOURCES)
	browserify -e js/ziptestprog.js -o $@

out/jsontestprog.js: out js/jsontestprog.js js/fs/jsontest.js $(JS_SOURCES)
	browserify -e js/jsontestprog.js -o $@

out/dragtestprog.js: out js/dragtestprog.js js/fs/dragtest.js $(JS_SOURCES)
	browserify -e js/dragtestprog.js -o $@

out/paneltestprog.js: out js/paneltestprog.js js/fs/paneltest.js $(JS_SOURCES)
	browserify -e js/paneltestprog.js -o $@
