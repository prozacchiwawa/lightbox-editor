all: out out/index.js out/child.js out/storagetestprog.js

clean:
	rm -rf out js/index.js

out:
	mkdir $@

js/index.js: index.fsx *.fs
	fable --projFile $< --outDir js

out/index.js: js/index.js js/vdominterface.js
	browserify -e js/prog.js -o $@

out/child.js: js/child.js js/vdominterface.js
	browserify -e js/child.js -o $@

js/storagetest.js: storagetest.fsx *.fs
	fable --projFile $< --outDir js

out/storagetestprog.js: js/storagetestprog.js js/storagetest.js js/qinterface.js
	browserify -e js/storagetestprog.js -o $@
