all: out out/vdomtest.js

clean:
	rm -rf out js/test.js

out:
	mkdir $@

js/test.js: test.fsx
	fable --projFile $< --outDir js

out/vdomtest.js: js/test.js js/vdominterface.js
	browserify -e js/testprog.js -o $@

