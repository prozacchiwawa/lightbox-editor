all: out out/index.js

clean:
	rm -rf out js/index.js

out:
	mkdir $@

js/index.js: index.fsx *.fs
	fable --projFile $< --outDir js

out/index.js: js/index.js js/vdominterface.js
	browserify -e js/prog.js -o $@

