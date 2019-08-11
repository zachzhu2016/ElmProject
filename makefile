
all: src/*.elm src/Page/*.elm
	elm make src/Main.elm src/Page/*.elm --output=elm.js

live: src/*.elm src/Page/*.elm
	elm-live src/Main.elm src/Page/*.elm -u -- --output=elm.js --debug 

main: src/Main.elm
	elm make src/Main.elm --output=elm.js

support: src/*.elm
	elm make src/*.elm --output=elm.js

page: src/page/*.elm
	elm make src/page/*.elm --output=elm.js

clean: 
	rm elm.js
	rm -r elm-stuff


