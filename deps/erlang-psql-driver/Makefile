## Makefile for building the skel application
## Author: Jon Doe <jondoe@erlang-consulting.com>
## Usage: make Will build the source code
##        make docs Will build the docs
##        make app Will build the application resource file

APPLICATION=psql
ERL=erl

all:
	(cd src && $(ERL) -make)
test:
	(cd tests && $(ERL) -DTEST -make)
docs:
	$(ERL) -noshell -pa `pwd`/ebin -run edoc_run application \
		"'$(APPLICATION)'" '"."' '[]' -s erlang halt

app: src/$(APPLICATION).app.src
	(cd src && sed "s|Modules|`ls -x -m *.erl | sed 's|.erl||g' | tr \\\n ' '`|g" `basename $<` > ../ebin/`basename $< .src`)
clean:
	rm -fv ebin/*.beam
	rm -fv ebin/*.app
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css

cleanapp:
	rm -fv ebin/*.app
