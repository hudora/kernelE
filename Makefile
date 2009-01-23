include ./support/include.mk

#DOCOPT={todo, true}, {private, true}
DOCOPT={todo, true}

all:
	python tools/genkernelinterface.py
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)

.PHONY: doc
doc:
	$(ERL) -noshell -eval 'edoc:application(mypl, ".", [${DOCOPT}]).' -run init stop

.PHONY: check
check:
	dialyzer $(INCLUDE_DIRS:../%=-I ../%) $(EBIN_DIRS:%=-pa %) --src -c src

# Upload documentation
.PHONY: upload
upload: doc
	rsync -r --delete doc root@cybernetics.hudora.biz:/usr/local/www/apache22/data/dist/myPL/
