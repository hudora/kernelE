include ../support/include.mk

all: $(EBIN_FILES)

typer:
	typer $(INCLUDE_DIRS:../%=-I ../%) --show-exported *.erl -T mypl_db.erl

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)
