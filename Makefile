# based on a Makefile by  Ludovic Coquelle <lcoquelle@gmail.com>
# snatched from adviserl

CMD_ERL      = erl
CMD_ERLC     = erlc -Ivendor -Iinclude
CMD_DIALYZER = dialyzer
APPNAME      = myPL

ERLPATH=-pa ebin -pa vendor/eunit/ebin

DOCOPT={todo, true}, {private, true}

APP_MOD    = $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))

# Main targets

.PHONY : prodbuild
prodbuild : BUILDOPT = 
prodbuild : vendor_build ${APP_MOD}

.PHONY : doc
doc:
	${CMD_ERL} \
        -noshell \
        ${ERLPATH} \
        -eval 'edoc:application(${APPNAME}, ".", [${DOCOPT}]).' \
        -s init stop

.PHONY : doc_upload
doc_upload: doc
	@rsync -rz doc b.23.nu:/var/www/static/md/Files/myPL

.PHONY : logs
logs:
	${CMD_ERL} \
        -noshell \
        ${ERLPATH} \
        -boot start_sasl \
        -eval "(rb:start([{report_dir, \"./log/\"}]) =:= ok) andalso rb:show()." \
        -s init stop

.PHONY : clean
clean :
	@rm -vf erl_crash.dump
	@rm -vf $(APP_MOD)
	@rm -vf $(VENDOR_MOD)
	@rm -vf log/*
	@touch doc/remove.me # ensure next command does not failed
	@find doc -not -path '*.svn*' -and -not -name overview.edoc -and -not -path 'doc/figure*' -delete


# Development targets

.PHONY : dev
dev: devbuild regression checks doc

.PHONY : devbuild
devbuild: BUILDOPT = +debug_info -DEUNIT -DLOG_DEBUG -Wall
devbuild: vendor_build ${APP_MOD} test_build

.PHONY : test
test: regression

.PHONY : regression
regression: devbuild
	@erl \
		-noshell \
		-kernel error_logger      "{file, \"log/utest-kernel.log\"}" \
		-sasl   sasl_error_logger "{file, \"log/utest-sasl.log\"}" \
		${ERLPATH} \
		-eval "filelib:fold_files(\
			\"ebin\",\
			\".*\.beam\",\
			true,\
			fun(F, Acc) -> \
				M = list_to_atom(filename:basename(F, \".beam\")),\
				io:format(\"Testing ~p~n\", [M]),\
				TR = eunit:test(M, [{verbose, true}]),\
				Acc\
			end,\
			[]\
		)." \
		-s init stop


.PHONY : checks
checks: xref dialyzer

.PHONY : xref
xref:
	${CMD_ERL} -noshell ${ERLPATH} -eval 'io:format("~p~n", [xref:d("ebin")]).' -s init stop

.PHONY : dialyzer
dialyzer:
	${CMD_DIALYZER} ${ERLPATH} -I include --src -c src


# Sub-targets

.PHONY : vendor_build
vendor_build:
	@sh -c "(cd vendor/eunit; make subdirs)"

.PHONY : vendor_clean
vendor_clean:
	@sh -c "(cd vendor/eunit; make clean)"

.PHONY : test_build
test_build: BUILDOPT = +debug_info -DEUNIT -DLOG_DEBUG -Wall


# Compiling
ebin/%.beam : src/%.erl
	${CMD_ERLC} ${BUILDOPT} -o ebin $<

