# based on a Makefile by  Ludovic Coquelle <lcoquelle@gmail.com>
# snatched from adviserl

CMD_ERL      = erl
CMD_ERLC     = erlc -Ivendor -Iinclude
CMD_DIALYZER = dialyzer
APPNAME      = myPL

ERLPATH=-pa ebin -pa vendor/eunit/ebin

DOCOPT={todo, true}, {private, true}

APP_MOD    = $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))
VENDOR_MOD = $(patsubst vendor/eunit/src/%.erl,vendor/eunit/ebin/%.beam,$(wildcard vendor/eunit/src/*.erl))
TEST_MOD   = $(patsubst test/src/%.erl,test/ebin/%.beam,$(wildcard test/src/*.erl))

default: devbuild

# Main targets

.PHONY : prodbuild
prodbuild : BUILDOPT = -smp
prodbuild : vendor_build ${APP_MOD}

.PHONY : doc
doc:
	${CMD_ERL} \
        -noshell \
        ${ERLPATH} \
        -eval 'edoc:application(${APPNAME}, ".", [${DOCOPT}]).' \
        -s init stop
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
	@rm -vf $(TEST_MOD)
	@rm -vf log/*
	@touch doc/remove.me # ensure next command does not failed
	@ls -1 doc | grep -vE "(^figure|\.(edoc|txt))$" | sed -e 's#\(.*\)#doc/\1#' | xargs rm -v


# Development targets

.PHONY : dev
dev: devbuild regression checks doc

.PHONY : devbuild
devbuild: BUILDOPT = +debug_info -DEUNIT -DLOG_DEBUG -Wall
devbuild: vendor_build ${APP_MOD} test_build

.PHONY : test
test: test_basic test_load

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

.PHONY : regression
regression_shell: devbuild
	@erl \
		${ERLPATH} \
		-eval "filelib:fold_files(\
			\"ebin\",\
			\".*\.beam\",\
			true,\
			fun(F, Acc) -> \
				M = list_to_atom(filename:basename(F, \".beam\")),\
				io:format(\"Testing ~p~n\", [M]),\
				TR = eunit:test(M, [{verbose, false}]),\
				Acc\
			end,\
			[]\
		)."

.PHONY : checks
checks: xref dialyzer cover

.PHONY : xref
xref:
	${CMD_ERL} -noshell ${ERLPATH} -eval 'io:format("~p~n", [xref:d("ebin")]).' -s init stop

.PHONY : dialyzer
dialyzer:
	${CMD_DIALYZER} ${ERLPATH} -I include --src -c src

.PHONY : cover
cover:
	@echo "... cover not used!"

.PHONY : perfs
perfs: fprof

.PHONY : fprof
fprof:
	@echo "... fprof not used!"



# Sub-targets

.PHONY : vendor_build
vendor_build : BUILDOPT = -DNOTEST
vendor_build: ${VENDOR_MOD}

.PHONY : test_build
test_build: BUILDOPT = +debug_info -DEUNIT -DLOG_DEBUG -Wall
test_build: ${TEST_MOD}


# Compiling

ebin/%.beam : src/%.erl
	${CMD_ERLC} ${BUILDOPT} -o ebin $<

test/ebin/%.beam : test/src/%.erl
	${CMD_ERLC} ${BUILDOPT} -o test/ebin $<

vendor/eunit/ebin/%.beam : vendor/eunit/src/%.erl
	${CMD_ERLC} ${BUILDOPT} -o vendor/eunit/ebin $<


# Testing

define run_test
	@echo
	@echo "**** Running test: $(strip $(1)) ****"
	@echo
	@${CMD_ERL} \
        -noshell \
        -config ebin/env_test \
        -pa ebin \
        -pa test/ebin/ \
        -kernel error_logger "{file, \"log/$(strip $(1))-kernel.log\"}" \
        -sasl sasl_error_logger "{file, \"log/$(strip $(1))-sasl.log\"}" \
        -s $(strip $(1)) start \
        -s init stop
	@echo
endef

.PHONY : test
test_basic: devbuild test_build
	${call run_test, basic}

.PHONY : test
test_load: devbuild test_build
	${call run_test, load}
