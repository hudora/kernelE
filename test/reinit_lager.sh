#!/bin/sh
# brings the myPL to a well defined state
rm -Rf Mnesia.mypl_test@*
erl -pa ./ebin ./vendor/eunit/ebin -sname mypl_test -noinput \
    -s mypl_db run_me_once \
    -s mnesia load_textfile '"test/data/lager-20071019.mnesiadump"' \
    -s mnesia info \
    -s erlang halt
