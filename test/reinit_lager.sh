#!/bin/sh
# brings the myPL to a well defined state
# rm -Rf Mnesia.mypl_test@*
erl -pa ./ebin ./vendor/eunit/ebin -sname mypl_test -noinput \
    -s mnesia clear_table movement \
    -s mnesia clear_table location \
    -s mnesia clear_table unit \
    -s mnesia clear_table pick \
    -s mnesia clear_table resevation \
    -s mypl_db run_me_once \
    -eval "mnesia:load_textfile(\"test/data/platzstammdaten.mnesiadump\")" \
    -s mnesia clear_table movement \
    -s mnesia clear_table location \
    -s mnesia clear_table unit \
    -s mnesia clear_table pick \
    -s mnesia clear_table resevation \
    -s mnesia stop \
    -s erlang halt
