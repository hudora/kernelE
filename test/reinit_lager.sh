#!/bin/sh
# brings the myPL to a well defined state
# seems not to work ... to slow?
# rm -Rf Mnesia.mypl_test@*
erl -pa ./ebin ./vendor/eunit/ebin -sname mypl_test -noinput \
    -s mnesia clear_table movement \
    -s mnesia clear_table location \
    -s mnesia clear_table unit \
    -s mnesia clear_table pick \
    -s mnesia clear_table resevation \
    -s mnesia clear_table provpipeline \
    -s mnesia clear_table provpipeline_processing \
    -s mnesia clear_table pickpipeline \
    -s mnesia clear_table retrievalpipeline \
    -s mypl_db run_me_once \
    -eval "mnesia:load_textfile(\"test/data/platzstammdaten.mnesiadump\")" \
    -s mnesia stop \
    -s erlang halt

# mnesia:clear_table(movement),mnesia:clear_table(location),mnesia:clear_table(unit),mnesia:clear_table(pick),mnesia:clear_table(resevation),mnesia:clear_table(provpipeline),mnesia:clear_table(provpipeline_processing),mnesia:clear_table(pickpipeline),mnesia:clear_table(retrievalpipeline),mnesia:load_textfile("test/data/platzstammdaten.mnesiadump").


