#!/bin/sh
# empties myPL
rm -Rf Mnesia.mypl_test@*
erl -pa ./ebin -sname mypl_test -noinput -s mypl_db run_me_once -s erlang halt
