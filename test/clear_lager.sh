#!/bin/sh
# empties myPL
rm -Rf Mnesia.mypl_test@*
erl -pa ./ebin ./vendor/eunit/ebin -sname mypl_test -noinput -s mypl_db run_me_once -s erlang halt
