#!/bin/sh
erl -pa ./ebin ./vendor/eunit/ebin -sname mypl_test -setcookie xTESTx -s mypl start -s tv start -s mnesia load_textfile "\"test/data/lager-20071017.mnesiadump\""