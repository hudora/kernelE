#!/bin/sh
erl -pa ./ebin ./vendor/eunit/ebin -sname mypl_test -setcookie xTESTx \
    -s mnesia start -s mnesia load_textfile "\"test/data/lager-20071017.mnesiadump\"" -s tv start -s mypl start