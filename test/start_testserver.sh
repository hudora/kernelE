#!/bin/sh
erl -pa ./ebin -sname mypl_test -setcookie xTESTx \
    -s mnesia start -s mnesia load_textfile "\"test/data/platzstammdaten.mnesiadump\"" -s tv start -s mypl start