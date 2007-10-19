#!/bin/sh
erl -pa ./ebin ./vendor/eunit/ebin -sname mypl_test -setcookie xTESTx -s mypl start
