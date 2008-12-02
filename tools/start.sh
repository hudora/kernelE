#!/bin/sh
erl -pa ./ebin ./vendor/eunit/ebin ./vendor/erlang-psql-driver/eunit/ebin -sname mypl_produktion@airvent -setcookie voom3OhlXeeg8vuD \
    -s mypl start
