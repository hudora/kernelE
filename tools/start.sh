#!/bin/sh
erl -pa ./ebin ./vendor/erlang-psql-driver/ebin -sname mypl_produktion@airvent -setcookie voom3OhlXeeg8vuD \
    -s mypl start
