#!/bin/sh
cd `dirname $0`
mkdir -p $PWD/db_mnesia
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -sname mypl_produktion@airvent -setcookie voom3OhlXeeg8vuD \
         -mnesia dir '"./db_mnesia"' -boot start_sasl -s reloader -s mypl
