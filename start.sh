#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -sname mypl_produktion@airvent -setcookie voom3OhlXeeg8vuD \
         -boot start_sasl -s mypl
