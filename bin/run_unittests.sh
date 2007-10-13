#!/bin/sh
erl -pa ./ebin ./vendor/eunit/ebin -s mypl_db testrunner -s
