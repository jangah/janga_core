#!/bin/sh
mkdir -p log
erl -sname jangah -setcookie nocookie -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s janga_core -s observer -config ../janga/etc/dev.config

