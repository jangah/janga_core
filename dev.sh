#!/bin/sh
mkdir -p log
erl -sname jangah -setcookie nocookie \
    -pa $PWD/_build/default/lib/*/ebin \
    -pz $PWD/japps/*/deps/*/ebin $PWD/japps/*/ebin \
    -boot start_sasl \
    -s janga_core \
    -s observer \
    -config ../janga/etc/dev.config
