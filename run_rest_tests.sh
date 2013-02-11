#!/bin/bash

./dist/build/hstodo/hstodo --no-access-log --error-log='-' -e test &
PID=$!

jasmine-node test/js/rest

kill -s SIGINT $PID
wait $PID
