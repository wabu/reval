#!/bin/bash -xe

cd $(dirname $0)
echo "created by $0 - $(date)" > todo.tmp.txt
echo "" >> todo.tmp.txt
egrep -n '(TODO|FIXME)' src/*.lhs src/*.hs >> todo.tmp.txt
