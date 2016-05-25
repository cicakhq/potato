#!/usr/bin/env bash -x

exec vagrant ssh -- -N -o ServerAliveInterval=240 -L15672:localhost:15672 -L4005:localhost:4005 -L4006:localhost:4006 -L4007:localhost:4007 -L4009:localhost:4009 -L5984:localhost:5984 -L8983:localhost:8983 -L*:8080:localhost:8180
