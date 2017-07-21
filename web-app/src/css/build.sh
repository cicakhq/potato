#!/bin/bash
for f in *scss; do sassc -t compressed $f tmp/${f%.*}.css; done
