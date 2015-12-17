#!/bin/sh

fontforge -script emoji.pe Symbola.ttf symb.ttf
ttf2eot symb.ttf  > symb.eot
ttf2woff symb.ttf symb.woff