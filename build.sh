#!/bin/sh

rm -f ./ir

gcc main.c ir.c type.c arena.c -o ir -std=c99