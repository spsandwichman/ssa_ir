#!/bin/sh

rm -f ./ir

gcc main.c ir.c type.c -o ir