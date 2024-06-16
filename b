#!/bin/sh
acme -v cbm2-kernal.b
diff -s kernal.bin original/901244-4a.bin
cmp kernal.bin original/901244-4a.bin