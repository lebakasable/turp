#!/bin/sh
set -xe
mkdir -p build
fpc -FEbuild -vewhb src/turp.pas
