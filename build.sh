#!/bin/sh
set -xe
mkdir -p build
fpc -FEbuild -vewh src/turp.pas
