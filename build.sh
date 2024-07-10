#!/bin/sh
set -xe
mkdir -p build
fpc -FEbuild -vewh turp.pas
