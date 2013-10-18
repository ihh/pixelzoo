#!/bin/bash

# Generate clibs.c and all stubs
make clibs.c

# Configure include.h
sed -i "" -e "s/.*\(sexp_default_module_path\).*/extern char\* \1;/g" -e "s/macosx/ios/g" include/chibi/install.h

# Remove unportable libs
sed -i "" -e "1,5d" -e "50,53d" -e "93d" -e "105d" clibs.c
