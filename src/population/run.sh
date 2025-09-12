#!/bin/bash

VER=0.1.8

echo "Installing the population package v${VER}..."
cd /code/src/population

wget --quiet https://raw.githubusercontent.com/kerguler/Population/main/dist/population-${VER}.tar.gz

tar -xvzf population-${VER}.tar.gz
cd population-${VER}
./configure
make
make install
ldconfig -v

cd ../
make
echo `./test`