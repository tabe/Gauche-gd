#!/bin/bash
HOME=/home/tabe
make maintainer-clean
cp -prf ${HOME}/src/Gauche-gd/* ./
autoreconf
#./configure CPPFLAGS='-I/usr/local/include -I/usr/include' LDFLAGS='-L/usr/local/lib -L/usr/lib'
./configure
make
