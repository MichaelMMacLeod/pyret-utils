#!/bin/sh

grep 'Compile Time' - | sed -e 's/Compile Time <\(.*\)>: \(.*\)/("\1" \2)/'
