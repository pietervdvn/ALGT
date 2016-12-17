#! /bin/bash
find | grep "\.hs$" | grep -v "dist" | xargs grep $1
