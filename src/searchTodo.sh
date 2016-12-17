#! /bin/bash
# we drop the first two lines, as these are the todo in the stdDef
find | grep "\.hs$" | grep -v "dist" | xargs grep "TODO" | grep -v "StdDef.hs"
echo -e "\nNumber of todos: "
find | grep "\.hs$" | grep -v "dist" | xargs grep "TODO" | grep -v "StdDef.hs"| wc -l

find | grep "\.hs$" | grep -v "dist" | xargs grep "PICKUP"
