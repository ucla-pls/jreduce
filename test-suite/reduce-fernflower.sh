#!/usr/bin/env bash

mainfolder="$PWD"
property="$mainfolder/properties/fernflower"

for arg
do
    jarname=$(basename $arg)
    output="$mainfolder/test-suite/fernflower/${jarname%.*}"

    if [ -e $output ]; 
    then
        echo "Already did $arg"
        echo "Run: rm -r $output" 
        continue
    fi

    mkdir -p "$output/classes"

    (cd $output/classes; unzip "$mainfolder/$arg")

    $property $output/classes

    if [[ "$?" != "0" ]]
    then
        echo "Property did not hold for $arg" 
        continue
    fi

    cd $output
    for r in "ddmin" "ddmin:graph" "gbired" "ddmin:verify" 
    do
        x="jreduce -v --cp classes -r $r --work-dir $r $property"
        echo "$x"
        $x 2>&1 | tee "$r.log"
    done

    find . -maxdepth 1 -mindepth 1 -type d -name '*-out' | while read dir; do
      printf "%-25.25s : " "$dir"
        find "$dir" -type f -name '*.class' | wc -l
    done

    wc -l *.csv
done
