#!/bin/bash
DIR=~/lab/steppers/algebraic_effects_with_extensions
TEST_DIR=$DIR/output_test
PROGRAM=$DIR/interpreter

success=0
failure=0

for classification in $TEST_DIR/*; do
    class_name=`basename $classification`
    for file in $classification/*.txt; do
	name=`basename ${file} .txt`
	path=$classification/$name
	if [ -e "$path.input" ]
	then input_file=$path.input
	else input_file=""
	fi
	result_file=$path.result
	cat $file | $PROGRAM -n $input_file >$result_file 2>&1
	if [ ! -e "$path.output" ]; then
	    echo "No $class_name/$name.output found"
	elif [ -z "`diff $result_file $path.output`" ]; then
	    let success++
	    echo "Passed: $class_name/$name"
	    rm $result_file
	else
	    let failure++
	    printf "\e[31mFailed: ${class_name}/${name}\n\e[m"
	    echo "----- The program ---------------------------------"
	    cat $file
	    echo "----- Expected result -----------------------------"
	    printf "\e[32m"
	    cat $path.output
	    printf "\e[m"
	    echo "----- Your output ---------------------------------"
	    printf "\e[31m"
	    cat $result_file
	    printf "\e[m"
	    echo "---------------------------------------------------"
	    echo ""
	fi
    done
done
printf "\e[32m${success} tests passed\n"
printf "\e[31m${failure} tests failed\n\e[m"
