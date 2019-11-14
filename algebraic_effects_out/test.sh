#!/bin/bash
DIR=~/lab/steppers/algebraic_effects_free
TEST_DIR=$DIR/output_test
TMP_FILE=$DIR/tmp-testing.txt
PROGRAM=$DIR/interpreter

success=0
failure=0

for classification in $TEST_DIR/*; do
    class_name=`basename $classification`
    for file in $classification/*.input; do
	name=`basename ${file} .input`
	path=$classification/$name
	cat $file | $PROGRAM -n >$TMP_FILE 2>&1
	if [ ! -e "$path.output" ]; then
	    echo "No $class_name/$name.output found"
	elif [ -z "`diff $TMP_FILE $path.output`" ]; then
	    let success++
	    echo "Passed: $class_name/$name"
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
	    cat $TMP_FILE
	    printf "\e[m"
	    echo "---------------------------------------------------"
	    echo ""
	fi
    done
done
printf "\e[32m${success} tests passed\n"
printf "\e[31m${failure} tests failed\n\e[m"
rm $TMP_FILE
