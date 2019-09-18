#!/bin/bash
DIR=/Users/tkn/lab/steppers/algebraic_effects
TEST_DIR=$DIR/output_test
TMP_FILE=$DIR/tmp-testing.txt
PROGRAM=$DIR/interpreter

success=0
failure=0

for classification in $TEST_DIR/*; do
    for file in $classification/*.txt; do
	cat $file | $PROGRAM -n >$TMP_FILE 2>&1
	if [ ! -e "$file.output" ]; then
	    echo "No $file.output found"
	elif [ -z "`diff $TMP_FILE $file.output`" ]; then
	    let success++
	    echo "Passed: $file"
	else
	    let failure++
	    echo "Failed: $FILE"
	    echo "----- The program ---------------------------------"
	    cat $file
	    echo "----- Expected result -----------------------------"
	    cat $file.output
	    echo "----- Your output ---------------------------------"
	    cat $TMP_FILE
	    echo "---------------------------------------------------"
	    echo ""
	fi
    done
done
echo "$success tests passed"
echo "$failure tests failed"
rm $TMP_FILE
