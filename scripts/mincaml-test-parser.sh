#! /bin/sh
cd "$(dirname "$0")"/.. || exit 1

# TODO change this to point to your mincamlc executable if it's different, or add
# it to your PATH. Use the appropriate option to run the parser as soon
# as it is implemented
MINCAMLC=ocaml/mincamlc

# run all test cases in syntax/valid and make sure they are parsed without error
# run all test cases in syntax/invalid and make sure the parser returns an error

# TODO extends this script to run test in subdirectories
# 

echo "----------------------------"
echo "      Testing parser"
echo "----------------------------"
for test_case in tests/syntax/valid/*.ml
do
    echo "testing parser on: $test_case"
    if $MINCAMLC "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "obtain OK expected OK"
    else 
        echo "obtain KO expected OK"
    fi
done

for test_case in tests/syntax/invalid/*.ml
do
    echo "testing parser on: $test_case"
    if $MINCAMLC "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "obtain OK expeted KO"
    else 
        echo "obtain KO expeted KO"
    fi
done
echo "----------------------------------"
echo "      Testing typechecking"
echo "----------------------------------"
for test_case in tests/typechecking/valid/*.ml
do
    echo "testing parser on: $test_case"
    if $MINCAMLC "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "obtain OK expected OK"
    else 
        echo "obtain KO expected OK"
    fi
done


for test_case in tests/typechecking/invalid/*.ml
do
    echo "testing parser on: $test_case"
    if $MINCAMLC "$test_case" 2> /dev/null 1> /dev/null
    then
        echo "obtain OK expeted KO"
    else 
        echo "obtain KO expeted KO"
    fi
done
#TODO finish that