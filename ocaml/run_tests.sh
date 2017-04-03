#!/bin/sh

cd src
ocamlfind ocamlc -o ExecuteProblemsTest -package oUnit,yojson,batteries -linkpkg -g problems.ml problems_test.ml

./ExecuteProblemsTest

if [ "$1" = "-Y" ] || [ "$1" = "-y" ] ; then
   rm ExecuteProblemsTest;
   rm problems.cmi;
   rm problems.cmo;
   rm problems_test.cmi;
   rm problems_test.cmo;
else
    while true; do
        echo ""
        read -p "Do you wish to delete the test files? (Y or N) " yn
        case $yn in
            [Yy]* )
            rm ExecuteProblemsTest;
            rm problems.cmi;
            rm problems.cmo;
            rm problems_test.cmi;
            rm problems_test.cmo;
            break;;
            [Nn]* ) exit;;
            * ) echo "Please answer yes or no.";;
        esac
    done
fi