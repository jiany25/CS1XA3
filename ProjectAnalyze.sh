#!/bin/bash

repoCheck(){
  command git pull | xargs echo
}

echo "Message1-->The status of your locl repository is :" 
repoCheck

function uncommitedChanges(){
  git status > changes.log
}

echo "Message2-->Check the changes.log file for uncommited changes." 
uncommitedChanges

findTodo(){
  grep -r -h "#TODO" . | xargs echo > todo.log 
}

echo "Message3-->Check the todo.log file for each line form every line of the project with the tage #TODO."
findTodo

checkError(){
  find . -name "*.hs" -type f | xargs ghc -fno-code > error.log
 
}
echo "Message4-->Check the error.log file for syntax errors of hskell files"
checkError

myFunction(){
  find . -iname "*.log" | xargs wc -l > lines.txt
}

echo "Message5-->Check lines.txt file for lines of each .log file."
myFunction
