#!/bin/bash

cd ~/github/my-python-project

# 5a. Get number of python files
ls *.py | wc -l

# 5b. Get total lines of code and comments (excluding empty lines)
cloc

# 5c. Get total number of functions
total_functions=0;for file in *.py; do total_functions+=$((grep -E 'def (.*)' $file -c)); done; echo "$total_functions"

# 5d. Calculate lines of changes from current version to HEAD~3
git diff HEAD..HEAD~3

# 5e. Total folder size of each subfolder in MB (down to 2 level depth)
tree -L 2 -d -s -h M




