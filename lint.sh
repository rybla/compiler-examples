#!/usr/bin/env bash

# This is a simple script to analyze and refactor source code according to the
# project's HLint rules. This requires a loop in order to recursively apply
# HLint refactors. A final pass formats source code with Ormolu and does a final
# HLint analysis without refactoring (since all refactors must already have been
# applied in the previous iterations).

set -euo pipefail

i=0
while true; do
    echo "Refactoring iteration #$i"

    # Flag to track if any hlint command failed in the current iteration
    failed=0

    # Use find with a while-read loop to safely handle unusual filenames
    while IFS= read -r -d '' file; do
        # echo "Linting file: $file"
        stack exec -- hlint --ignore="todo" --refactor --refactor-options="--inplace" "$file" >&2
        exit_code=$?

        # Check the exit status of the hlint command
        if [ $exit_code -ne 0 ]; then
            failed=1
        fi
    done < <(find . -type f -name "*.hs" -print0)

    # If any hlint invocation failed, continue to the next iteration of the main loop
    if [ "$failed" -ne 0 ]; then
        i=i+1
        continue
    fi

    # If all files passed successfully, break out of the main loop
    break
done

# this ordering ensures that the exit code from linting is the exit code of this script
echo "Finished refactoring and linting"
stack exec -- ormolu --mode inplace $(find . -type f -name "*.hs")
stack exec -- hlint --color=never .
