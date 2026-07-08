#!/usr/bin/env bash

# This is a simple script to query Hoogle (through Stack) while ensuring to
# always access an updated database.

set -euo pipefail

QUERY=$1

echo "Querying hoogle database with \"$QUERY\" ..."

# Create a temporary file for capturing combined stdout and stderr
mkdir -p tmp
TMP_OUT=$(mktemp --tmpdir=tmp)
trap 'rm -f "$TMP_OUT"' EXIT

mkdir -p tmp

# Execute the command and redirect both stdout and stderr to the temp file
stack hoogle --rebuild -- --count=100 $QUERY > "$TMP_OUT" 2>&1

echo "Hoogle results:"

# Use sed to delete all lines from the start of the file up to the line
# matching the regex ^Took, then print the remaining lines.
sed '1,/^Generated Hoogle database./d' "$TMP_OUT"
