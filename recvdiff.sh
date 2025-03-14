#!/usr/bin/bash

for i in $(seq 1 $1); do
  echo "$i / $1";nc -l 7788 > diff.patch;git checkout -f;git apply --ignore-space-change --ignore-whitespace diff.patch;
done

echo "recv complete."
