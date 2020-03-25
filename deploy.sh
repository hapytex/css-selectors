#!/bin/bash

stack sdist
pth=$(stack path --dist-dir)

/opt/cabal/bin/cabal upload -u "$1" -p "$2" "$pth"/*.tar.gz

rm stack.yaml.lock
