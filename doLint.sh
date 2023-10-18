#!/bin/bash
getFiles() {
    hlint . | grep '^\.' | cut -d':' -f1 | uniq
}

refactor() {
    hlint --refactor --refactor-options="-is" $@
}

for f in $(getFiles); do refactor $f; done
