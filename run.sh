#!/bin/bash

set -o xtrace

RACKET=/Users/sdmoore/Documents/racket/racket/bin/

while true ; do
    ${RACKET}/raco make dynamic.rkt
    ${RACKET}/racket -l errortrace -t dynamic.rkt
    sleep 1
done
