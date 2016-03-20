#!/bin/bash

ab -e refactor.csv -n 150 'http://localhost:8000/jsonp/package/modify?callback=jQuery1101010331580438651145_1458501210532&name=drracket-cyberpunk&description=&source=git%3A%2F%2Fgithub.com%2Fthinkmoore%2Fdrracket-cyberpunk.git&pkg=drracket-cyberpunk&email=scott%40thinkmoore.net&passwd=test&_=1458501210536'
