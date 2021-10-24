#!/bin/sh
for f in testcases/*; do
	echo "Main.translate \"$f\";" | sml chap7/sources.cm | grep '^testcases'
done
