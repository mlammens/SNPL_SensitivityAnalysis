#!/bin/bash
# A simple script to convert *.mp files created in a *nix environment to Windows format
my_list=`cat $1`
for f in ${my_list}; do
	#echo $f
	f_nix=`echo $f | sed 's/snpl/snpl_nix/'`
	#echo $f_nix
	echo "mv $f $f_nix"
	mv $f $f_nix
	echo "awk 'sub("$", "\r")' $f_nix > $f"
	awk 'sub("$", "\r")' $f_nix > $f
	rm $f_nix
done
