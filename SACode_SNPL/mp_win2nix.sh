#!/bin/bash
# A simple script to convert windows txt files to nix format
my_list=`cat $1`
for f in ${my_list}; do
	#echo $f
	f_win=`echo $f | sed 's/Results/WinResults/'`
	echo $f_win
	echo "mv $f $f_win"
	mv $f $f_win
	echo "awk '{ sub("\r$", ""); print}' $f_win > $f"
	awk '{ sub("\r$", ""); print}' $f_win > $f
	#rm $f_win
done
