#! /bin/bash
snpl_list=`cat snpl_list.txt`
# echo $snpl_list
for mp in ${snpl_list}; do
	scl=`echo $mp | sed s/mp$/SCL/`
	echo $mp
	echo $scl
	if [ -e $scl ]; 
	then
		echo "Simulation Complete"
	else
		echo "Simulation NOT Complete"
		echo $mp >> unrun_list.txt
	fi
done
