#!/bin/bash
echo $1
newFile=`echo $1 | sed s/bat$/sh/`
echo $newFile
while read line; do
	echo $line | sed s/'CALL RunMP'/'wine START \/WAIT Metapop.exe'/ >> $newFile
done < $1
