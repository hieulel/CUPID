#!bin/sh
if [$# -ne 2 ]
then
	echo Usage: cuprd cupidout cuprdout
else
	if [-f $ 1]
		then
			cuprd3 <<!
$1
$2
0
0
2,1,225
0
1
4
4 1 1
5 1 4
5 1 5
5 1 9
48
1
0 2 2 2
3 12
0 0 0 0
1 1
0
-1
!
	else
		echo "cuprd3 input file (cupid out file) does not exist -  cant find " $1
	fi
fi

