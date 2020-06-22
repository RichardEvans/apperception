#!/bin/bash

case $(expr $1 + 1) in
	1 )
		echo "Solving house example H1..."
		time code/solve house H1
		;;
	2 )
		echo "Solving house example H2..."
		time code/solve house H2
		;;
	3 )
		echo "Solving house example H3..."
		time code/solve house H3
		;;
	4 )
		echo "Solving house example H4..."
		time code/solve house H4
		;;
	5 )
		echo "Solving house example H5..."
		time code/solve house H5
		;;
	6 )
		echo "Solving house example H6..."
		time code/solve house H6
		;;
	7 )
		echo "Solving house example H7..."
		time code/solve house H7
		;;
	8 )
		echo "Solving house example H8..."
		time code/solve house H8
		;;
	9 )
		echo "Solving house example H9..."
		time code/solve house H9
		;;
	10 )
		echo "Solving house example H10..."
		time code/solve house H10
		;;
	11 )
		echo "Solving house example H11..."
		time code/solve house H11
		;;
	12 )
		echo "Solving house example H12..."
		time code/solve house H12
		;;
	13 )
		echo "Solving house example H13..."
		time code/solve house H13
		;;
	14 )
		echo "Solving house example H14..."
		time code/solve house H14
		;;
	15 )
		echo "Solving house example H15..."
		time code/solve house H15
		;;
	16 )
		echo "Solving house example H16..."
		time code/solve house H16
		;;
esac
