#!/bin/bash

case $(expr $1 + 1) in
	1 )
		echo "Solving new_seq_1.lp..."
		time code/solve seq-mnist new_seq_1.lp
		;;
	2 )
		echo "Solving new_seq_2.lp..."
		time code/solve seq-mnist new_seq_2.lp
		;;
	3 )
		echo "Solving new_seq_3.lp..."
		time code/solve seq-mnist new_seq_3.lp
		;;
	4 )
		echo "Solving new_seq_4.lp..."
		time code/solve seq-mnist new_seq_4.lp
		;;
	5 )
		echo "Solving new_seq_5.lp..."
		time code/solve seq-mnist new_seq_5.lp
		;;
	6 )
		echo "Solving new_seq_6.lp..."
		time code/solve seq-mnist new_seq_6.lp
		;;
	7 )
		echo "Solving new_seq_7.lp..."
		time code/solve seq-mnist new_seq_7.lp
		;;
	8 )
		echo "Solving new_seq_8.lp..."
		time code/solve seq-mnist new_seq_8.lp
		;;
	9 )
		echo "Solving new_seq_9.lp..."
		time code/solve seq-mnist new_seq_9.lp
		;;
	10 )
		echo "Solving new_seq_10.lp..."
		time code/solve seq-mnist new_seq_10.lp
		;;
esac
