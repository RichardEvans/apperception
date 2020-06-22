#!/bin/bash

case $(expr $1 + 1) in
	1 )
		time code/solve misc predict_1.lp
		;;
	2 )
		time code/solve misc predict_2.lp
		;;
	3 )
		time code/solve misc exog_1.lp
		;;
	4 )
		time code/solve eca-general predict_eca_245_b3.lp
		;;
	5 )
		time code/solve eca predict_110_b5_t14.lp
		;;
	6 )
		time code/solve eca retrodict_110_b5_t14.lp
		;;
	7 )
		time code/solve eca impute_110_b5_t14.lp
		;;
	8 )
		time code/solve sw predict_3.lp
		;;
	9 )
		time code/solve music predict_Scale.lp
		;;
	10 )
		time code/solve rhythm predict_Mazurka.lp
		;;
	11 )
		time code/solve binding predict_r2_b5.lp
		;;
	12 )
		time code/solve occlusion w2
		;;
	13 )
		time code/solve sokoban e7
		;;
	14 )
		time code/solve noisy 1 3 2
		;;
	15 )
		time code/solve sok-pixels e0
		;;
	16 )
		time code/solve seq-mnist seq_7_5.lp
		;;		
esac
