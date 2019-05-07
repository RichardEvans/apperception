#!/bin/bash

case $(expr $1 + 1) in
	1 )
		time code/solve misc predict_1.lp
		;;
	2 )
		time code/solve misc predict_2.lp
		;;
	3 )
		time code/solve misc predict_3.lp
		;;
	4 )
		time code/solve misc predict_4.lp
		;;
esac
