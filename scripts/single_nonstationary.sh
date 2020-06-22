#!/bin/bash

case $(expr $1 + 1) in
	1 )
		time code/solve nonstationary predict_1.lp
		;;
	2 )
		time code/solve nonstationary predict_2.lp
		;;
	3 )
		time code/solve nonstationary predict_3.lp
		;;
	4 )
		time code/solve nonstationary predict_4.lp
		;;
	5 )
		time code/solve nonstationary predict_5.lp
		;;
	6 )
		time code/solve nonstationary predict_6.lp
		;;
	7 )
		time code/solve nonstationary predict_7.lp
		;;
	8 )
		time code/solve nonstationary predict_8.lp
		;;
esac
