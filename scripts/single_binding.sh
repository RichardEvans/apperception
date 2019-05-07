#!/bin/bash

case $(expr $1 + 1) in
	1 )
		time code/solve binding predict_r110_b5.lp
		;;
	2 )
		time code/solve binding predict_r11_b5.lp
		;;
	3 )
		time code/solve binding predict_r133_b5.lp
		;;
	4 )
		time code/solve binding predict_r135_b5.lp
		;;
	5 )
		time code/solve binding predict_r139_b5.lp
		;;
	6 )
		time code/solve binding predict_r13_b5.lp
		;;
	7 )
		time code/solve binding predict_r147_b6.lp
		;;
	8 )
		time code/solve binding predict_r148_b13.lp
		;;
	9 )
		time code/solve binding predict_r155_b8.lp
		;;
	10 )
		time code/solve binding predict_r158_b14.lp
		;;
	11 )
		time code/solve binding predict_r167_b11.lp
		;;
	12 )
		time code/solve binding predict_r176_b13.lp
		;;
	13 )
		time code/solve binding predict_r193_b9.lp
		;;
	14 )
		time code/solve binding predict_r25_b5.lp
		;;
	15 )
		time code/solve binding predict_r26_b5.lp
		;;
	16 )
		time code/solve binding predict_r2_b5.lp
		;;
	17 )
		time code/solve binding predict_r30_b5.lp
		;;
	18 )
		time code/solve binding predict_r61_b5.lp
		;;
	19 )
		time code/solve binding predict_r67_b5.lp
		;;
	20 )
		time code/solve binding predict_r90_b5.lp
		;;
esac
