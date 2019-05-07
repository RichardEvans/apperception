#!/bin/bash

case $(expr $1 + 1) in
	1 )
		time code/solve occlusion w1
		;;
	2 )
		time code/solve occlusion w10
		;;
	3 )
		time code/solve occlusion w11
		;;
	4 )
		time code/solve occlusion w12
		;;
	5 )
		time code/solve occlusion w13
		;;
	6 )
		time code/solve occlusion w14
		;;
	7 )
		time code/solve occlusion w15
		;;
	8 )
		time code/solve occlusion w16
		;;
	9 )
		time code/solve occlusion w17
		;;
	10 )
		time code/solve occlusion w18
		;;
	11 )
		time code/solve occlusion w19
		;;
	12 )
		time code/solve occlusion w2
		;;
	13 )
		time code/solve occlusion w20
		;;
	14 )
		time code/solve occlusion w3
		;;
	15 )
		time code/solve occlusion w4
		;;
	16 )
		time code/solve occlusion w5
		;;
	17 )
		time code/solve occlusion w6
		;;
	18 )
		time code/solve occlusion w7
		;;
	19 )
		time code/solve occlusion w8
		;;
	20 )
		time code/solve occlusion w9
		;;
esac
