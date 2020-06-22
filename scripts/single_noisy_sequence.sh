#!/bin/bash

case $(expr $1 + 1) in
	1 )
		echo "Solving predict_1_1_2.lp"
		time code/solve noisy 1 1 2
		;;
	2 )
		echo "Solving predict_1_1_3.lp"
		time code/solve noisy 1 1 3
		;;
	3 )
		echo "Solving predict_1_2_2.lp"
		time code/solve noisy 1 2 2
		;;
	4 )
		echo "Solving predict_1_2_3.lp"
		time code/solve noisy 1 2 3
		;;
	5 )
		echo "Solving predict_1_3_2.lp"
		time code/solve noisy 1 3 2
		;;
	6 )
		echo "Solving predict_1_3_3.lp"
		time code/solve noisy 1 3 3
		;;
	7 )
		echo "Solving predict_2_2_2.lp"
		time code/solve noisy 2 2 2
		;;
	8 )
		echo "Solving predict_2_2_3.lp"
		time code/solve noisy 2 2 3
		;;
	9 )
		echo "Solving predict_2_3_2.lp"
		time code/solve noisy 2 3 2
		;;
	10 )
		echo "Solving predict_2_3_3.lp"
		time code/solve noisy 2 3 3
		;;
	11 )
		echo "Solving predict_2_4_2.lp"
		time code/solve noisy 2 4 2
		;;
	12 )
		echo "Solving predict_2_4_3.lp"
		time code/solve noisy 2 4 3
		;;
	13 )
		echo "Solving predict_3_3_2.lp"
		time code/solve noisy 3 3 2
		;;
	14 )
		echo "Solving predict_3_3_3.lp"
		time code/solve noisy 3 3 3
		;;
	15 )
		echo "Solving predict_3_4_2.lp"
		time code/solve noisy 3 4 2
		;;
	16 )
		echo "Solving predict_3_4_3.lp"
		time code/solve noisy 3 4 3
		;;
	17 )
		echo "Solving predict_3_5_2.lp"
		time code/solve noisy 3 5 2
		;;
	18 )
		echo "Solving predict_3_5_3.lp"
		time code/solve noisy 3 5 3
		;;
	19 )
		echo "Solving predict_4_1_2.lp"
		time code/solve noisy 4 1 2
		;;
	20 )
		echo "Solving predict_4_1_3.lp"
		time code/solve noisy 4 1 3
		;;
	21 )
		echo "Solving predict_4_2_2.lp"
		time code/solve noisy 4 2 2
		;;
	22 )
		echo "Solving predict_4_2_3.lp"
		time code/solve noisy 4 2 3
		;;
	23 )
		echo "Solving predict_4_3_2.lp"
		time code/solve noisy 4 3 2
		;;
	24 )
		echo "Solving predict_4_3_3.lp"
		time code/solve noisy 4 3 3
		;;
	25 )
		echo "Solving predict_5_2_2.lp"
		time code/solve noisy 5 2 2
		;;
	26 )
		echo "Solving predict_5_2_3.lp"
		time code/solve noisy 5 2 3
		;;
	27 )
		echo "Solving predict_5_3_2.lp"
		time code/solve noisy 5 3 2
		;;
	28 )
		echo "Solving predict_5_3_3.lp"
		time code/solve noisy 5 3 3
		;;
	29 )
		echo "Solving predict_5_4_2.lp"
		time code/solve noisy 5 4 2
		;;
	30 )
		echo "Solving predict_5_4_3.lp"
		time code/solve noisy 5 4 3
		;;
	31 )
		echo "Solving predict_6_3_2.lp"
		time code/solve noisy 6 3 2
		;;
	32 )
		echo "Solving predict_6_3_3.lp"
		time code/solve noisy 6 3 3
		;;
	33 )
		echo "Solving predict_6_4_2.lp"
		time code/solve noisy 6 4 2
		;;
	34 )
		echo "Solving predict_6_4_3.lp"
		time code/solve noisy 6 4 3
		;;
	35 )
		echo "Solving predict_6_5_2.lp"
		time code/solve noisy 6 5 2
		;;
	36 )
		echo "Solving predict_6_5_3.lp"
		time code/solve noisy 6 5 3
		;;
	37 )
		echo "Solving predict_7_1_2.lp"
		time code/solve noisy 7 1 2
		;;
	38 )
		echo "Solving predict_7_1_3.lp"
		time code/solve noisy 7 1 3
		;;
	39 )
		echo "Solving predict_7_2_2.lp"
		time code/solve noisy 7 2 2
		;;
	40 )
		echo "Solving predict_7_2_3.lp"
		time code/solve noisy 7 2 3
		;;
	41 )
		echo "Solving predict_7_3_2.lp"
		time code/solve noisy 7 3 2
		;;
	42 )
		echo "Solving predict_7_3_3.lp"
		time code/solve noisy 7 3 3
		;;
	43 )
		echo "Solving predict_8_2_2.lp"
		time code/solve noisy 8 2 2
		;;
	44 )
		echo "Solving predict_8_2_3.lp"
		time code/solve noisy 8 2 3
		;;
	45 )
		echo "Solving predict_8_3_2.lp"
		time code/solve noisy 8 3 2
		;;
	46 )
		echo "Solving predict_8_3_3.lp"
		time code/solve noisy 8 3 3
		;;
	47 )
		echo "Solving predict_8_4_2.lp"
		time code/solve noisy 8 4 2
		;;
	48 )
		echo "Solving predict_8_4_3.lp"
		time code/solve noisy 8 4 3
		;;
	49 )
		echo "Solving predict_9_3_2.lp"
		time code/solve noisy 9 3 2
		;;
	50 )
		echo "Solving predict_9_3_3.lp"
		time code/solve noisy 9 3 3
		;;
	51 )
		echo "Solving predict_9_4_2.lp"
		time code/solve noisy 9 4 2
		;;
	52 )
		echo "Solving predict_9_4_3.lp"
		time code/solve noisy 9 4 3
		;;
	53 )
		echo "Solving predict_9_5_2.lp"
		time code/solve noisy 9 5 2
		;;
	54 )
		echo "Solving predict_9_5_3.lp"
		time code/solve noisy 9 5 3
		;;
	55 )
		echo "Solving predict_10_1_2.lp"
		time code/solve noisy 10 1 2
		;;
	56 )
		echo "Solving predict_10_1_3.lp"
		time code/solve noisy 10 1 3
		;;
	57 )
		echo "Solving predict_10_2_2.lp"
		time code/solve noisy 10 2 2
		;;
	58 )
		echo "Solving predict_10_2_3.lp"
		time code/solve noisy 10 2 3
		;;
	59 )
		echo "Solving predict_10_3_2.lp"
		time code/solve noisy 10 3 2
		;;
	60 )
		echo "Solving predict_10_3_3.lp"
		time code/solve noisy 10 3 3
		;;
	61 )
		echo "Solving predict_11_2_2.lp"
		time code/solve noisy 11 2 2
		;;
	62 )
		echo "Solving predict_11_2_3.lp"
		time code/solve noisy 11 2 3
		;;
	63 )
		echo "Solving predict_11_3_2.lp"
		time code/solve noisy 11 3 2
		;;
	64 )
		echo "Solving predict_11_3_3.lp"
		time code/solve noisy 11 3 3
		;;
	65 )
		echo "Solving predict_11_4_2.lp"
		time code/solve noisy 11 4 2
		;;
	66 )
		echo "Solving predict_11_4_3.lp"
		time code/solve noisy 11 4 3
		;;
	67 )
		echo "Solving predict_12_3_2.lp"
		time code/solve noisy 12 3 2
		;;
	68 )
		echo "Solving predict_12_3_3.lp"
		time code/solve noisy 12 3 3
		;;
	69 )
		echo "Solving predict_12_4_2.lp"
		time code/solve noisy 12 4 2
		;;
	70 )
		echo "Solving predict_12_4_3.lp"
		time code/solve noisy 12 4 3
		;;
	71 )
		echo "Solving predict_12_5_2.lp"
		time code/solve noisy 12 5 2
		;;
	72 )
		echo "Solving predict_12_5_3.lp"
		time code/solve noisy 12 5 3
		;;
	73 )
		echo "Solving predict_13_1_2.lp"
		time code/solve noisy 13 1 2
		;;
	74 )
		echo "Solving predict_13_1_3.lp"
		time code/solve noisy 13 1 3
		;;
	75 )
		echo "Solving predict_13_2_2.lp"
		time code/solve noisy 13 2 2
		;;
	76 )
		echo "Solving predict_13_2_3.lp"
		time code/solve noisy 13 2 3
		;;
	77 )
		echo "Solving predict_13_3_2.lp"
		time code/solve noisy 13 3 2
		;;
	78 )
		echo "Solving predict_13_3_3.lp"
		time code/solve noisy 13 3 3
		;;
	79 )
		echo "Solving predict_14_2_2.lp"
		time code/solve noisy 14 2 2
		;;
	80 )
		echo "Solving predict_14_2_3.lp"
		time code/solve noisy 14 2 3
		;;
	81 )
		echo "Solving predict_14_3_2.lp"
		time code/solve noisy 14 3 2
		;;
	82 )
		echo "Solving predict_14_3_3.lp"
		time code/solve noisy 14 3 3
		;;
	83 )
		echo "Solving predict_14_4_2.lp"
		time code/solve noisy 14 4 2
		;;
	84 )
		echo "Solving predict_14_4_3.lp"
		time code/solve noisy 14 4 3
		;;
	85 )
		echo "Solving predict_15_3_2.lp"
		time code/solve noisy 15 3 2
		;;
	86 )
		echo "Solving predict_15_3_3.lp"
		time code/solve noisy 15 3 3
		;;
	87 )
		echo "Solving predict_15_4_2.lp"
		time code/solve noisy 15 4 2
		;;
	88 )
		echo "Solving predict_15_4_3.lp"
		time code/solve noisy 15 4 3
		;;
	89 )
		echo "Solving predict_15_5_2.lp"
		time code/solve noisy 15 5 2
		;;
	90 )
		echo "Solving predict_15_5_3.lp"
		time code/solve noisy 15 5 3
		;;
	91 )
		echo "Solving predict_16_1_2.lp"
		time code/solve noisy 16 1 2
		;;
	92 )
		echo "Solving predict_16_1_3.lp"
		time code/solve noisy 16 1 3
		;;
	93 )
		echo "Solving predict_16_1_4.lp"
		time code/solve noisy 16 1 4
		;;
	94 )
		echo "Solving predict_16_2_2.lp"
		time code/solve noisy 16 2 2
		;;
	95 )
		echo "Solving predict_16_2_3.lp"
		time code/solve noisy 16 2 3
		;;
	96 )
		echo "Solving predict_16_2_4.lp"
		time code/solve noisy 16 2 4
		;;
	97 )
		echo "Solving predict_16_3_2.lp"
		time code/solve noisy 16 3 2
		;;
	98 )
		echo "Solving predict_16_3_3.lp"
		time code/solve noisy 16 3 3
		;;
	99 )
		echo "Solving predict_16_3_4.lp"
		time code/solve noisy 16 3 4
		;;
	100 )
		echo "Solving predict_17_2_2.lp"
		time code/solve noisy 17 2 2
		;;
	101 )
		echo "Solving predict_17_2_3.lp"
		time code/solve noisy 17 2 3
		;;
	102 )
		echo "Solving predict_17_2_4.lp"
		time code/solve noisy 17 2 4
		;;
	103 )
		echo "Solving predict_17_3_2.lp"
		time code/solve noisy 17 3 2
		;;
	104 )
		echo "Solving predict_17_3_3.lp"
		time code/solve noisy 17 3 3
		;;
	105 )
		echo "Solving predict_17_3_4.lp"
		time code/solve noisy 17 3 4
		;;
	106 )
		echo "Solving predict_17_4_2.lp"
		time code/solve noisy 17 4 2
		;;
	107 )
		echo "Solving predict_17_4_3.lp"
		time code/solve noisy 17 4 3
		;;
	108 )
		echo "Solving predict_17_4_4.lp"
		time code/solve noisy 17 4 4
		;;
	109 )
		echo "Solving predict_18_3_2.lp"
		time code/solve noisy 18 3 2
		;;
	110 )
		echo "Solving predict_18_3_3.lp"
		time code/solve noisy 18 3 3
		;;
	111 )
		echo "Solving predict_18_3_4.lp"
		time code/solve noisy 18 3 4
		;;
	112 )
		echo "Solving predict_18_4_2.lp"
		time code/solve noisy 18 4 2
		;;
	113 )
		echo "Solving predict_18_4_3.lp"
		time code/solve noisy 18 4 3
		;;
	114 )
		echo "Solving predict_18_4_4.lp"
		time code/solve noisy 18 4 4
		;;
	115 )
		echo "Solving predict_18_5_2.lp"
		time code/solve noisy 18 5 2
		;;
	116 )
		echo "Solving predict_18_5_3.lp"
		time code/solve noisy 18 5 3
		;;
	117 )
		echo "Solving predict_18_5_4.lp"
		time code/solve noisy 18 5 4
		;;
esac
