#!/bin/bash

case $(expr $1 + 1) in
	1 )
		time code/solve music impute_Arpeggio.lp
		;;
	2 )
		time code/solve music impute_HumptyDumptySmall.lp
		;;
	3 )
		time code/solve music impute_IncyWincySpiderSmall.lp
		;;
	4 )
		time code/solve music impute_LittleLambSmall.lp
		;;
	5 )
		time code/solve music impute_Minimal.lp
		;;
	6 )
		time code/solve music impute_OldMacdonaldSmall.lp
		;;
	7 )
		time code/solve music impute_RowYourBoatSmall.lp
		;;
	8 )
		time code/solve music impute_Scale.lp
		;;
	9 )
		time code/solve music impute_SmallTwinkleTwinkle.lp
		;;
	10 )
		time code/solve music impute_ThreeBlindMiceSmall.lp
		;;
	11 )
		time code/solve music predict_Arpeggio.lp
		;;
	12 )
		time code/solve music predict_HumptyDumptySmall.lp
		;;
	13 )
		time code/solve music predict_IncyWincySpiderSmall.lp
		;;
	14 )
		time code/solve music predict_LittleLambSmall.lp
		;;
	15 )
		time code/solve music predict_Minimal.lp
		;;
	16 )
		time code/solve music predict_OldMacdonaldSmall.lp
		;;
	17 )
		time code/solve music predict_RowYourBoatSmall.lp
		;;
	18 )
		time code/solve music predict_Scale.lp
		;;
	19 )
		time code/solve music predict_SmallTwinkleTwinkle.lp
		;;
	20 )
		time code/solve music predict_ThreeBlindMiceSmall.lp
		;;
	21 )
		time code/solve music retrodict_Arpeggio.lp
		;;
	22 )
		time code/solve music retrodict_HumptyDumptySmall.lp
		;;
	23 )
		time code/solve music retrodict_IncyWincySpiderSmall.lp
		;;
	24 )
		time code/solve music retrodict_LittleLambSmall.lp
		;;
	25 )
		time code/solve music retrodict_Minimal.lp
		;;
	26 )
		time code/solve music retrodict_OldMacdonaldSmall.lp
		;;
	27 )
		time code/solve music retrodict_RowYourBoatSmall.lp
		;;
	28 )
		time code/solve music retrodict_Scale.lp
		;;
	29 )
		time code/solve music retrodict_SmallTwinkleTwinkle.lp
		;;
	30 )
		time code/solve music retrodict_ThreeBlindMiceSmall.lp
		;;
esac
