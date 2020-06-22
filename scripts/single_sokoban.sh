#!/bin/bash

case $(expr $1 + 1) in
	1 )
		echo "Solving sokoban example e0..."
		time code/solve sokoban e0
		;;
	2 )
		echo "Solving sokoban example e1..."
		time code/solve sokoban e1
		;;
	3 )
		echo "Solving sokoban example e2..."
		time code/solve sokoban e2
		;;
	4 )
		echo "Solving sokoban example e3..."
		time code/solve sokoban e3
		;;
	5 )
		echo "Solving sokoban example e4..."
		time code/solve sokoban e4
		;;
	6 )
		echo "Solving sokoban example e5..."
		time code/solve sokoban e5
		;;
	7 )
		echo "Solving sokoban example e6..."
		time code/solve sokoban e6
		;;
	8 )
		echo "Solving sokoban example e7..."
		time code/solve sokoban e7
		;;
	9 )
		echo "Solving sokoban example e8..."
		time code/solve sokoban e8
		;;
esac
