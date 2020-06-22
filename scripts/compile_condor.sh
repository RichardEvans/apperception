cd code
sed -i -- 's/flag_condor = False/flag_condor = True/g' Interpretation.hs
ghc -o solve -O2 Solve
cd ..
