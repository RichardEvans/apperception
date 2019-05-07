echo "Running all experiments..."

sed -i -- 's/flag_condor = False/flag_condor = True/g' code/Interpretation.hs

scripts/compile_solve.sh

condor_submit condor/condor_sw.cmd