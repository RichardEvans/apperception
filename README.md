# The source code for *"Making sense of sensory input"*

## Installation instructions

You need to have installed Haskell and Clingo (version 4.5 or above).

1. To install Haskell:
    * go to https://www.haskell.org/downloads/

2. You also need to install various additional Haskell libraries:
    * `cabal install Cabal cabal-install`
    * cabal install <TODO>

3. To install Clingo:
    * go to https://potassco.org/clingo/

## Compilation instructions

Once you have clingo installed, Haskell installed, and also
the addition Haskell libraries, just run (from the root directory):

`scripts/compile_all.sh`

## Simple Examples

Once the system is installed (see above), you are ready to try some examples.

To run these examples, make sure you are in the root directory.

A single sensor oscillating between on and off:
`code/solve misc predict_1.lp`

Two sensors, one oscillates between on and off, while the other has the same reading throughout:
`code/solve misc predict_2.lp`

Exogenous action:
`code/solve misc exog_1.lp`

Searching through templates of increasing complexity, looking for a unified interpretation:
`code/solve eca_general predict_eca_245_b3.lp`

## More Complex Examples

These examples take significantly longer to run than the simple examples above. There is a time-limit of 4 hours per template. 

ECA prediction:
`code/solve eca predict_110_b5_t14.lp`

ECA retrodiction:
`code/solve eca retrodict_110_b5_t14.lp`

ECA imputation:
`code/solve eca impute_110_b5_t14.lp`

The Seek Whence "theme song" babbbbbcbbdb...
`code/solve sw predict_3.lp`

Music prediction:
`code/solve music predict_IncyWincySpiderSmall.lp`

Rhythm prediction:
`code/solve rhythm predict_Mazurka.lp`

Binding prediction:
`code/solve binding predict_r2_b5.lp`

Occlusion:
`code/solve occlusion w1`

Walker:
`code/solve walker predict w0`

Noisy sequences:
`code/solve noisy 1 3 2'

In general, solve can be run with any file in the data directory.
The options are:
    * `code/solve eca <file in data/eca>`
    * `code/solve sw <file in data/sw>`
    * `code/solve music <file in data/music>`
    * `code/solve rhythm <file in data/rhythm>`
    * `code/solve binding <file in data/binding>`
    * `code/solve occlusion <file in data/binding>`
    * `code/solve walker [predict/retrodict/impute] <world-id>`

## Understanding the output of the solve process

When solve is run, it produces...
* the theory *θ* composed of...
    * the initial conditions (*I*)
    * the rules (*R*)
    * the constraints (*C*)
* the trace (*τ(θ)*)
* statistics: the cost of the interpretation *θ*
* accuracy: whether or not all the predicted sensor readings match the hidden readings

To generate a latex-readable description of the output:
    * set `flag_output_latex = True` in Interpretation.hs
    * recompile: `scripts/compile_solve.sh`
    * run again

## Data generation

The data is already provided in the `data/` folder.

But if you want to regenerate it:
* `scripts/compile_all.sh`
* `cd code`
* `./eca all`
* `./sw all`
* `./music all`
* `./rhythm all`



