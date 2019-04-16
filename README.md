# The source code for *"Making sense of sensory input"*

## Installation instructions

You need to have installed Haskell and Clingo.

1. To install Haskell:
   * go to https://www.haskell.org/downloads/

2. To install Clingo (version 4.5 or above):
   * go to https://potassco.org/clingo/

## Compilation instructions

Once you have Haskell and Clingo installed, just run (from the root directory):
   * `cd code`
   * `cabal configure`
   * `cabal build`
   * `cabal install`

## Simple Examples

Once the system is installed (see above), you are ready to try some examples.

To run these examples, make sure you are in the root directory.

In this simple example, there is a single sensor oscillating between on and off:
`./solve.sh misc misc1`

In this example, there are two sensors. One oscillates between on and off, while the other has the same reading throughout:
`./solve.sh misc misc2`

Here, we search through templates of increasing complexity, looking for a unified interpretation:
`./solve.sh eca_general predict_eca_245_b3.lp`

## More Complex Examples

These examples take significantly longer to run than the simple examples above. There is a time-limit of 4 hours per template. 

ECA prediction:
`./solve.sh eca predict_110_b5_t14.lp`

ECA retrodiction:
`./solve.sh eca retrodict_110_b5_t14.lp`

ECA imputation:
`./solve.sh eca impute_110_b5_t14.lp`

The Seek Whence "theme song" babbbbbcbbdb...
`./solve.sh sw predict_3.lp`

Music prediction:
`./solve.sh music predict_IncyWincySpiderSmall.lp`

Rhythm prediction:
`./solve.sh rhythm predict_Mazurka.lp`

Binding prediction:
`./solve.sh binding predict_r2_b5.lp`

Occlusion:
`./solve.sh occlusion w1`

In general, solve can be run with any file in the data directory.
The options are:
    * `./solve.sh eca <file in data/eca>`
    * `./solve.sh sw <file in data/sw>`
    * `./solve.sh music <file in data/music>`
    * `./solve.sh rhythm <file in data/rhythm>`
    * `./solve.sh binding <file in data/binding>`
    * `./solve.sh occlusion <file in data/binding>`

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
    * recompile
    * run again

## Data generation

The data is already provided in the `data/` folder.

But if you want to generate more data:
* Compile ECA.hs, SW.hs, Music.hs, Rhythm.hs in the code directory
* `cd code`
* `./eca all`
* `./sw all`
* `./music all`
* `./rhythm all`



