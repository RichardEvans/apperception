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
`scripts/solve.sh misc predict_1.lp`

In this example, there are two sensors. One oscillates between on and off, while the other has the same reading throughout:
`scripts/solve.sh misc predict_2.lp`

Here, we search through templates of increasing complexity, looking for a unified interpretation:
`scripts/solve.sh eca_general predict_eca_245_b3.lp`

## More Complex Examples

The following examples take significantly longer to run than the simple examples above. But all examples terminate within 24 hours (on a standard MacBook).

ECA prediction:
`scripts/solve.sh eca predict_110_b5_t14.lp`

ECA retrodiction:
`scripts/solve.sh eca retrodict_110_b5_t14.lp`

ECA imputation:
`scripts/solve.sh eca impute_110_b5_t14.lp`

The Seek Whence "theme song" babbbbbcbbdb...
`scripts/solve.sh sw predict_3.lp`

Music prediction:
`scripts/solve.sh music predict_IncyWincySpiderSmall.lp`

Rhythm prediction:
`scripts/solve.sh rhythm predict_Mazurka.lp`

Binding prediction:
`scripts/solve.sh binding predict_r2_b5.lp`

Occlusion:
`scripts/solve.sh occlusion w1`

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
* the theory *θ = (φ, I, R, C)* composed of...
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

## Precomputed results

The experimental results described in the paper are stored in the results folder.

## Data generation

The data is already provided in the `data/` folder.

But if you want to generate more data:
  * Compile ECA.hs, SW.hs, Music.hs, Rhythm.hs in the code directory
  * `cd code`
  * `./eca all`
  * `./sw all`
  * `./music all`
  * `./rhythm all`



