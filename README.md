# Synthesis Format Conversion Tool<br/>(Version 1.0.0.22)

A tool for reading, manipulating and transforming synthesis
specifications in [TLSF](https://arxiv.org/abs/1604.02284).

## About this tool

The tool interprets the high level constructs of [TLSF 1.1](https://arxiv.org/abs/1604.02284)
(functions, sets, ...) and supports the transformation of the
specification to Linear Temporal Logic (LTL) in different output
formats. The tool has been designed to be modular with respect to the
supported output formats and semantics. Furthermore, the tool allows
to identify and manipulate parameters, targets and semantics of a
specification on the fly. This is especially thought to be useful for
comparative studies, as they are for example needed in the
[Synthesis Competition](http://www.syntcomp.org).

The main features of the tool are summarized as follows:

* Interpretation of high level constructs, which allows to reduce the
  specification to its basic fragment where no more parameter and
  variable bindings occur (i.e., without the GLOBAL section).

* Transformation to other existing specification formats, like
  Basic TLSF, [Promela LTL](http://spinroot.com/spin/Man/ltl.html), [PSL](https://en.wikipedia.org/wiki/Property_Specification_Language), [Unbeast](https://www.react.uni-saarland.de/tools/unbeast), [Wring](http://www.ist.tugraz.at/staff/bloem/wring.html),
  [structured Slugs](https://github.com/VerifiableRobotics/slugs/blob/master/doc/input_formats.md#structuredslugs), and [SlugsIn](https://github.com/VerifiableRobotics/slugs/blob/master/doc/input_formats.md#slugsin).

* Syntactical analysis of membership in GR(k) for any k (modulo
  boolean identities).

* On the fly adjustment of parameters, semantics or targets.

* Preprocessing of the resulting LTL formula.

* Conversion to negation normal form.

* Replacement of derived operators.

* Pushing/pulling next, eventually, or globally operators
  inwards/outwards.

* Standard simplifications.

## Installation

SyfCo is written in Haskell and can be compiled using the
Glasgow Haskell Compiler (GHC).

Prerequisites:

* [GHC](https://www.haskell.org/ghc) (recommended version: >= 7.0.1, [Haskell2010](https://wiki.haskell.org/Definition))

* [parsec](https://hackage.haskell.org/package/parsec) (recommended version: >= 3.1)

* [array](https://hackage.haskell.org/package/array) (recommended version: >= 0.5)

* [containers](https://hackage.haskell.org/package/containers) (recommended version: >= 0.5)

* [directory](https://hackage.haskell.org/package/directory) (recommended version: >= 1.2)

* [mtl](https://hackage.haskell.org/package/mtl) (recommended version: >= 2.2)

* [transformers](https://hackage.haskell.org/package/transformers) (recommended version: >= 0.4)

To install the above dependencies, build the tool,
and install it with [cabal](https://www.haskell.org/cabal):

`cabal install`

If the dependencies are already installed,
then build (no installation) with:

`make`

However, if you encounter any problems,
please inform us via [the project bug tracker](https://github.com/reactive-systems/syfco/issues).

## Usage

```syfco [OPTIONS]... <file>```

#### File Operations:

|Command|Description|
|-------|-----------|
|```-o, --output```|path of the output file (results are printed to STDOUT if not set)|
|```-r, --read-config```|read parameters from the given configuration file (may overwrite prior arguments)|
|```-w, --write-config```|write the current configuration to the given path (includes later arguments)|
|```-f, --format```|output format - possible values are:</br> <table><tbody> <tr><td>```full```</td><td>input file with applied transformations (default)</td></tr><tr><td>```basic```</td><td>high level format (without global section)</td></tr><tr><td>```utf8```</td><td>human readable output using UTF8 symbols</td></tr><tr><td>```wring```</td><td>Wring input format</td></tr><tr><td>```lily```</td><td>Lily input format</td></tr><tr><td>```acacia```</td><td>Acacia / Acacia+ input format</td></tr><tr><td>```acacia-specs```</td><td>Acacia input format with spec units</td></tr><tr><td>```ltlxba```</td><td>LTL2BA / LTL3BA input format</td></tr><tr><td>```promela```</td><td>Promela LTL</td></tr><tr><td>```unbeast```</td><td>Unbeast input format</td></tr><tr><td>```slugs```</td><td>structured Slugs format [GR(1) only]</td></tr><tr><td>```slugsin```</td><td>SlugsIn format [GR(1) only]</td></tr><tr><td>```psl```</td><td>PSL Syntax</td></tr><tr><td>```smv```</td><td>SMV file format</td></tr> </tbody></table>|
|```-m, --mode```|output mode - possible values are:</br> <table><tbody> <tr><td>```pretty```</td><td>pretty printing (as less parentheses as possible) (default)</td></tr><tr><td>```fully```</td><td>output fully parenthesized formulas</td></tr> </tbody></table>|
|```-pf, --part-file```|create a partitioning (```.part```) file|
|```-bd, --bus-delimiter```|delimiter used to print indexed bus signals</br> (default: ```_```)|
|```-ps, --prime-symbol```|symbol/string denoting primes in signals</br> (default: ```'```)|
|```-as, --at-symbol```|symbol/string denoting @-symbols in signals</br> (default: ```@```)|
|```-in, --stdin```|read the input file from STDIN|

#### File Modifications:

|Command|Description|
|-------|-----------|
|```-os, --overwrite-semantics```|overwrite the semantics of the file|
|```-ot, --overwrite-target```|overwrite the target of the file|
|```-op, --overwrite-parameter```|overwrite a parameter of the file|

#### Formula Transformations (disabled by default):

|Command|Description|
|-------|-----------|
|```-s0, --weak-simplify```|simple simplifications (removal of true/false in boolean connectives, redundant temporal operators, etc.)|
|```-s1, --strong-simplify```|advanced simplifications</br> (includes: ```-s0 -nnf -nw -nr -pgo -pfo -pxo```)|
|```-nnf, --negation-normal-form```|convert the resulting LTL formula into negation normal form|
|```-pgi, --push-globally-inwards```|push global operators inwards</br>   ```G (a && b) => (G a) && (G b)```|
|```-pfi, --push-finally-inwards```|push finally operators inwards</br>   ```F (a || b) => (F a) || (F b)```|
|```-pxi, --push-next-inwards```|push next operators inwards</br>   ```X (a && b) => (X a) && (X b)```</br>   ```X (a || b) => (X a) || (X b)```|
|```-pgo, --pull-globally-outwards```|pull global operators outwards</br>   ```(G a) && (G b) => G (a && b)```|
|```-pfo, --pull-finally-outwards```|pull finally operators outwards</br>   ```(F a) || (F b) => F (a || b)```|
|```-pxo, --pull-next-outwards```|pull next operators outwards</br>   ```(X a) && (X b) => X (a && b)```</br>   ```(X a) || (X b) => X (a || b)```|
|```-nw, --no-weak-until```|replace weak until operators</br>   ```a W b => (a U b) || (G a)```|
|```-nr, --no-release```|replace release operators</br>   ```a R b => b W (a && b)```|
|```-nf, --no-finally```|replace finally operators</br>   ```F a => true U a```|
|```-ng, --no-globally```|replace global operators</br>   ```G a => false R a```|
|```-nd, --no-derived```|same as: ```-nw -nf -ng```|

#### Check Specification Type (and exit):

|Command|Description|
|-------|-----------|
|```-gr, --generalized-reactivity```|check whether the input is in the Generalized Reactivity fragment|

#### Extract Information (and exit):

|Command|Description|
|-------|-----------|
|```-c, --check```|check that input conforms to TLSF|
|```-t, --print-title```|output the title of the input file|
|```-d, --print-description```|output the description of the input file|
|```-s, --print-semantics```|output the semantics of the input file|
|```-g, --print-target```|output the target of the input file|
|```-a, --print-tags```|output the target of the input file|
|```-p, --print-parameters```|output the parameters of the input file|
|```-i, --print-info```|output all data of the info section|
|```-ins, --print-input-signals```|output the input signals of the specification|
|```-outs, --print-output-signals```|output the output signals of the specification|
|```-v, --version```|output version information|
|```-h, --help```|display this help|

#### Sample Usage:

```
syfco -o converted -f promela -m fully -nnf -nd file.tlsf
syfco -f psl -op n=3 -os Strict,Mealy -o converted file.tlsf
syfco -o converted -in
syfco -t file.tlsf
```


## Examples

A number of synthesis benchmarks in TLSF can be found in the
```/examples``` directory.

## Editor Support

If you use [Emacs](https://www.gnu.org/software/emacs), you should try our emacs mode (```tlsf-mode.el```),
which can be found in the ```/misc``` directory.

## Adding output formats

If you like to add a new output format, first consider
```/Writer/Formats/Example.hs```, which contains the most common
standard constructs and a short tutorial.
