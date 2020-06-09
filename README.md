# Synthesis Format Conversion Tool<br/>(Version 1.2.1.1)

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
  Boolean identities).

* On the fly adjustment of parameters, semantics or targets.

* Preprocessing of the resulting LTL formula.

* Conversion to negation normal form.

* Replacement of derived operators.

* Pushing/pulling next, eventually, or globally operators
  inwards/outwards.

* Standard simplifications.

## Installation

SyfCo is written in Haskell and can be compiled using the
Glasgow Haskell Compiler (GHC). To install the tool you can either
use [cabal](https://www.haskell.org/cabal) or [stack](https://docs.haskellstack.org/en/stable/README/) (recommended).
For more information about the purpose of these tools and why you
should prefer using stack instead of cabal, we recommend reading
[this blog post](https://www.fpcomplete.com/blog/2015/06/why-is-stack-not-cabal) by Mathieu Boespflug. 

To install the tool with stack use:

`stack install`

Stack then automatically fetches the right compiler version
and required dependencies. After that it builds and installs
the package into you local stack path. If you instead prefer
to build only, use `stack build`.

If you insist to use cabal instead, we recommend at least to use
a sandbox. Initialize the sandbox and configure the project via

`cabal sandbox init && cabal configure`

Then use `cabal build` or `cabal install` to build or install the
tool.

Note that (independent of the chosen build method) building the
tool will only create the final executable in a hidden sub-folder,
which might get cumbersome for development or testing local changes.
Hence, for this purpose, you may prefer to use `make`. The makefile
determines the chosen build method, rebuilds the package, and copies
the final `syfco` executable to the root directory of the project.

If you still encounter any problems, please inform us via the
[project bug tracker](https://github.com/reactive-systems/syfco/issues).

## Usage

<code>syfco [OPTIONS]... <file></code>

#### File Operations:

|Command|Description|
|-------|-----------|
|<code>-o, --output</code>|path of the output file (results are printed to STDOUT if not set)|
|<code>-r, --read-config</code>|read parameters from the given configuration file (may overwrite prior arguments)|
|<code>-w, --write-config</code>|write the current configuration to the given path (includes later arguments)|
|<code>-f, --format</code>|output format - possible values are:</br> <table><tbody> <tr><td><code>full</code></td><td>input file with applied transformations (default)</td></tr><tr><td><code>basic</code></td><td>high level format (without global section)</td></tr><tr><td><code>utf8</code></td><td>human readable output using UTF8 symbols</td></tr><tr><td><code>wring</code></td><td>Wring input format</td></tr><tr><td><code>lily</code></td><td>Lily input format</td></tr><tr><td><code>acacia</code></td><td>Acacia / Acacia+ input format</td></tr><tr><td><code>acacia-specs</code></td><td>Acacia input format with spec units</td></tr><tr><td><code>ltlxba</code></td><td>LTL2BA / LTL3BA input format</td></tr><tr><td><code>ltlxba-decomp</code></td><td>LTL2BA / LTL3BA input format (decomposed)</td></tr><tr><td><code>ltl</code></td><td>pure LTL formula</td></tr><tr><td><code>promela</code></td><td>Promela LTL</td></tr><tr><td><code>unbeast</code></td><td>Unbeast input format</td></tr><tr><td><code>slugs</code></td><td>structured Slugs format [GR(1) only]</td></tr><tr><td><code>slugsin</code></td><td>SlugsIn format [GR(1) only]</td></tr><tr><td><code>psl</code></td><td>PSL Syntax</td></tr><tr><td><code>smv</code></td><td>SMV file format</td></tr><tr><td><code>smv-decomp</code></td><td>SMV file format (decomposed)</td></tr><tr><td><code>bosy</code></td><td>Bosy input format</td></tr><tr><td><code>rabinizer</code></td><td>Rabinizer input format</td></tr> </tbody></table>|
|<code>-m, --mode</code>|output mode - possible values are:</br> <table><tbody> <tr><td><code>pretty</code></td><td>pretty printing (as less parentheses as possible) (default)</td></tr><tr><td><code>fully</code></td><td>output fully parenthesized formulas</td></tr> </tbody></table>|
|<code>-q, --quote</code>|quote mode - possible values are:</br> <table><tbody> <tr><td><code>none</code></td><td>identifiers are not quoted (default)</td></tr><tr><td><code>double</code></td><td>identifiers are quoted using "</td></tr> </tbody></table>|
|<code>-pf, --part-file</code>|create a partitioning (<code>.part</code>) file|
|<code>-bd, --bus-delimiter</code>|delimiter used to print indexed bus signals</br> (default: <code>_</code>)|
|<code>-ps, --prime-symbol</code>|symbol/string denoting primes in signals</br> (default: <code>'</code>)|
|<code>-as, --at-symbol</code>|symbol/string denoting @-symbols in signals</br> (default: <code>@</code>)|
|<code>-in, --stdin</code>|read the input file from STDIN|

#### File Modifications:

|Command|Description|
|-------|-----------|
|<code>-os, --overwrite-semantics</code>|overwrite the semantics of the file|
|<code>-ot, --overwrite-target</code>|overwrite the target of the file|
|<code>-op, --overwrite-parameter</code>|overwrite a parameter of the file|

#### Formula Transformations (disabled by default):

|Command|Description|
|-------|-----------|
|<code>-s0, --weak-simplify</code>|simple simplifications (removal of true/false in boolean connectives, redundant temporal operators, etc.)|
|<code>-s1, --strong-simplify</code>|advanced simplifications</br> (includes: <code>-s0 -nnf -nw -nr -pgo -pfo -pxo</code>)|
|<code>-nnf, --negation-normal-form</code>|convert the resulting LTL formula into negation normal form|
|<code>-pgi, --push-globally-inwards</code>|push global operators inwards</br>   <code>G (a && b) => (G a) && (G b)</code>|
|<code>-pfi, --push-finally-inwards</code>|push finally operators inwards</br>   <code>F (a &#124;&#124; b) => (F a) &#124;&#124; (F b)</code>|
|<code>-pxi, --push-next-inwards</code>|push next operators inwards</br>   <code>X (a && b) => (X a) && (X b)</code></br>   <code>X (a &#124;&#124; b) => (X a) &#124;&#124; (X b)</code>|
|<code>-pgo, --pull-globally-outwards</code>|pull global operators outwards</br>   <code>(G a) && (G b) => G (a && b)</code>|
|<code>-pfo, --pull-finally-outwards</code>|pull finally operators outwards</br>   <code>(F a) &#124;&#124; (F b) => F (a &#124;&#124; b)</code>|
|<code>-pxo, --pull-next-outwards</code>|pull next operators outwards</br>   <code>(X a) && (X b) => X (a && b)</code></br>   <code>(X a) &#124;&#124; (X b) => X (a &#124;&#124; b)</code>|
|<code>-nw, --no-weak-until</code>|replace weak until operators</br>   <code>a W b => (a U b) &#124;&#124; (G a)</code>|
|<code>-nr, --no-release</code>|replace release operators</br>   <code>a R b => b W (a && b)</code>|
|<code>-nf, --no-finally</code>|replace finally operators</br>   <code>F a => true U a</code>|
|<code>-ng, --no-globally</code>|replace global operators</br>   <code>G a => false R a</code>|
|<code>-nd, --no-derived</code>|same as: <code>-nw -nf -ng</code>|

#### Check Specification Type (and exit):

|Command|Description|
|-------|-----------|
|<code>-gr, --generalized-reactivity</code>|check whether the input is in the Generalized Reactivity fragment|

#### Extract Information (and exit):

|Command|Description|
|-------|-----------|
|<code>-c, --check</code>|check that input conforms to TLSF|
|<code>-t, --print-title</code>|output the title of the input file|
|<code>-d, --print-description</code>|output the description of the input file|
|<code>-s, --print-semantics</code>|output the semantics of the input file|
|<code>-g, --print-target</code>|output the target of the input file|
|<code>-a, --print-tags</code>|output the target of the input file|
|<code>-p, --print-parameters</code>|output the parameters of the input file|
|<code>-i, --print-info</code>|output all data of the info section|
|<code>-ins, --print-input-signals</code>|output the input signals of the specification|
|<code>-outs, --print-output-signals</code>|output the output signals of the specification|
|<code>-v, --version</code>|output version information|
|<code>-h, --help</code>|display this help|

#### Sample Usage:

```
syfco -o converted -f promela -m fully -nnf -nd file.tlsf
syfco -f psl -op n=3 -os Strict,Mealy -o converted file.tlsf
syfco -o converted -in
syfco -t file.tlsf
```


## Examples

A number of synthesis benchmarks in TLSF can be found in the
<code>/examples</code> directory.

## Syfco Library

Syfco is also provided as a Haskell library. In fact, the syfco
executable is nothing different than a fancy command line interface
to this library. If you are interested in using the interface, we
recommend to build and check the interface documentation, which is
generated by:

`make haddock`

## Editor Support

If you use [Emacs](https://www.gnu.org/software/emacs), you should try our emacs mode (<code>tlsf-mode.el</code>),
which can be found in the <code>/misc</code> directory.

## Adding output formats

If you like to add a new output format, first consider
<code>/Writer/Formats/Example.hs</code>, which contains the most common
standard constructs and a short tutorial.
