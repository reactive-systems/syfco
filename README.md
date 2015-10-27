# Synthesis Format Conversion Tool (SyFCo v0.1.0.0)
A tool for reading, manipulating and transforming synthesis specifications in the .tlsf (temporal logic synthesis) format.

## About this tool
The tool interprets the high level constructs of the Temporal Logic Synthesis Format (TLSF) and supports the transformation of specifications to linear temporal logic (LTL) in different output formats. The tool has been designed to be modular with respect to the supported output formats and semantics. Furthermore, the tool can identify and manipulate parameters, targets and semantics of a specification on the fly, and thus allows comparative studies, as it is for example needed in the Synthesis Competition.

The main features of the tool can be summarized as follows:

* Evaluation of the high level constructs, i.e., the specification
  can be reduced to the fragment where no more parameter and variable
  bindings occur (i.e., without the GLOBAL section).
  
* Transformation to other existing specification formats, like
  Promela LTL, PSL, Unbeast, or the Extended AIGER Format.

* On the fly adjustment of parameters, semantics or targets.

* Preprocessing of the resulting LTL formula

    * conversion to negation normal form

    * replacement of derived operators

    * pushing/pulling next, eventually, or globally operators inwards/outwards

    * ...

## Installation
SyfCo is written in Haskell and can be compiled using the Glasgow Haskell Compiler (GHC).

Prerequisites:
* GHC >= 7.0.1
* parsec >=3.1 && <3.2

Steps for compilation:
* "make" in the root directory.

In case of problems, please write an e-mail to the author.

## Usage
To get detailed information about command line parameters, invoke the SyfCo tool with parameter "--help".

A number of synthesis benchmarks in TLSF can be found in /examples.

If you use emacs, you can try our emacs mode (tlsf-mode.el) to edit TLSF files, which can be found in /misc.

## Adding output formats
To add a new output format, first consider /Writer/Formats/Example.hs, which provides all standard constructs and contains a short tutorial.
