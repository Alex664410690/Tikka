# Tikka

An educational Haskell Compiler and Debugger. Built as a university 3rd year project and dissertation by Alex Hobbs.

## Running the compiler

Tikka should be run from the main directory (due to the module system's reliance on relative paths), with the following command:

`./dist/compile [FILEPATH] <options>`

The list of available options and what they do can be found by running `./dist/compile --help`. The *repl* and *visual* flags are not compatible with one another, so should not be used simultaneously. For example, the following would compile *sample.hs* with static type checking enabled, dump the desugared code to *output.txt* and trace the evaluation in full with concise explanation, before entering REPL mode:

`./dist/compile sample.hs --stc --dump=output.txt --trace=3 --verb=2 --repl`

The available levels of evaluation tracing are:

1. No trace, only displays the final result (default)
2. Minimal trace, shows all steps of evaluation other than basic arithmetic, boolean and list operations
3. Full trace, shows all steps of evaluation

The available verbosity levels of explanation (when the trace is enabled) are:

1. No explanation (default)
2. Concise explanation
3. Concise explanation with details
4. Conversational explanation

## Prelude

Tikka has a *Prelude* (or standard library) of common functions (such as succ, elem, etc.) and lambda combinators. As Haskell requires that functions start with a lowercase letter, combinators are lowercase also ('k 1 2' is equivalent to 'K 1 2' in combinatory logic). To see the full list and their implementations, check out */imports/prelude*.

##

Please note: */dist/compile.exe* is a Windows executable file and */dist/compile* is a Linux binary file, and thus Tikka runs on both of those systems (the command is the same). Due to me not having access to a Mac, Tikka is incompatible with MacOS - if you have a Mac and would like to help me change that, do feel free to get in touch.

&copy; Alex Hobbs 2025  
alexhobbs.0515@gmail.com