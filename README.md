# Tikka

All of the code for this project can be found in the *app* directory: *Main.hs* drives the project, *Tikka.hs* contains the code for the interpreter itself, *Application.hs* contains the code for the visual debugger built with Monomer, and *TextAreaScroll.hs* is a modified Monomer TextArea widget (*Monomer.Widgets.Singles.TextArea*, open source) used in the IDE's code editor.

The *imports* directory contains the default modules which are imported by Tikka: *prelude* contains the standard library of Haskell functions, *local-decl* contains any functions declared in the REPL (cleared on startup), and *command-line* contains a function / expression which is executed in the REPL. Finally, the *dist* directory contains the interpreter executable as well as 3 required dll files which are not installed on Windows by default.

## Running the interpreter

Tikka should be run from the main directory (due to the module system's reliance on relative paths), with the following command:

`./dist/tikka [FILEPATH] <options>`

The list of available options and what they do can be found by running `./dist/tikka --help`. The *repl* and *visual* flags are not compatible with one another, so should not be used simultaneously. For example, the following would run *sample.hs* with static type checking enabled, dump the desugared code to *output.txt* and trace the evaluation in full with concise explanation, before entering REPL mode:

`./dist/tikka sample.hs --stc --dump=output.txt --trace=4 --verb=2 --repl`

The available levels of evaluation tracing are:

1. No trace, only displays the final result (default)
2. Minimal trace, excludes all pattern matching, case selection and operations
3. Reduced trace, shows all steps of evaluation other than basic arithmetic, boolean and list operations
4. Full trace, shows all steps of evaluation

The available verbosity levels of explanation (when the trace is enabled) are:

1. No explanation (default)
2. Concise explanation
3. Concise explanation with details
4. Conversational explanation

Please note: */dist/tikka.exe* is a Windows executable file, as Windows was used for the development of this project. The project should also be compatible with many linux distributions, including on the DCS machines as they were used for user testing. However, since the introduction of the visual debugger I no longer have the required permissions to build the project on the DCS machines as Monomer requires the installation of SDL2. If you would like to run the project on Linux, feel free to rebuild the project on your system as described below.

## Building the project

This project uses the Haskell Tool Stack, which can be installed [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/). Then the command

`stack build --copy-bins --local-bin-path=./dist`

can be used to build the project. This may take a while the first time it is run. If it encounters an error, run the same command again and it should install fully. If it does not, please contact me.