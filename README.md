# Tikka

Tikka is an educational interpreter and debugger for a modified subset of Haskell, designed for use in introductory Haskell and wider functional programming courses or for anyone looking to learn the language. The original paper outlining the core features and university-level classroom testing of Tikka can be found [here](https://dl.acm.org/doi/10.1145/3830439.3831274), and in the Proceedings of the 19th ACM SIGPLAN Haskell Symposium. Core features of Tikka include (but are not limited to):
- a simplified and modified subset of Haskell for new learners (notably excluding type classes and monads);
- a standard library "Prelude" of predefined operators and functions;
- Hindley-Milner type checking, in both staticly- and dynamically-type checked modes;
- evaluation tracing with step-by-step explanation;
- a special `TRACE` keyword to enable tracing of only specific sections of code;
- an interactive Real-Evaluate-Print Loop (REPL); and
- an Interactive Development Environment (IDE) for learners less comfortable with the command line.

The project is built using the Haskell Tool Stack (commonly referred to as just "stack"). Details for running the pre-built versions or building from scratch can be found below. We encourage and warmly welcome contributions to the project as well as all feedback, helping us to improve the tool and Haskell's beginner experience a little bit more. Please do not hesitate to reach out to the project maintainer Alex Hobbs (alexhobbs.0515@gmail.com). We hope you enjoy Tikka and find it useful for teaching and learning!

## Project Structure

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

Please note: */dist/tikka.exe* is a Windows executable file, and comes with some required library (*.dll*) files. */dist/tikka* is a Linux binary file, and comes with its own library (*.so*) files. A pre-built executable for MacOS does not currently exist, but if you are able to build Tikka on that platform and are happy sharing it with other users, then please reach out to the [maintainer](alexhobbs.0515@gmail.com).

## Building the project

This project uses the Haskell Tool Stack, which can be installed [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/). Then the command

`stack build --copy-bins --local-bin-path=./dist`

can be used to build the project. This may take a while the first time it is run. If it encounters an error, run the same command again and it should install fully. If it still does not after the third attempt, please contact the [maintainer](alexhobbs.0515@gmail.com).

## License
Copyright (c) 2026 Alex Hobbs and Alex Dixon. 
This project is licensed under the [BSD 3-Clause License](https://opensource.org/license/bsd-3-clause).

Permission to use the "Lambda" and "Night mode" icons (in the IDE) granted by [Flaticon](https://www.flaticon.com/).
