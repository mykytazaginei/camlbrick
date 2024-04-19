# Introduction
This project is a OCaml implementation of a brick game. The main file is camlbrick.ml.

## Setup
To set up this project, you need to have OCaml installed on your machine. You can install it using the following command:
```
brew install ocaml
```
## Run
```
ocamlc -c camlbrick.ml
ocamlc -c -I +labltk labltk.cma camlbrick_gui.ml
ocamlc -c camlbrick_launcher.ml
ocamlc -o camlbrick -I +labltk labltk.cma camlbrick.cmo camlbrick_gui.cmo camlbrick_launcher.cmo
ocamlrun ./camlbrick
```
