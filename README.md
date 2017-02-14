# isa_translator

This repository contains 4 directories
 - ocaml_translator
 - sml_translator
 - proto
 - prolog

### ocaml_translator contains: 

####ocaml_translator.ml
This program is the most important file in the repo and follows my most up to date encoding methods and ideas.
It creates constraints for different program/state lengths and solves them via the z3 ocaml api and prints the generated program.
I have written detailed comments in this file if you want to learn more.

####make 
This bash script that compiles ocaml_translator.ml (you may need to make this executable with the command 'chmod +x make')

### sml_translator contains:

####sml_translator.sml
This is a standard ml program that contains various helper methods that create strings which are smt2 language expressions.
Using these helper methods, a complex smt constraint is created and the string is written to the file 'sml_gen.smt2'.
Z3 can take this file as input with the command z3 sml_gen.smt2.

####make 
This is bash script that compiles sml_translator.sml (you may need to make this executable with the command 'chmod +x make')

### proto contains:
The directory proto (as in prototype) contains 20 or so different smt2 files (starting at proto1.smt2 up to proto21.smt2)
Through writing these I learnt the lisp-like smt language that z3 can take as input and helped me design and experiment with different encoding formats.
However, even the latest one is out of date and the encoding format in the ocaml_translator.ml file has surpassed these proto files.

Also in the proto directory:

####python_api.py
This is a python file that uses the python z3 api. The python z3 api is much easier and more user friendly than the ocaml or c++ api.
So if you are not locked into using the ocaml/c++ api I recommend python (although maybe this comes at a price, perhaps the python api is not as flexible).

####generator.py
This writes smt constraints to a file called proto_gen.smt2. 
It's not pretty and you should probably just ignore these.

### prolog contains:

####arm.pl 
This is a prolog program I wrote as an experiment. It is a toy super-optimizer that searchs for a satisfiable program given a pre and post state.
It does not scale well at all though and takes too long to find programs greater than 3 instructions.

