Contents
========

## lib/

- `Date.ml` : provides Unix time in a human-readable format

- `MyList.ml` : Extension of the List module with additional functions

- `MyString.ml` : Extension of the String module with additional functions

- `Trick.ml` : Some usefuk tricks for programming in Ocaml

- `Pretty.ml` : A library for pretty printing in Latex | Ascii | Html | Dot

- `Bit_Vector.ml` :  Operation on bit vectors parmaterized by the representation of bits (using functor). It requires an instranciation of a Bit module defining Two_Values zero and unit

- `Color.ml` : A library of predefined colors and color format conversion

- `Html.ml` : A library for generating html output	 

- `Pattern.ml` :  A library for generic pattern matching of symbols with respect to an alphabet = list of symbols
    It provides the constructions
    - ANY symbols
    - any symbols BUT this symbol
    - any symbols IN  a list of symbols
    - any symbols OUT of a list of symbols


## [src/](src/) 

- `Makefile` : Here are the possible commands:
   - make cmo ...... produces .cmo files. This command is used by
   - make run ...... produces an executable called "run" and executes it
   - make play ..... load the .cmo files in the ocaml interpreter. Type #use "main.ml";;  in the interpreter to execute the main function.
   - make clean .... delete compilation files
 
Each module containts a demo function called `demo()` (sometimes it is defined in a Demo module)

- `main.ml` : calls the demo functions of each interesting module

- `Symbol.ml` : Symbols of the alphabet used by Turing Machines

- `Alphabet.ml` : defines the set of symbols used by a Turing Machines:  baton, binary, ...
     and the conversion of symboles into vectors of bits for Turing Machines that operate on binary alphabet.

- `State.ml` : Representation of the states of a Turing Machine

- `Band.ml` : Representation of Bands of Turing Machines

     The alphabet is needed for translation into a binary representation of the symbols.
     The alphabet is associated with the band (instead of being associated with the TM) for two reasons:
     
     1. outside the execution of a TM on the band, we would not know the alphabet and we would not be able to perform translation
     2. we will apply several TM in sequence on the band so they should agree and exchange the alphabet: that's complicated...
     3. Some of these TM can be generic, meaning that their instanciations depends on the alphabet
     
     Thus, it seems more convenient that the band sets the alphabet at creation.

     **FIXME** Translation to binary should be done in module Emulator.ml using module Binary

- `Action.ml` : Basic actions of a Turing Machine

- `Turing_Machine.ml` : Representation of Turing Machines as a list of Transitions

- `Execution.ml` : Execution of single-band or multi-bands Turing Machines with logging of the execution steps


### [src/Turing_Machines/](src/Turing_Machines/)

 - `TM_Basic.ml` : A collection of Basic Standard Turing Machines

 - `TM_2Bands.ml` : A collection of Two-Bands Turing Machines

 - `TM_Busy_Beaver.ml` : The busy beavers BB4, BB5, BB6

### [src/Universal/](src/Universal/)

  - `README.md` 
  
  - `UTM.ml` : The Universal Turing Machine as 3-Bands Turing Machine (PROJET 2019)

### src/Lambda_Calculus/

  - `Lambda_Calcul.ml` : defines lambda terms, reduction with tracing of beta-reduction (no dependency with Turing Machine)
  
  - `LC_by_TM.ml` : Simulation of Lambda-Caculus by Turing Machine and comparison of the result

  - `TM_substitution.ml` : part of the project Lambda-Calculus Simulated by Turing Machines


### src/Simulator/

- **FIXME** `Emulator.ml` : provides means to write Emulators, ie. just-in-time compilers that can, for instance,
    - simulate the action of a TM operating on an alphabet \Sigma by a Turing Machine operating on bit-vectors encoding the symbols of \Sigma.



