# Contents

lib/

- `Date.ml` : print Unix time

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




Each module contains a demo function called `demo()`

- `main.ml` : calls the demo functions of each interesting module

- `Symbol.ml` : Symbols of the alphabet used by Turing Machines

- FIXME `Alpahbet.ml` : defines the set of symbols used by a Turing Machines:  baton, binary, full.
     and the conversion into vectors of bits for Turing Machines that operate on binary alphabet.

- `State.ml` : Representation of the states of a Turing Machine


- FIXME `Band.ml` : representation of Bands of Turing Machines

     The alphabet is needed for translation into a binary representation of the symbols.
     The alphabet is associated with the band (instead of being associated with the TM) for two reasons:
       1. outside the execution of a TM on the band, we would not know the alphabet and we would not be able to perform translation
       2. we will apply several TM in sequence on the band so they should agree and exchange the alphabet: that's complicated...
       3. Some of these TM can be generic, meaning that their instanciations depends on the alphabet
     Thus, it seems more convenient that the band sets the alphabet at creation.

    FIXME 2019 : Translation to binary : TODO in module Emulator.ml using module Binary

- `Action.ml` : basic actions of a Turing Machine

- `Transition.ml` :  Transitions of Turing Machines

- `Turing_Machine.ml` : definition of Turing Machines

- `Execution.ml` : I'M HERE

- FIXME `Emulator.ml` : provides means to write Emulators, ie. just-in-time compilers which can simulate the action of a TM operating on an alphabet ALPHA
                        by a Turing Machine operating on bit-vectors encoding the symbols of ALPHA.



? simulator/

Analyse du cycle de vie d’un smartphone sur 2 et 5 ans


Turing_Machines/

 - `TM_Basic.ml` : A collection of Basic Turing Machines

 - `TM_2Bands.ml` : A collection of Two-Bands Turing Machines

 - `TM_Busy_Beaver.ml` : The busy beavers BB4,BB5,BB6


Language/


Lambda_Calculus/

  - `Lambda_Calcul.ml` : lambda terms, reduction with tracing of beta-reduction

  - `TM_substituation.ml` : part of the sub-project LAMBDA-CALCULUS SIMULATED BY TURING MACHINES: substitution

  - `LC_by_TM.ml` : main 

Universal/

