# Turing Machines for Real *(TM4R)*

Michael PÉRIN, Verimag / Univ. Grenoble-Alpes

## Projet 2019 : Réalisation d'une version 3 bandes de la Machine de Turing Universelle ([sujet](src/Universal/README.md))

## The TM4R project aims at developping the Turing Machine constructions

Those constructions have been described in books but never implemented:

- Emulation of a TM operating on an n-symbols alphabet by a TM operating on binary alphabet (2017)
- The Universal Turing Machine (2019)
- Emulation of a n-Bands TM by a single-band TM (2021 ?)
- The Universal Scheduler running two Turing Machines one step each (2023 ?)


## Modules already available

- Constructions for [defining Turing Machines](src/Turing_Machine.ml)
- An [execution engine](src/Execution.ml) with an html output
- An execution layer on top of the previous one for [emulation](src/Emulator.ml) of on-the-fly transformations
- An output of the control flow graph of a Turing_Machine in a `.dot` for the free graph vizualizer software ([graphviz](https://www.graphviz.org))


## Examples of Turing Machines

- Some examples of [single-band Turing Machines](/src/Turing_Machines/TM_Basic.ml)
- The known [busy beavers](/src/Turing_Machines/TM_Busy_Beaver.ml)
- Some examples of [two-bands Turing Machines](src/Turing_Machines/TM_2Bands.ml)


## Available outputs of the engines

### [demo/run/](demo/run/) contains html files

- The run of the 4-states Busy Beaver (107 steps)
- The run of a TM which erases the content of a vector while keeping the structure (..,..)
- The run of its binary version operating on the 2-symbols alphabet {T,F}
- The run of a 2-bands Turing Machine performing the xor of two bands


### [demo/dot/](demo/dot/) contains some control flow graphs in `.dot` format

- The 4-states Busy Beaver 
- The xor 2-bands Turing Machine
- The erase-vec 1-band Turing Machine 





