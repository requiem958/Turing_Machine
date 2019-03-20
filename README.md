# Turing Machines for Real

Michael PÉRIN, Verimag / Univ. Grenoble-Alpes

## Projet 2019 : Réalisation d'une version 3 bandes de la Machine de Turing Universelle ([sujet](src/Universal/README.md))

## The TM4R project aims at developping the Turing Machine constructions

Those constructions have been (partially) described in books but never implemented:

- Emulation of a TM operating on an n-symbols alphabet by a TM operating on binary alphabet (2017)
- The Universal Turing Machine (2019)
- Emulation of a n-Bands TM by a single-band TM (2021 ?)
- The Universal Scheduler running two Turing Machine one step each (2023 ?)


## Already available

- Constructions for [defining Turing Machines](src/Turing_Machine.ml)

- An [execution engine](src/Execution.ml) with an html output

- An execution engine for [emulation](src/Emulator.ml) of on-the-fly transformations

- An dot output to see the control flow graph of a Turing_Machine
  ([Turing_Machine.ml](src/Turing_Machine.ml), [Dot.ml](src/lib/Dot.ml))

- Some examples of [single-band Turing Machines](/src/Turing_Machines/TM_Basic.ml)

- The known [busy beavers](/src/Turing_Machines/TM_Busy_Beaver.ml)

- Some examples of [two-bands Turing Machines](src/Turing_Machines/TM_2Bands.ml)


## Available outputs

### [demo/run/](demo/run/) contains html files

- The run of the 4-states Busy Beaver ([107 steps](demo/run/TM_BB4_1xBand.html))

- The run of a TM which erases the content of a vector while keeping the structure (..,..)

- The run of its binary version operating on 

- The run of a 2-bands Turing Machine performing the xor of two bands


### [demo/dot/](demo/dot/) contains some control flow graphs in `.dot` format

- the 4-states Busy Beaver (BB4.dot)






