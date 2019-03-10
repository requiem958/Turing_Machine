TODO
====


## src/Simulator/ PROJET 2017

- **FIXME** `Emulator.ml` : provides means to write Emulators, ie. just-in-time compilers that can, for instance,
    - simulate the action of a TM operating on an alphabet \Sigma by a Turing Machine operating on bit-vectors encoding the symbols of \Sigma.


## Simulation d'une MT multi-bandes par une MT standard

## Language/

- reconnaissance de langages classiques

## Algo/

- réalisation de pi: Nat -> Nat : n -> la nième décimale de \pi
- réalisation du crible d'Erathostène


## Compilation 

- Enumération des macros-transitions et les etats labelises 

```Ocaml

 par foreach Sigma - {B} (fun l -> MT utilisant l)

let rec (foreach_symbol_in: 'a list -> ('a -> transitions) -> transitions) = fun symbols instrantiate_transitions_with ->
  match symbols with
  | [] -> []
  | s::other_symbols -> union (instrantiate_transitions_with s) (foreach_symbol_in other_symbols instrantiate_transitions_with)
```


DONE
====

### NOT TESTED: Application d'une MT multi-bandes à un sous-ensemble de bandes

- Introduire une action Run_on of band list * TM.
  Application: Run_on([4;2], Copy) qui recopie la bande 4 sur la bande 2 sans modifier la bande 1 ni la 3

Pour permettre cela il faut 
- attribuer un Id unique à chaque bande
- sélectionner les bandes [4;2] parmi les bandes [ B1,B2,B3,B4 ]
- appliquer la TM Copy à deux bandes vues comme (B'1=B4, B'2=B2) -TM COpy -> (B'1m=B4m,B'2m=B2m)  les versions modifiées
- reconstruire la configuration globale [B1,B2m,B3,B4m] à la fin de l'exécution de TM copy en se basant sur le Band.id de chaque bande.


## Affichage

- afficher un pas en plusieurs temps : lecture = configuration precedente ; ecriture ; deplacement (cf. Emulator)
- afficher le nom de la MT en cours d'execution
- afficher le nom de la bande
- afficher l'etat courant : sous quelle forme ? nombre = /!\ taille des fontes OU couleurs = comment les choisir ? OU les deux.


### Divers

- Restructuration : Fusionner Transition + Instruction + Turing_Machine

- une MT peut avoir plusieurs bandes

- une configuration contient la MT, son état courant, les bandes = liste de bande

- une execution peut faire tourner plusieurs machines en parrallèle
  une execution = liste de configuration = [ CMT1 ; CMT2 ; ... ]
