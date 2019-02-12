# TODO

## Affichage

- afficher un pas en plusieurs temps : lecture = configuration precedente ; ecriture ; deplacement
- afficher le nom de la MT en cours d'execution
- afficher l'etat courant : sous quelle forme ? nombre = /!\ taille des fontes OU couleurs = comment les choisir ? OU les deux.

## Compilation 

- Enumération des macros-transitions et les etats labelises 

```Ocaml

 par foreach Sigma - {B} (fun l -> MT utilisant l)

let rec (foreach_symbol_in: 'a list -> ('a -> transitions) -> transitions) = fun symbols instrantiate_transitions_with ->
  match symbols with
  | [] -> []
  | s::other_symbols -> union (instrantiate_transitions_with s) (foreach_symbol_in other_symbols instrantiate_transitions_with)
```

## Simulateur

- la touche 1 fait faire une transition à la MT 1 et affiche la configuration CMT1 
  ...
  la touche i fait faire une transition à la MT i et affiche la configuration CMTi 

- la touche ESPACE fait faire une transition à chaque MT et affiche l'ensemble des configurations.

- la trouche T effectue la traduction de la MT1 en MT2 (binaire) et ajoute la configuration de MT2 `dans la liste de configuration

## Done

- une MT peut avoir plusieurs bandes

- une configuration contient la MT, son état courant, les bandes = liste de bande

- une execution peut faire tourner plusieurs machines en parrallèle
  une execution = liste de configuration = [ CMT1 ; CMT2 ; ... ]