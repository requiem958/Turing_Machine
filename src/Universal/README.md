# PROJET 2019 : Réalisation de la Machine Universelle

(sous la forme d'une Machine de Turing à 3 bandes)

> La connaissance du langage Ocaml n'est pas nécessaire pour réaliser ce projet.

## Installation

1. Récupérez le projet **Turing_Machine** : cliquez sur `clone` et choississez `https`
2. Installez Ocaml : voir [INSTALL.md](../../INSTALL.md)
3. Testez votre installation à l'aide des commandes suivantes
   - `cd src/` puis `make play` qui lance l'interprète ocaml dans un terminal
   - tapez `Demo.demo();;` dans l'interprète puis `#quit;;` ou `Ctrl-D` pour quitter
   - ouvrez les fichiers `html` générés par le simulateur dans le répertoire `_log/`
4. Dans l'interprète `UTM.demo();;` exécute la démo (à compléter) de la Machine Universelle

## Travail à réaliser par groupe de 4 étudiants

1. Relisez le chapitre 6 du [cours](http://www-verimag.imag.fr/~perin/enseignement/L3/mcal/cours/MCAL-MT-CM-a-trous.pdf)
2. Complétez la définition de la machine universelle `utm` dans le fichier [UTM.ml](src/Universal/UTM.ml)

   *utm(m,w)* est un interpréteur qui exécute le code de la machine *m* sur le mot d'entrée *w*.
   
   Je vous suggère une version encore plus simple à réaliser que celle du cours à l'aide du MT à 3 bandes.
   - La bande 1 contient l'entrée *w* à traiter
   - La bande 2 contient la listes des transitions de la machine *m*. Par convention le premier état de la liste est l'état initial.
   - La bande 3 contient l'état courant de la machine *m*
    
   Vous pouvez vous inspirer des exemples de MT 
   - à 1 bande  dans [src/Turing_Machines/TM_Basic.ml](../Turing_Machines/TM_Basic.ml)
   - à 2 bandes dans [src/Turing_Machines/TM_2Bands.ml](../Turing_Machines/TM_2Bands.ml)


3. Donnez le code des machines `incr` et `decr`
4. Utilisez le simulateur pour tester votre machine universelle avec les trois machines fournies : `neg`, `incr` et `decr`
5. Préparez pour la soutenance de (5 minutes) une fonction `démo()` qui montre le bon fonctionnement de votre machine universelle 

