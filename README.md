# TIPE - Eric et Sylvain

**Problématique**: *Comment modéliser le développement des espaces productifs à travers une approche évolutionniste*

### Principe

Le but de ce TIPE est d'étudier l'évolution d'un système semi-urbain à travers un modèle simplifié. La modélisation s'effectue sur la base d'un algorithme évolutionniste, qui ne conserve que les systèmes les plus performants.

### Utilisation du projet

Rien n'a vraiment été implémenté, il faudra utiliser à l'avenir pour exécuter le projet
```
dune exec main.ml
```

## Règles

### Fonctionnement de la simulation

Une simulation est constitué de tours qui aboutissent ou non à la destruction ou à la survie d'un village. 

Au début d'une, l'algorithme génère un village qui consiste en une permutation aléatoire de $n$ batiments connectés par des routes. À chaque tour, l'algorithme calcule si un village survit ou doit être détruit. Les villages sans population sont supprimés, tandis que les deux villages les moins peuplés sont détruits. Les villages subissent une mutation à chaque tour, ce qui consiste à l'ajout, au retrait ou au changement d'affectation d'une usine.

### Villages

Les villages sont situés dans $\mathbb{Z}^2$ et ont leur coordonées de départ propres. Ils ont leur météo et leur environnement respectifs. Ils pouront à l'avenir intéragir en se faisant la guerre et en commerçant;

### Batiments

Les **batiments** sont organisés en **villages**. Chaque batiment nécessite un certain nombre d'habitants pour son fonctionnement. Les habitants ont besoin lors de chaque tour pour survivre d'une ressource **logement** et d'une ressource **nouriture**. Un batiment est détruit s'il n'y a pas assez d'habitants pour le maintenir. Certains batiments ont aussi besoin de **matières premières** pour produire les ressources associés lors du tour.

### Types de batiments

- **Maison**: fait apparaître 5 habitants dans le village, emploie 0.25 personnes pour sa maintenance. A besoin de 5 nouriture par tour.
- **Champ**: fait apparaître 5 grain dans le village, emploie 2 personnes. Le grain doit être transmis au moulin pour être transformé en nouriture.
- **Moulin**: fait apparaître 5 nourriture dans le village, emploie 2 personnes. Un nourriture permet de faire survivre une personne pour un tour.
- **Mine**: fait apparaître 5 minerais dans le village, emploie 2 personnes. Le minerai doit être transmis à une usine pour être transformée en machines.
- **Usine**: booste la production des batiments adjacents, utilise autant de minerais que de batiments boostés. Si il n'y a pas assez de ressources nécessaires, le batiment entre en pénurie et arrète de fonctionner. Un batiment boosté voit sa production doublé, mais ses besoins en matières premières doublent aussi. Une usine peut également être boosté. Une usine boosté booste des batiments en multipiant leurs productions par 2,5.

### Tableau récapitulatif

|        |  production    |  emploi  | matières premières |
|--------|----------------|----------|--------------------|
| Maison | 20 logement    |   1      |  5 nouriture       |
| Champ  | 20 grain       |   8      |  -                 |
| Moulin | 20 nouriture   |   8      |  5 grain           |
| Mine   | 20 minerai     |   8      |  -                 |
| Usine  | < 9 boosts     |   20     |  = nb de boosts    |

*Disclaimer*: Les valeurs sont indicativent et varient selon les environnements
