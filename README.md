# TIPE - Eric et Sylvain

**Problématique**: *Comment modéliser le développement des espaces productifs à travers une approche évolutionniste*

### Principe

Le but de ce TIPE est d'étudier l'évolution d'un système semi-urbain à travers un modèle simplifié. La modélisation s'effectue sur la base d'un algorithme évolutionniste, qui ne conserve que les systèmes les plus performants.

## Règles

### Batiments

Les **batiments** sont organisés en **villages**, connectés par des **routes**. Chaque batiment nécessite un certain nombre d'habitants pour son fonctionnement. Les habitants ont besoin lors de chaque tour pour survivre d'une ressource **logement** et d'une ressource **nouriture**. Un batiment est détruit s'il n'y a pas assez d'habitants pour le maintenir. Certains batiments ont aussi besoin de **matières premières** pour produire les ressources associés lors du tour.

### Types de batiments

- **Route**: n'est pas un batiment à part entière, emploie 0.1 personnes pour sa maintenance. Fait le lien entre les batiments pour qu'ils puissent interagir entre eux.
- **Maison**: fait apparaître 5 habitants dans le village, emploie 0.25 personnes pour sa maintenance. A besoin de 5 nouriture par tour.
- **Champ**: fait apparaître 5 grain dans le village, emploie 2 personnes. Le grain doit être transmis au moulin pour être transformé en nouriture.
- **Moulin**: fait apparaître 5 nourriture dans le village, emploie 2 personnes. Un nourriture permet de faire survivre une personne pour un tour.
- **Mine**: fait apparaître 5 minerais dans le village, emploie 2 personnes. Le minerai doit être transmis à une usine pour être transformée en machines.
- **Usine**: fait apparaître 1 machine qui sert de "boost" au batiment qui lui est assigné.


### Tableau récapitulatif

|        |  production   |  emploi  | matières premières |
|--------|---------------|----------|--------------------|
| Route  | -             |   0.1    |  -                 |
| Maison | 5 logement    |   0.25   |  5 nouriture       |
| Champ  | 5 grain       |   2      |  -                 |
| Moulin | 5 nouriture   |   2      |  5 grain           |
| Mine   | 5 minerai     |   2      |  -                 |
| Usine  | 1 machine     |   5      |  1 minerai         |

*Note: le calcul de l'emploi du batiment s'effectue après avoir pris la partie entière supérieure de la somme de l'emploi de chaque batiment de son type. Par exemple, si le village contient $n$ maisons, les maisons vont employer $\lceil n \times 0.25 \rceil$ habitants.*
