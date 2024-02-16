# TIPE - Eric et Sylvain

**Problématique**: *Comment modéliser le développement des espaces productifs à travers une approche évolutionniste*

## Principe

Le but de ce TIPE est d'étudier l'évolution d'un système semi-urbain à travers un modèle simplifié. La modélisation s'effectue sur la base d'un algorithme évolutionniste, qui ne conserve que les systèmes les plus performants.

## Utilisation du projet

Rien n'a vraiment été implémenté, il faudra utiliser à l'avenir pour exécuter le projet
```
dune exec main.ml
```


## Cadre de la simulation

### L'espace de simulation

La simulation se déroule au sein d'un carré fini de taille $n$, constitué de cases repérés par des coordonnées entières positives. Les villages sont initialement séparés par une distance $d$ arbitraire. L'espace de simulation est constitué de différents biomes aux paramètres d'évolution distincts. Un biome est un *chunk* de taille $m$ (*À adapter selon* $n$ *et* $d$).

Par exemple, une forêt sera peu vulnérable aux sécheresses mais aura un taux de mortalité plus élevé à cause de la faune locale.

### Mode de génération des biomes

Au départ de la simulation, l'algorithme génère deux [bruits de Perlin](https://fr.wikipedia.org/wiki/Bruit_de_Perlin) de taille $\frac{n}{m}$ puis les combine afin d'obtenir deux valeurs $(h, c)$ par chunk. La première valeur $h$ est un indicateur d'humidité locale, tandis que la deuxième valeur $c$ est un indicateur de chaleur. Les biomes sont définis par le tableau ci-dessous. La valeur en abscisse est $h$, tandis que la valeur en ordonnée est $c$.

|                 | **0 (sec)**  | **1 (tempéré)** | **2 (humide)** |
|-----------------|--------------|-----------------|----------------|
| **0 (froid)**   | désert froid | montagne        | tundra         |
| **1 (tempéré)** | steppe       | plaine          | forêt          |
| **2 (chaud)**   | désert chaud | savane          | jungle         |

### Villages

(*En cours de réécriture*)

## Bâtiments

Les **bâtiments** sont organisés en **villages**. Chaque bâtiment nécessite un certain nombre d'habitants pour son fonctionnement. Les habitants ont besoin lors de chaque tour pour survivre d'une ressource **logement** et d'une ressource **nourriture**. Un bâtiment est détruit s'il n'y a pas assez d'habitants pour le maintenir. Certains bâtiments ont aussi besoin de **matières premières** pour produire les ressources associés lors du tour.

### Types de bâtiments

- **Maison**: fait apparaître 5 habitants dans le village, emploie 0.25 personnes pour sa maintenance. A besoin de 5 nourriture par tour.
- **Champ**: fait apparaître 5 grain dans le village, emploie 2 personnes. Le grain doit être transmis au moulin pour être transformé en nourriture.
- **Moulin**: fait apparaître 5 nourriture dans le village, emploie 2 personnes. Un nourriture permet de faire survivre une personne pour un tour.
- **Mine**: fait apparaître 5 minerais dans le village, emploie 2 personnes. Le minerai doit être transmis à une usine pour être transformée en machines.
- **Usine**: booste la production des bâtiments adjacents, utilise autant de minerais que de bâtiments boostés. Si il n'y a pas assez de ressources nécessaires, le bâtiment entre en pénurie et arrête de fonctionner. Un bâtiment boosté voit sa production doublé, mais ses besoins en matières premières doublent aussi. Une usine peut également être boosté. Une usine boosté booste des bâtiments en multipliant leurs productions par 2,5.

### Tableau récapitulatif

|        |  production    |  emploi  | matières premières |
|--------|----------------|----------|--------------------|
| Maison | 20 logement    |   1      |  5 nourriture      |
| Champ  | 20 grain       |   8      |  -                 |
| Moulin | 20 nourriture  |   8      |  5 grain           |
| Mine   | 20 minerai     |   8      |  -                 |
| Usine  | < 9 boosts     |   20     |  = nb de boosts    |

*Disclaimer*: Les valeurs sont indicatives et varient selon les environnements
