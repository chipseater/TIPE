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

La simulation se déroule au sein d'un carré fini (appelée carte) de coté $L$, constitué de cases repérés par des coordonnées entières positives. Elle est découpée en chunks notés de 4x4 cases. On note $c_{i,j}$ le chunk contenant la case de coordonée $(4i, 4j)$. Autrement dit, $c_{i,j}$ est le $i$-eme chunk horizontal et le $j$-eme chunk vertical.

## Biomes, altitude et accessibilité

### Définition d'un biome

On associe à chaque chunk un biome *(cf. Mode de génération des biomes)* qui modifie la production des batiments, l'accessibilité et les aléas qui peuvent frapper la région. Chaque biome possède aussi une altitude qui modifie le coût de construction des batiments, l'accessibilité et les distances entre les villes.

Par exemple, une forêt sera peu vulnérable aux sécheresses mais aura un taux de mortalité plus élevé à cause de la faune locale.
On ne pourra pas produire de grain dans un désert froid ou chaud mais on pourra y implanter des mines qui auront une production accrue.

### Mode de génération des biomes

Au départ de la simulation, l'algorithme génère deux [bruits de Perlin](https://fr.wikipedia.org/wiki/Bruit_de_Perlin) de taille $\frac{L}{4}$ puis les combine afin d'obtenir deux valeurs $(h, q)$ par chunk. La première valeur $h$ est un indicateur d'humidité locale, tandis que la deuxième valeur $q$ est un indicateur de chaleur. Les biomes sont définis par le tableau ci-dessous. La valeur en abscisse est $h$, tandis que la valeur en ordonnée est $q$.
On considère que $h$ et $q$ sont des grandeurs adimentionés.

|                 | **0 (sec)**  | **1 (tempéré)** | **2 (humide)** |
|-----------------|--------------|-----------------|----------------|
| **0 (froid)**   | désert froid | tundra          | taïga          |
| **1 (tempéré)** | steppe       | plaine          | forêt          |
| **2 (chaud)**   | désert chaud | savane          | jungle  |

### Fonction sigmoïde

On pose pour tout $m \in \mathbb{R}$ la fonction $\sigma_m: \mathbb{R} \to \left]0, 1 \right[$ telle que

$$\forall x \in \mathbb{R}, \sigma_m(x) = \frac{1}{1 + e^{m-x}}$$

Cette fonction strictement croissante "contracte" toutes les valeurs de $\mathbb{R}$ dans $\left]0, 1 \right[$ et associe à l'antécédent $m$ la valeur $\frac{1}{2}$. 

Ainsi, si on pose pour $n \in \N$ la famille $(x_1, ..., x_n) \in \R^n$ et $x_0$ sa valeur moyenne, il est préférable de choisir $m$ tel que $m = x_0$ pour que les valeurs supérieures à $x_0$ soient supérieures à $\frac{1}{2}$et les valeurs inférieure à $x_0$ soient inférieures à $\frac{1}{2}$.

[En savoir plus](https://fr.wikipedia.org/wiki/Sigmo%C3%AFde_(math%C3%A9matiques))

### Altitude

L'altitude d'un chunk $c$ est notée $z(c)$ et est générée à partir d'un bruit de Perlin de façon similaire aux biomes.

On pose $z_r(c)$ l'altitude relative du chunk $c$, ç-à-d la différence de l'altitude du chunk et de la moyenne des altitudes des 8 chunks adjacents telle que:

$$z_r(c) = z(c) - \frac{1}{8} \sum_{c_a\text{ adjacent}} z(c_a)$$

On pose aussi $z_{\text{rmc}}$ l'altitude relative moyenne de la carte définie par

$$z_{\text{rmc}} = \frac{1}{L}\sum_{0 \le i,j \le L} z_r(c_{i,j})$$

### Accessibilité environnementale

Pour tout biome $b$, on note $e_b$ l'accessibilité environnementale de $b$ définie selon $h_b$ et $q_b$, respectivement l'humidité du biome $b$ et sa chaleur tel que

$$e_b = (|h - 1| + 1) (|q - 1| + 1)$$

**Remarque**: On a $0 \le h, q \le 2$, donc $e_b \in \{1,2,4\}$ et $[e_b] = 1$

### Coeficient d'accessibilité

Le coeficient d'accessibilité d'un chunk $c$ est noté $a(c)$ et est calculée à partir de la formule suivante

$$a_c = e_b \times \sigma_{z_{\text {rmc}}}\left(\frac{z_r(c)}{z_{\text{rmc}}}\right)$$

**Remarques**:
- On a $0 \le a_c \le 4$
- Si $z_r(c) = z_{\text{rmc}}$, alors $a_c = e_b$
- L'accessibilité est adimentionné, en effet $[z_r(c)] = [z_{\text{rmc}}] = \text{L}$, donc $\left[\frac{z_r(c)}{z_{\text{rmc}}}\right] = 1$, d'où $[a_c] = 1$

## Villages

Au début de la simulation, l'algorithme génère des permutations aléatoires de villages et les place sur la grille de simulation. Ensuite, l'algorithme crée un graphe d'adjacence: chaque village est un noeud connecté à ses voisins ayant des bâtiments situés à $d$ de leur épicentre.

### Bâtiments

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
