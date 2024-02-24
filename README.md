---
title: "TIPE - Eric et Sylvain"
geometry: margin = 2cm
---

**Problématique**: *Comment modéliser le développement des espaces productifs à travers une approche évolutionniste*

# Principe

Le but de ce TIPE est d'étudier l'évolution d'un système semi-urbain à travers un modèle simplifié. La modélisation s'effectue sur la base d'un algorithme évolutionniste, qui ne conserve que les systèmes les plus performants.

## Utilisation du projet

Rien n'a vraiment été implémenté, il faudra utiliser à l'avenir pour exécuter le projet
```
dune exec main.ml
```

## Cadre de la simulation

La simulation se déroule au sein d'un carré fini (appelée carte) de coté $L$, constitué de cases repérés par des coordonnées entières positives. Elle est découpée en chunks notés de 4x4 cases. On note $c_{i,j}$ le chunk contenant la case de coordonnée $(4i, 4j)$. Autrement dit, $c_{i,j}$ est le $i$-eme chunk horizontal et le $j$-eme chunk vertical.

# Biomes, altitude et accessibilité

## Définition d'un biome

On associe à chaque chunk un biome *(cf. Mode de génération des biomes)* qui modifie la production des bâtiments, l'accessibilité et les aléas qui peuvent frapper la région. Chaque biome possède aussi une altitude qui modifie le coût de construction des bâtiments, l'accessibilité et les distances entre les villes.

Par exemple, une forêt sera peu vulnérable aux sécheresses mais aura un taux de mortalité plus élevé à cause de la faune locale.
On ne pourra pas produire de grain dans un désert froid ou chaud mais on pourra y implanter des mines qui auront une production accrue.

## Mode de génération des biomes

Au départ de la simulation, l'algorithme génère deux [bruits de Perlin](https://fr.wikipedia.org/wiki/Bruit_de_Perlin) de taille $\frac{L}{4}$ puis les combine afin d'obtenir deux valeurs $(h, q)$ par chunk. La première valeur $h$ est un indicateur d'humidité locale, tandis que la deuxième valeur $q$ est un indicateur de chaleur. Les biomes sont définis par le tableau ci-dessous. La valeur en abscisse est $h$, tandis que la valeur en ordonnée est $q$.
On considère que $h$ et $q$ sont des grandeurs adimentionné.

|                 | **0 (sec)**  | **1 (tempéré)** | **2 (humide)** |
|-----------------|--------------|-----------------|----------------|
| **0 (froid)**   | désert froid | tundra          | taïga          |
| **1 (tempéré)** | steppe       | plaine          | forêt          |
| **2 (chaud)**   | désert chaud | savane          | jungle  |

## Altitude

L'altitude d'un chunk $c$ est notée $z(c)$ et est générée à partir d'un bruit de Perlin de façon similaire aux biomes.

On pose $z_r(c)$ l'altitude relative du chunk $c$, ç-à-d la différence de l'altitude du chunk et de la moyenne des altitudes des 8 chunks adjacents telle que:

$$z_r(c) = z(c) - \frac{1}{8} \sum_{c_a\text{ adjacent}} z(c_a)$$

On pose aussi $\overline{z_r}$ l'altitude relative moyenne de la carte et $\sigma$ l'écart-type de l'altitude relative de la carte.

$$ \overline{z_r} = \frac{1}{N^2} \sum_{0 \le i, j < N} z_r(c_{i,j}) \quad \text{et} \quad \sigma = \sqrt{\frac{1}{N^2}\left(\sum_{0 \le i,j < N}{(z_r(c_{i,j}))^2}\right) - \overline{z_r}^2}$$

## Coefficient d'hostilité environnementale

Pour tout chunk $c$, on note $\mathcal{H}(c)$ le coefficient d'hostilité environnementale de $c$ définie selon $h(c)$ et $q(c)$, respectivement l'humidité et la chaleur du chunk $c$ tel que

$$\mathcal{H}(c) = (|h(c) - 1| + 1) (|q(c) - 1| + 1)$$

**Remarque**: On a $0 \le h, q \le 2$, donc $\mathcal{H} \in \{1,2,4\}$

## Coefficient d'accessibilité

Le coefficient d'accessibilité ou accessibilité d'un chunk $c$ est noté $\mathcal{A}(c)$ et est calculée à partir de la formule suivante

$$\mathcal{A}(c) = \mathcal{H} \frac{|\overline{z_r} - z_r(c)|}{\sigma}$$

**Remarque**: $\frac{|\overline{z_r} - z_r(c)|}{\sigma}$ représente l'éloignement de $z_r(c)$ par rapport à $\overline{z_r}$

# Villages

## Concept

Au début de la simulation, l'algorithme place des villages tout les $d_0$ chunks. Un village qui possède au moins un bâtiment dans une des cases d'un chunk possède celui-ci.

Chaque village possède des ressources qu'elle peut stocker indéfiniment sans limite de quantité. Chaque village est possède également une population $p$ qui varie.

Le village peut effectuer $n$ actions de façon autonome, rassemblés dans une famille $(\alpha_i)_{i < n} \in \mathbb{R}^n$ de taille $n$.

## Matrice de décision, vecteur d'état et vecteur de décision

Les informations propres aux villages sont stockés dans un vecteur d'état $r(r_1, ..., r_n)$ avec $n$ le nombre de ressources stockés et pour tout $i \in [\![ 0, n ]\!]$, $r_i$ la quantité de la $i$-ème ressource stockée, en comptant la population comme une ressource.

Pour $m \in \mathbb{N}$, on pose $\Delta(\delta_1, ..., \delta_m) \in \mathbb{R}^m$ le vecteur décision.

$\forall i \in [\![ 0, m ]\!]$, si $\delta_i > 1$, alors la décision $\alpha_i$ sera prise par le village.

Chaque village possède aussi une matrice décisionnelle $D$ qui lui permet de prendre des décisions de façon autonome. $\mathcal{D}$ est généré de façon aléatoire en début de simulation.

Ces grandeurs sont reliés par la relation:

$$r \times D = \Delta$$

D'où

$$
    \begin{pmatrix}
        r_1 \\
        \vdots \\
        r_n
    \end{pmatrix}
    \begin{pmatrix}
        D_{0,0} \ \ldots \ D_{n,m} \\
        \vdots \ \ddots \ \vdots \\
        D_{n, 0} \ \ldots \ D_{n,m}
    \end{pmatrix}
    = \begin{pmatrix}
        \delta_1 \\
        \vdots \\
        \delta_n
    \end{pmatrix}
$$

## Bâtiments

Un village est constitué de bâtiments. Chaque bâtiment nécessite un certain nombre d'habitants pour son fonctionnement. Les habitants ont besoin lors de chaque tour pour survivre d'une ressource **logement** et d'une ressource **nourriture**. Un bâtiment est détruit s'il n'y a pas assez d'habitants pour le maintenir. Certains bâtiments ont aussi besoin de **matières premières** pour produire les ressources associés lors du tour.

### Mode d'apparition

Au début de la simulation, l'algorithme muni chaque village de $4$ bâtiments. Le village est également muni d'actions  de construction géré par la matrice de décisions. Construire un bâtiment requiert de d'acquitter son coût de construction associé.

### Types de bâtiments

- **Maison**: fait apparaître 20 habitants dans le village.
- **Champ**: fait apparaître 20 grain dans le village, emploie 8 personnes. Le grain doit être transmis au moulin pour être transformé en nourriture.
- **Moulin**: fait apparaître 20 nourriture dans le village, emploie 4 personnes. Un nourriture permet de faire survivre une personne pour un tour.
- **Mine**: fait apparaître 20 minerais dans le village, emploie 8 personnes. Le minerai doit être transmis à une usine pour être transformée en machines.
- **Usine**: booste la production des bâtiments du village par $1.1$, utilise 8 minerais. Emploie 4 personnes.

## Tableau récapitulatif

|        |  production   | emploi | matières premières | coût de construction |
|--------|---------------|--------|--------------------|----------------------|
| Maison | 20 logement   |  0     |  -                 | 20 grain             |
| Champ  | 20 grain      |  8     |  -                 | 40 nourriture        |
| Moulin | 20 nourriture |  4     |  10 grain          | 60 grain             |
| Mine   | 20 minerai    |  8     |  -                 | 40 minerais          |
| Usine  | $\times$ 1.1  |  4     |  20 minerais       | 80 minerais          |

*Disclaimer*: Les valeurs sont indicatives et varient selon les environnements
