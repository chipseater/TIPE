# Principe

Le but de ce TIPE est d'étudier l'évolution d'un système urbain à travers un modèle simplifié. La modélisation s'effectue sur la base d'un algorithme évolutionniste, qui ne conserve que les systèmes les plus performants et les reproduit.

# Utilisation

## Instalation des dépendances

Il faut tout d'abord installer `opam` avec le package manager de sa distribution, puis lancer `opam init`. Il faut ajouter ensuite `eval (opam env)` à son `.bashrc` (ou assimilé) pour initialiser l'environement opam à chaque session.

Pour installer les dépendances requises, il faut lancer

```
opam install dune yojson domainslib
```

`dune` est l'outil utilisé pour gérer les projets ocaml, `yojson` est la bibliothèque qui permet d'utiliser des fichiers en `.json` pour stocker l'état du jeu.

## Lancement du projet

Pour lancer le projet, il faut utiliser

```
dune exec TIPE
```

## Visualisations

Pour générer des visuels à partir d'un fichier `.json`, il faut avoir une installation python capable de lancer des notebook jupyter, j'utilise pour ma part `pyenv`.

### Pour installer `pyenv` et les dépendances

Pour installer `pyenv`, on peut utiliser le script d'installation automatique

```
curl https://pyenv.run | bash
```

Puis il faut ajouter ces lignes à son `~/.bashrc`

```
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"

eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
```

Il faut ensuite installer python en utilisant `pyenv` après avoir redémarré son shell

```
pyenv install 3.12.4
```

Il faut ensuite créer un environnement virtuel en se plaçant dans le répertoire `jupyter/` du projet

```
pyenv virtualenv visualisation
pyenv activate visualisation
```

On installe enfin les dépendances nécessaires avec

```
pip install jupyter jupytext numpy matplotlib
```

### Utilisation du notebook

Pour lancer le notebook, il faut d'abord activer l'environnement virtuel en se plaçant dans `jupyter/`, puis démarrer jupyter
```
pyenv activate visualisation
jupyter notebook
```

Pour l'ouvrir, il faut aller sur `localhost:8888` et ouvrir `visualization.py` en tant que notebook.

![Screenshot pour ouvrir le notebook](./images/open_with_notebook.png)

# Documentation

La documentation ci-n'est plus tellement à jour, elle avait été écrite comme une spécification qui n'a été que partiellement suivie. 

