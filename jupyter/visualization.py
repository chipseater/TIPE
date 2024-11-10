# ---
# jupyter:
#   jupytext:
#     custom_cell_magics: kql
#     formats: py:percent
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.16.3
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# %%
import numpy as np
import matplotlib.pyplot as plt
import json

# %%
def char_to_int(char):
    if char == 'D':
        return 3
    if char == 'P':
        return 2
    if char == 'F':
        return 1

# %%
def get_chunk_width(map):
    return len(map[0][0]['tiles'])

def get_map_size(map):
    return len(map) * get_chunk_width(map)

def get_tile(map, i, j, n):
    n = get_map_size(map)
    chunk_width = get_chunk_width(map)
    chunk = map[i // chunk_width][j // chunk_width]['tiles']
    return chunk[i % chunk_width][j % chunk_width]


# %%
def get_chunk_z(chunk):
    chunk_z = []
    for tile_row in chunk["tiles"]:
        chunk_z.append([tile['z'] for tile in tile_row])
    return np.array(chunk_z)

def get_map_z(map):
    n = get_map_size(map)
    z_map = []
    for i in range(n):
        z_row = []
        for j in range(n):
            z_row.append(get_tile(map, i, j, n)['z'])
        z_map.append(z_row)
    return np.array(z_map)


# %%
def get_biomes(map):
    chunk_width = get_chunk_width(map)
    n = get_map_size(map) // chunk_width
    biome_map = []
    for i in range(n):
        biome_row = []
        for j in range(n):
            biome_row.append(char_to_int(map[i][j]['biome']))
        biome_map.append(biome_row)
    return np.array(biome_map)


# %%
def tree_height(tree):
    if tree == 'V':
        return 0
    else:
        return 1 + max(tree_height(tree['l_child']), tree_height(tree['r_child']))


# %%
# Fonction d'affichage des noeuds
def node_to_symbol(node):
    if node != 'V':
        return node['action']['bat']
    return 'V'

# À l'aide d'un parcours en largeur, répartit les noeuds par lignes de profondeur
def get_lines(tree : dict):
    # Une pile d'appel qui contient les noeuds avec leur profondeur
    stack = [(tree, 0)]
    lines = [""] * (tree_height(tree) + 1)
    while stack != []:
        root, depth = stack.pop()
        # Pour une raison inconnue, ça marche seulement quand j'utilise des strings à la place de listes
        lines[depth] += node_to_symbol(root) + ','
        if root == 'V':
            continue
        l_child = root["l_child"]
        r_child = root["r_child"]
        stack.append((l_child, depth + 1))
        stack.append((r_child, depth + 1))
    # Convertit les chaînes en listes
    return list(map(lambda x: x[:-1].split(','), lines))


# %%
def get_paddding_width(height, depth, string_width):
    return string_width * (2 ** (height - depth) - 1)

def print_tree_lines(tree):
    lines = tree
    height = len(lines)
    for i in range(height):
        line = lines[i]
        string_width = len(line[0])
        padding_width = get_paddding_width(height, i + 1, string_width)
        sep_width = get_paddding_width(height, i, string_width) 
        padding = padding_width * " "
        separator = sep_width * " "
        formatted_line = padding + separator.join(line)
        print(" ".join(formatted_line))

# %%
f = open('../game.json', 'r')
data = np.array(json.loads(f.read()))
tree = np.array(data[0]['tree_array'])[10]
print_tree_lines(get_lines(tree))

