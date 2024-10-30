# ---
# jupyter:
#   jupytext:
#     formats: py:percent
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.16.4
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
f = open('../test_game.json', 'r')
json_data = json.loads(f.read())[0]
map_data = json_data['map']
villages = json_data['villages']
chunk_width = get_chunk_width(map_data)

biome_map = get_biomes(map_data)
z_map = get_map_z(map_data)
extent = 0, get_map_size(map_data), get_map_size(map_data), 0

for village in villages:
    pos = village['position']
    plt.plot(chunk_width * pos['x'], chunk_width * pos['y'], marker='x', color="red") 

plt.imshow(z_map, cmap="grey", extent=extent)
plt.imshow(biome_map, extent=extent, alpha=0.3)
# [(village['position']['x'] * chunk_width, village['position']['y'] * chunk_width) for village in villages]
np.array([tree_height(village['tree']) for village in villages])
