# ---
# jupyter:
#   jupytext:
#     custom_cell_magics: kql
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
        return 1 + max(tree_height(tree['fils_g']), tree_height(tree['fils_d']))


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
        fils_g = root["fils_g"]
        fils_d = root["fils_d"]
        stack.append((fils_g, depth + 1))
        stack.append((fils_d, depth + 1))
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
def ecrire_arbre(dict, nom,fi):
    fi.write(nom)
    fi.write('[')
    typ = dict['condition']['type']
    r1 = dict['condition']['ressource1']
    r2 = dict['condition']['ressource2']
    ing = dict['condition']['ing']
    x = dict['condition']['int']
    bat = dict['bat']
    fi.write(typ + '<br>' + r1+ ' ' + r2+ ' ' + ing+ ' '+str(x)+'<br>'+bat) 
    fi.write(']')
    fi.write(' ')
    if dict['fils_g'] != 'V' and dict['fils_d'] != 'V'  :
        fi.write('--> ') 
        fi.write(nom+'0;\n')
        fi.write(nom+' --> '+nom+'1;\n')
        ecrire_arbre(dict['fils_g'],nom+'0',fi)
        ecrire_arbre(dict['fils_d'],nom+'1',fi)
    elif dict['fils_g'] != 'V' :
        fi.write('--> ') 
        fi.write(nom+'0;\n')
        ecrire_arbre(dict['fils_g'],nom+'0',fi)
    elif dict['fils_d'] != 'V' :
        fi.write('--> ') 
        fi.write(nom+'1;\n')
        ecrire_arbre(dict['fils_d'],nom+'1',fi)
    else :
        fi.write(';\n')


# %%
def ecrire_arbrepos(dict, nom,fi):
    fi.write(nom)
    fi.write('[')
    l= dict['liste']
    for i in range(len(l)):
        b1 =l[i]['bat_org']
        b2 =l[i]['bat_comp']
        x  = l[i]['nb']
        boo=l[i]['bool']
        fi.write(b1 +' '+ b2 +' '+ str(x) +' '+ boo +'<br>') 
    fi.write(']')
    fi.write(' ')
    if dict['child'] != 'V' :
        fi.write('--> ') 
        fi.write(nom+'0;\n')
        ecrire_arbrepos(dict['child'],nom+'0',fi)
    else :
        fi.write(';\n')


# %%
def increment_excel_style(column_label: str) -> str:
    # Convertir la colonne en un numéro (base 26)
    column_number = 0
    for char in column_label:
        column_number = column_number * 26 + (ord(char.upper()) - ord('A') + 1)
    # Incrémenter le numéro
    column_number += 1
    # Convertir le numéro en colonne
    new_label = ""
    while column_number > 0:
        column_number -= 1
        new_label = chr(column_number % 26 + ord('A')) + new_label
        column_number //= 26
    return new_label


# %%
def construction_fichier(tree):
    fi = open('../arbres/arbres.md','w')
    fi.write('# Arbres \n')
    fi.write('```mermaid \n')
    fi.write('graph LR; \n')
    n = len(tree)
    cl='A'
    for i in range(n):
        ecrire_arbre(tree[i],cl,fi)
        cl = increment_excel_style(cl)    
    fi.write('```')



# %%
def construction_fichier_treepos(tree):
    fi = open('../arbres/arbrespos.md','w')
    fi.write('# Arbres \n')
    fi.write('```mermaid \n')
    fi.write('graph LR; \n')
    n = len(tree)
    cl='A'
    for i in range(n):
        ecrire_arbrepos(tree[i],cl,fi)
        cl = increment_excel_style(cl)    
    fi.write('```')


# %%
def score(data):
    l=[]
    for i in range(1,len(data)):
        l.append(data[i]['evaluation'])
    return l
def moyenne(liste):
    c=0
    n=0
    for i in range(len(liste)):
        for j in range(len(liste[i])):
            c+=liste[i][j]
            n+=1
    return c/n
def meilleur_score(liste):
    c=0 
    for i in range(len(liste)):
        for j in range(len(liste[i])):
            if c < liste[i][j] and liste[i][j] < 2000  :
                c= liste[i][j]
    return c
def pire_score(liste):
    c=liste[1][1]
    for i in range(len(liste)):
         for j in range(len(liste[i])):
            if c > liste[i][j] and liste[i][j] >= 0 :
                c= liste[i][j]
    return c


# %%
def app(l,e):
    if len(l)<10:
        l.append(e)
        l.sort()
    else:
        if l[-1] < e: 
            l[-1]=e
            l.sort()
    return l 


# %%
def moy_best(liste):
    l=[]
    for i in range(len(liste)):
         for j in range(len(liste[i])):
            l= app(l,liste[i][j])
    c=0
    for i in range(len(l)):
       c += l[i]
    return c/10

def list_moy_best(liste):
    l=[]
    for i in range(len(liste)): 
            l.append(moy_best(liste[i]))
    return l


# %%
def list_moyenne(liste):
    l=[]
    for i in range(len(liste)):
        l.append(moyenne(liste[i]))
    return l
def list_meilleur(liste):
    l=[]
    for i in range(len(liste)):
        l.append(meilleur_score(liste[i]))
    return l
def list_pire(liste):
    l=[]
    for i in range(len(liste)):
        l.append(pire_score(liste[i]))
    return l


# %%
def visualisation(data):
    l = score(data)
    l1=list_moyenne(l)
    l2=list_meilleur(l)
    l3=list_pire(l)
    l5=list_moy_best(l)
    l4=range(len(data)-1)
    plt.plot(l4,l1,'b-')
    plt.plot(l4,l2,'rx')
    plt.plot(l4,l3,'g*')
    plt.plot(l4,l5,'y+')
    plt.xlabel("Generation")
    plt.ylabel("Score")


# %%
def reconstruction(data):
    tab=np.array(data[-1]['tree_array'])
    f= open('../test.json','w')
    f.write(json.dumps(list(tab)))
    f.close


# %%
f = open('../game.json', 'r')
data = np.array(json.loads(f.read()))
#tree = np.array(data[0]['tree_array'])
#construction_fichier(tree)
#treepos = np.array(data[-1]['treepos_array'])
#construction_fichier_treepos(treepos)
visualisation(data)
#reconstruction(data)

# %%

# %%
