# -*- coding: utf-8 -*-
"""
Fichier pour convertir les données issues de la chaine newton :
    en un unique fichier pour tous les niveaux de fap
    simplifié avec moins de colonne
    avec une seule année
    
Source des données :  https://dares.travail-emploi.gouv.fr/publications/les-tensions-sur-le-marche-du-travail-en-2019
"""

import os
from os.path import dirname, join
import pandas as pd

parent_folder = dirname(dirname(__file__))
tension_inputs = join(parent_folder, 'inputs', 'tensions')

excel = pd.ExcelFile(join(tension_inputs, "dares_donnees_tensions_2019_corrige.xlsx"))
excel.sheet_names

columns_to_use = ['ind_tension_d', "ind_int_emb_d",
                  "ind_main_oeuvre_d", "ind_dur_empl_d",
                  "ind_cond_travail_d", "ind_mismatch_geo_d",
                  "ind_specif_form_empl_d"
                  ]

columns_to_use = ['Emploi moyen', 
                  'Tension ',
                  'Tension - discret',
                  "Intensité d'embauches - discret",
                  'Lien formation-emploi - discret',
                  "Manque de main-d'œuvre disponible - discret",
                  "Non-durabilité de l'emploi - discret",
                  "Conditions de travail contraignantes - discret",
                  'Inadéquation géographique - discret',
                  'Année'
       ]

sheets_225 = [x for x in excel.sheet_names if "225" in x]
sheets_87 = [x for x in excel.sheet_names if "87" in x]
all_data = pd.DataFrame()

for sheet_name in sheets_225 + sheets_87:
    niveau_geo = sheet_name.split("x ")[1].lower()
    niveau_fap = sheet_name.split(" x")[0].lower()
    print(niveau_fap, niveau_geo)
    tab = excel.parse(sheet_name)
    # tab = tab[tab['Année'] == 2019]
    colonne_geo = tab.columns[3]
    columns = [tab.columns[1]] + columns_to_use
    print(tab.columns[[1,3]].tolist())
    if niveau_geo != "nat":
        columns = tab.columns[[1,3]].tolist() + columns_to_use
        columns.append('Tension (Valeurs imputées: volumétrie insuffisantes)')
        columns.remove('Tension ')
    tab = tab[columns]
    assert ('Code FAP 87' in tab.columns) or ('Code FAP 225' in tab.columns)
    tab.rename(
        {'Tension (Valeurs imputées: volumétrie insuffisantes)': "Tension ",
         'Tension - discret': "Catégorie Tension",
         'Code FAP 87': 'fap',
         'Code FAP 225': 'fap',
         'Code région': 'reg',
         'Code département': 'dep',
         'reg ': 'reg'
         },
        axis=1, errors='ignore', inplace=True)

    tab.columns = [x.replace(' - discret', '').replace(' -discret', '').replace('-discret', '') for x in tab.columns]

    tab['niveau_geo'] = niveau_geo
    tab['niveau_fap'] = niveau_fap
    all_data = all_data.append(tab)

from numpy import nan
all_data.replace('n.d.', nan, inplace=True)
all_data['niveau_geo'].replace('reg ', 'reg', inplace=True)

all_data = all_data[all_data['Emploi moyen'].notnull()]
all_data['Emploi moyen'] = all_data['Emploi moyen'].round(-2).astype(int)

data_2019 = all_data[all_data['Année'] == 2019]

data_2019.to_csv(join(tension_inputs, "tensions.csv"), encoding='utf8', index=False)

evolution_tensions = all_data.loc[all_data['niveau_geo'] == 'nat', ['fap', 'Tension ', 'Année']]

evolution_tensions.to_csv(join(tension_inputs, "evol_tensions.csv"),
                          encoding='utf8', index=False)

# test des valeurs dans une seule fap :
tt = all_data.groupby('fap')['Conditions de travail contraignantes'].nunique()
tt[tt > 1]
tt = all_data.groupby('fap')['Lien formation-emploi'].nunique()
tt[tt > 1]