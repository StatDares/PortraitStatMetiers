# -*- coding: utf-8 -*-
"""
appellations par code 
Modifie le format kewords, utile pour le sélecteur de Fap
"""


import pandas as pd

appellations = pd.read_csv('../inputs/unix_referentiel_appellation_v344_utf8.csv',
                           encoding='utf8')
appellations = appellations[["code_rome", "libelle_appellation_long"]]
rome_fap = pd.read_csv('../inputs/fap225_rome.csv', sep=';', encoding='cp1252')
rome_fap.loc["new_line"] = ['U1Z82','L1510','']
appellations = appellations.merge(rome_fap, how='left',
                                  left_on='code_rome', right_on='Rome',
                                  indicator=True)

appellations['libelle_appellation_long'] = appellations['libelle_appellation_long'] + ','
appellations['Fap87'] = appellations['Fap'].str[:3]
appellations.rename({'Fap': 'fap'}, axis=1, inplace=True)

appellations_225 = appellations.groupby('fap')['libelle_appellation_long'].apply(sum)
appellations_87 = appellations.groupby('Fap87')['libelle_appellation_long'].apply(sum)
appellations_87.index.name = "fap"


appellations_225 = appellations_225.str[:-1]
appellations_87 = appellations_87.str[:-1]

appellations_225.to_csv('../inputs/keywords/appellations_fap225.csv',
                            sep=';', encoding='utf8')
appellations_87.to_csv('../inputs/keywords/appellations_fap87.csv',
                       sep=';', encoding='utf8')


# pour_description = appellations[["code_rome", "libelle_appellation_long"]]
# pour_description.columns = ["rome", "appellations"]
# pour_description.to_csv('../inputs/keywords/appellations_rome.csv',
#                             sep=';', encoding='utf8',
#                             index=False)