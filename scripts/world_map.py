#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Ce programme transforme des données de références de contours des pays pour avoir une carte exploitable
pour Screenjob

@author: Alexis Eidelman
"""

import os
import pandas as pd
import geopandas as gp
from shapely.ops import cascaded_union

# Téléchargé depuis https://unstats.un.org/unsd/methodology/m49/overview/
noms = pd.read_csv('../inputs/immigration/UNSD — Methodology.csv')
# Téléchargé depuis https://ec.europa.eu/eurostat/fr/web/gisco/geodata/reference-data/administrative-units-statistical-units/countries#countries20
# © EuroGeographics pour les limites administratives
plan = gp.read_file("../inputs/immigration/ref-countries-2020-60m/CNTR_RG_60M_2020_4326.geojson")

plan = plan[['ISO3_CODE', 'geometry']].merge(noms[['ISO-alpha3 Code', 'Region Name', 'Sub-region Name', 'Country or Area']],
                right_on = "ISO-alpha3 Code", left_on='ISO3_CODE')

plan['final'] = plan['Region Name']
détail = plan['Country or Area'].isin(["France", "Espagne", "Italie", "Portugal",
          "Algérie", "Maroc", "Tunisie", "Turquie", "Chine"])
plan.loc[détail, 'final'] = plan.loc[détail, 'Country or Area']
plan.loc[plan['Country or Area'].isin(["Cambodge", 'République démocratique populaire lao', 'Viet Nam']), "final"] = "Cambodge, Laos, Vietnam"

plan.loc[plan['Country or Area'].isin(
    ["Cambodge", 'République démocratique populaire lao', 'Viet Nam']), "final"] = "Cambodge, Laos, Vietnam"


plan['final'].replace('Europe', 'Europe hors Union Européenne', inplace=True)

autre_UE28 = ["Allemagne", 'Autriche', 'Belgique', 'Bulgarie', 'Chypre',
              'Croatie', 'Danemark', 'Estonie', 'Finlande', 'Grèce', 'Hongrie',
              'Irlande', 'Lettonie', 'Lituanie', 'Luxembourg', 'Malte',
              'Pays-Bas', 'Pologne', 'Tchéquie', 'Roumanie',
              'Slovaquie', 'Slovénie', 'Suède',
              "Royaume-Uni de Grande-Bretagne et d’Irlande du Nord"]
plan.loc[plan['Country or Area'].isin(autre_UE28), "final"] = "Union européenne hors Espagne, Italie et Portugal"


données_insee = pd.read_excel("../inputs/immigration/chiffres_secteurs_metiers - envoi 2.xls",
    sheet_name="FAP87-Immigrés",
    skiprows=2)
données_insee.drop(["Immigrés", 
                    "Familles et domaines professionnelles agrégées\n (87 positions et 22 sous-totaux)",
                    "Total Europe", "Total Afrique", "Total \nAsie", "Total Amerique, Oceanie"], axis=1, inplace=True)
données_insee.rename(columns={
        'Unnamed: 0': "FAP",
        
        },
    inplace=True)

données_insee = données_insee[données_insee['FAP'].str.len() == 3]

carte = plan.groupby('final').apply(lambda x: x.geometry.unary_union)

gp.GeoSeries(carte).to_file("../inputs/immigration/carte_du_monde.geojson",
                            driver='GeoJSON',
                            encoding='utf8')

