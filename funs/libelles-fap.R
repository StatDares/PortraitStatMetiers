
#' Table des codes FAP, domaines et libelles
#'
#' @return Un \code{data.table}
#' @export
#'
#' @examples
#' 
#' libelles_fap()
#' 
libelles_fap <- function() {
  # niv_fap <- read_excel("inputs/Fichiers excel PSM 2019/Niveaux_fap_2009.xlsx")
  # niv_fap <- as.data.table(niv_fap)
  # niv_fap22 <- niv_fap[, .(fap = unique(code_fap22),
  #               libelle = unique(libelle_fap22))]
  # niv_fap87 <- niv_fap[, .(fap = unique(code_fap87),
  #               libelle = unique(libelle_fap87))]
  # niv_fap225 <- niv_fap[, .(fap = unique(code_fap225),
  #               libelle = unique(libelle_fap225))]
  # fap_codes <- rbind(niv_fap22, niv_fap87, niv_fap225)
  
  fap_codes <- data.table(
    fap = c("A", "B", "C", "D", "E", "Y", "F", "G", "H", "J", "K", "L", 
            "M", "N", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Z", "A0Z", 
            "A1Z", "A2Z", "A3Z", "B0Z", "B1Z", "B2Z", "B3Z", "B4Z", "B5Z", 
            "B6Z", "B7Z", "C0Z", "C1Z", "C2Z", "D0Z", "D1Z", "D2Z", "D3Z", 
            "D4Z", "D6Z", "E0Z", "E1Z", "E2Z", "ENS", "F0Z", "F1Z", "F2Z", 
            "F3Z", "F4Z", "F5Z", "G0A", "G0B", "G1Z", "H0Z", "J0Z", "J1Z", 
            "J3Z", "J4Z", "J5Z", "J6Z", "K0Z", "L0Z", "L1Z", "L2Z", "L3Z", 
            "L4Z", "L5Z", "L6Z", "M0Z", "M1Z", "M2Z", "N0Z", "P0Z", "P1Z", 
            "P2Z", "P3Z", "P4Z", "Q0Z", "Q1Z", "Q2Z", "R0Z", "R1Z", "R2Z", 
            "R3Z", "R4Z", "S0Z", "S1Z", "S2Z", "S3Z", "T0Z", "T1Z", "T2A", 
            "T2B", "T3Z", "T4Z", "T6Z", "U0Z", "U1Z", "V0Z", "V1Z", "V2Z", 
            "V3Z", "V4Z", "V5Z", "W0Z", "W1Z", "X0Z", "ZZZ", "A0Z00", "A0Z01", 
            "A0Z02", "A0Z40", "A0Z41", "A0Z42", "A0Z43", "A1Z00", "A1Z01", 
            "A1Z40", "A1Z41", "A1Z42", "A2Z70", "A2Z90", "A3Z00", "A3Z40", 
            "A3Z41", "A3Z90", "B0Z20", "B0Z21", "B1Z40", "B2Z40", "B2Z41", 
            "B2Z42", "B2Z43", "B2Z44", "B3Z20", "B4Z41", "B4Z42", "B4Z43", 
            "B4Z44", "B5Z40", "B6Z70", "B6Z71", "B6Z72", "B6Z73", "B7Z90", 
            "B7Z91", "C0Z20", "C1Z40", "C2Z70", "C2Z71", "C2Z80", "D0Z20", 
            "D1Z40", "D1Z41", "D2Z40", "D2Z41", "D2Z42", "D3Z20", "D4Z40", 
            "D4Z41", "D6Z70", "D6Z71", "D6Z80", "E0Z20", "E0Z21", "E0Z22", 
            "E0Z23", "E0Z24", "E1Z40", "E1Z41", "E1Z42", "E1Z43", "E1Z44", 
            "E1Z46", "E1Z47", "E2Z70", "E2Z80", "ENSEM", "F0Z20", "F1Z40", 
            "F1Z41", "F2Z20", "F3Z40", "F3Z41", "F4Z20", "F4Z41", "F5Z70", 
            "G0A40", "G0A41", "G0A42", "G0A43", "G0B40", "G0B41", "G1Z70", 
            "G1Z71", "G1Z80", "H0Z90", "H0Z91", "H0Z92", "J0Z20", "J1Z40", 
            "J1Z80", "J3Z40", "J3Z41", "J3Z42", "J3Z43", "J3Z44", "J4Z40", 
            "J4Z60", "J4Z80", "J5Z60", "J5Z61", "J5Z62", "J5Z80", "J6Z90", 
            "J6Z91", "J6Z92", "K0Z20", "K0Z40", "L0Z60", "L1Z60", "L2Z60", 
            "L2Z61", "L3Z80", "L4Z80", "L4Z81", "L5Z90", "L5Z91", "L5Z92", 
            "L6Z00", "L6Z90", "M0Z60", "M1Z80", "M1Z81", "M2Z90", "M2Z91", 
            "M2Z92", "N0Z90", "N0Z91", "P0Z60", "P0Z61", "P0Z62", "P1Z80", 
            "P1Z81", "P1Z82", "P2Z90", "P2Z91", "P2Z92", "P3Z90", "P3Z91", 
            "P4Z60", "P4Z61", "P4Z80", "Q0Z60", "Q1Z80", "Q1Z81", "Q2Z90", 
            "Q2Z91", "R0Z60", "R0Z61", "R1Z60", "R1Z61", "R1Z62", "R1Z63", 
            "R1Z66", "R1Z67", "R2Z80", "R2Z83", "R3Z80", "R3Z81", "R3Z82", 
            "R4Z90", "R4Z91", "R4Z92", "R4Z93", "S0Z20", "S0Z40", "S0Z41", 
            "S0Z42", "S1Z20", "S1Z40", "S1Z80", "S2Z60", "S2Z61", "S2Z80", 
            "S2Z81", "S3Z00", "S3Z90", "T0Z60", "T1Z60", "T2A60", "T2B60", 
            "T3Z60", "T3Z61", "T4Z60", "T4Z61", "T4Z62", "T6Z61", "U0Z80", 
            "U0Z81", "U0Z90", "U0Z91", "U0Z92", "U1Z80", "U1Z81", "U1Z82", 
            "U1Z91", "U1Z92", "U1Z93", "V0Z60", "V1Z80", "V1Z81", "V2Z90", 
            "V2Z91", "V2Z92", "V2Z93", "V3Z70", "V3Z71", "V3Z80", "V3Z90", 
            "V4Z80", "V4Z83", "V4Z85", "V5Z00", "V5Z81", "V5Z82", "V5Z84", 
            "W0Z80", "W0Z90", "W0Z91", "W0Z92", "W1Z80", "X0Z00", "X0Z01", 
            "ZZZZZ"),
    libelle = c("Agriculture, marine, p\u00eache", "B\u00e2timent, travaux publics", 
                "\u00c9lectricit\u00e9, \u00e9lectronique", "M\u00e9canique, travail des m\u00e9taux", 
                "Industries de process", "Tous m\u00e9tiers", "Mat\u00e9riaux souples, bois, industries graphiques", 
                "Maintenance", "Ing\u00e9nieurs et cadres de l\'industrie", "Transports, logistique et tourisme", 
                "Artisanat", "Gestion, administration des entreprises", "Informatique et t\u00e9l\u00e9communications", 
                "\u00c9tudes et recherche", "Administration publique, professions juridiques, arm\u00e9e et police", 
                "Banque et assurances", "Commerce", "H\u00f4tellerie, restauration, alimentation", 
                "Services aux particuliers et aux collectivit\u00e9s", "Communication, information, art et spectacle", 
                "Sant\u00e9, action sociale, culturelle et sportive", "Enseignement, formation", 
                "Politique, religion", "Non renseign\u00e9 ou autre", "Agriculteurs, \u00e9leveurs, sylviculteurs, b\u00fbcherons", 
                "Mara\u00eechers, jardiniers, viticulteurs", "Techniciens et cadres de l\'agriculture", 
                "Marins, p\u00eacheurs, aquaculteurs", "Ouvriers non qualifi\u00e9s du gros \u0153uvre du b\u00e2timent, des travaux publics, du b\u00e9ton et de l\'extraction", 
                "Ouvriers qualifi\u00e9s des travaux publics, du b\u00e9ton et de l\'extraction", 
                "Ouvriers qualifi\u00e9s du gros \u0153uvre du b\u00e2timent", "Ouvriers non qualifi\u00e9s du second \u0153uvre du b\u00e2timent", 
                "Ouvriers qualifi\u00e9s du second \u0153uvre du b\u00e2timent", "Conducteurs d\'engins du b\u00e2timent et des travaux publics", 
                "Techniciens et agents de ma\u00eetrise du b\u00e2timent et des travaux publics", 
                "Cadres du b\u00e2timent et des travaux publics", "Ouvriers non qualifi\u00e9s de l\'\u00e9lectricit\u00e9 et de l\'\u00e9lectronique", 
                "Ouvriers qualifi\u00e9s de l\'\u00e9lectricit\u00e9 et de l\'\u00e9lectronique", "Techniciens et agents de ma\u00eetrise de l\'\u00e9lectricit\u00e9 et de l\'\u00e9lectronique", 
                "Ouvriers non qualifi\u00e9s travaillant par enl\u00e8vement ou formage de m\u00e9tal", 
                "Ouvriers qualifi\u00e9s travaillant par enl\u00e8vement de m\u00e9tal", "Ouvriers qualifi\u00e9s travaillant par formage de m\u00e9tal", 
                "Ouvriers non qualifi\u00e9s de la m\u00e9canique", "Ouvriers qualifi\u00e9s de la m\u00e9canique", 
                "Techniciens et agents de ma\u00eetrise des industries m\u00e9caniques", 
                "Ouvriers non qualifi\u00e9s des industries de process", "Ouvriers qualifi\u00e9s des industries de process", 
                "Techniciens et agents de ma\u00eetrise des industries de process", 
                "Tous m\u00e9tiers", "Ouvriers non qualifi\u00e9s du textile et du cuir", 
                "Ouvriers qualifi\u00e9s du textile et du cuir", "Ouvriers non qualifi\u00e9s du travail du bois et de l\'ameublement", 
                "Ouvriers qualifi\u00e9s du travail du bois et de l\'ameublement", 
                "Ouvriers des industries graphiques", "Techniciens et agents de ma\u00eetrise des mat\u00e9riaux souples, du bois et des industries graphiques", 
                "Ouvriers qualifi\u00e9s de la maintenance", "Ouvriers qualifi\u00e9s de la r\u00e9paration automobile", 
                "Techniciens et agents de ma\u00eetrise de la maintenance", "Ing\u00e9nieurs et cadres techniques de l\'industrie", 
                "Ouvriers non qualifi\u00e9s de la manutention", "Ouvriers qualifi\u00e9s de la manutention", 
                "Conducteurs de v\u00e9hicules", "Agents d\'exploitation des transports", 
                "Agents administratifs et commerciaux des transports et du tourisme", 
                "Cadres des transports, de la logistique et navigants de l\'aviation", 
                "Artisans et ouvriers artisanaux", "Secr\u00e9taires", "Employ\u00e9s de la comptabilit\u00e9", 
                "Employ\u00e9s administratifs d\'entreprise", "Secr\u00e9taires de direction", 
                "Techniciens des services administratifs, comptables et financiers", 
                "Cadres des services administratifs, comptables et financiers", 
                "Dirigeants d\'entreprises", "Employ\u00e9s et op\u00e9rateurs de l\'informatique", 
                "Techniciens de l\'informatique", "Ing\u00e9nieurs de l\'informatique", 
                "Personnels d\'\u00e9tudes et de recherche", "Employ\u00e9s administratifs de la fonction publique (cat\u00e9gorie C et assimil\u00e9s)", 
                "Professions interm\u00e9diaires administratives de la fonction publique (cat\u00e9gorie B et assimil\u00e9s)", 
                "Cadres de la fonction publique (cat\u00e9gorie A et assimil\u00e9s)", 
                "Professionnels du droit (hors juristes en entreprise)", "Arm\u00e9e, police, pompiers", 
                "Employ\u00e9s de la banque et des assurances", "Techniciens de la banque et des assurances", 
                "Cadres de la banque et des assurances", "Caissiers, employ\u00e9s de libre service", 
                "Vendeurs", "Attach\u00e9s commerciaux et repr\u00e9sentants", "Ma\u00eetrise des magasins et interm\u00e9diaires du commerce", 
                "Cadres commerciaux et technico-commerciaux", "Bouchers, charcutiers, boulangers", 
                "Cuisiniers", "Employ\u00e9s et agents de ma\u00eetrise de l\'h\u00f4tellerie et de la restauration", 
                "Patrons et cadres d\'h\u00f4tels, caf\u00e9s, restaurants", "Coiffeurs, esth\u00e9ticiens", 
                "Employ\u00e9s de maison", "Aides \u00e0 domicile et aides m\u00e9nag\u00e8res", 
                "Assistantes maternelles", "Agents de gardiennage et de s\u00e9curit\u00e9", 
                "Agents d\'entretien", "Employ\u00e9s des services divers", "Professionnels de la communication et de l\'information", 
                "Professionnels des arts et des spectacles", "Aides-soignants", 
                "Infirmiers, sages-femmes", "M\u00e9decins et assimil\u00e9s", "Professions para-m\u00e9dicales", 
                "Professionnels de l\'action sociale et de l\'orientation", "Professionnels de l\'action culturelle, sportive et surveillants", 
                "Enseignants", "Formateurs", "Professionnels de la politique et clerg\u00e9", 
                "Non renseign\u00e9 ou autre", "Agriculteurs ind\u00e9pendants", "\u00c9leveurs ind\u00e9pendants", 
                "B\u00fbcherons, sylviculteurs ind\u00e9pendants", "Agriculteurs salari\u00e9s", 
                "\u00c9leveurs salari\u00e9s", "B\u00fbcherons, sylviculteurs salari\u00e9s et agents forestiers", 
                "Conducteurs d\'engins agricoles ou forestiers", "Mara\u00eechers, horticulteurs ind\u00e9pendants", 
                "Viticulteurs, arboriculteurs ind\u00e9pendants", "Mara\u00eechers, horticulteurs salari\u00e9s", 
                "Jardiniers salari\u00e9s", "Viticulteurs, arboriculteurs salari\u00e9s", 
                "Techniciens et agents d\'encadrement d\'exploitations agricoles", 
                "Ing\u00e9nieurs, cadres techniques de l\'agriculture", "Marins, p\u00eacheurs, aquaculteurs ind\u00e9pendants", 
                "P\u00eacheurs, aquaculteurs salari\u00e9s", "Marins salari\u00e9s", "Cadres et ma\u00eetres d\'\u00e9quipage de la marine", 
                "Ouvriers non qualifi\u00e9s des travaux publics, du b\u00e9ton et de l\'extraction", 
                "Ouvriers non qualifi\u00e9s du gros \u0153uvre du b\u00e2timent", "Ouvriers qualifi\u00e9s des travaux publics, du b\u00e9ton et de l\'extraction", 
                "Ma\u00e7ons", "Professionnels du travail de la pierre et des mat\u00e9riaux associ\u00e9s", 
                "Charpentiers (m\u00e9tal)", "Charpentiers (bois)", "Couvreurs", "Ouvriers non qualifi\u00e9s du second \u0153uvre du b\u00e2timent", 
                "Plombiers, chauffagistes", "Menuisiers et ouvriers de l\'agencement et de l\'isolation", 
                "\u00c9lectriciens du b\u00e2timent", "Ouvriers qualifi\u00e9s de la peinture et de la finition du b\u00e2timent", 
                "Conducteurs d\'engins du b\u00e2timent et des travaux publics", "G\u00e9om\u00e8tres", 
                "Techniciens et charg\u00e9s d\'\u00e9tudes du b\u00e2timent et des travaux publics", 
                "Dessinateurs en b\u00e2timent et en travaux publics", "Chefs de chantier, conducteurs de travaux (non cadres)", 
                "Architectes", "Ing\u00e9nieurs du b\u00e2timent et des travaux publics, chefs de chantier et conducteurs de travaux (cadres)", 
                "Ouvriers non qualifi\u00e9s de l\'\u00e9lectricit\u00e9 et de l\'\u00e9lectronique", 
                "Ouvriers qualifi\u00e9s de l\'\u00e9lectricit\u00e9 et de l\'\u00e9lectronique", "Techniciens en \u00e9lectricit\u00e9 et en \u00e9lectronique", 
                "Dessinateurs en \u00e9lectricit\u00e9 et en \u00e9lectronique", "Agents de ma\u00eetrise et assimil\u00e9s en fabrication de mat\u00e9riel \u00e9lectrique, \u00e9lectronique", 
                "Ouvriers non qualifi\u00e9s travaillant par enl\u00e8vement ou formage de m\u00e9tal", 
                "R\u00e9gleurs", "Ouvriers qualifi\u00e9s travaillant par enl\u00e8vement de m\u00e9tal", 
                "Chaudronniers, t\u00f4liers, traceurs, serruriers, m\u00e9talliers, forgerons", 
                "Tuyauteurs", "Soudeurs", "Ouvriers non qualifi\u00e9s m\u00e9tallerie, serrurerie, montage", 
                "Monteurs, ajusteurs et autres ouvriers qualifi\u00e9s de la m\u00e9canique", 
                "Agents qualifi\u00e9s de traitement thermique et de surface", "Techniciens en m\u00e9canique et travail des m\u00e9taux", 
                "Dessinateurs en m\u00e9canique et travail des m\u00e9taux", "Agents de ma\u00eetrise et assimil\u00e9s en fabrication m\u00e9canique", 
                "Ouvriers non qualifi\u00e9s des industries chimiques et plastiques", 
                "Ouvriers non qualifi\u00e9s des industries agro-alimentaires", "Ouvriers non qualifi\u00e9s en m\u00e9tallurgie, verre, c\u00e9ramique et mat\u00e9riaux de construction", 
                "Ouvriers non qualifi\u00e9s du papier-carton et du bois", "Autres ouvriers non qualifi\u00e9s de type industriel", 
                "Pilotes d\'installation lourde des industries de transformation", 
                "Autres ouvriers qualifi\u00e9s des industries chimiques et plastiques", 
                "Autres ouvriers qualifi\u00e9s des industries agro-alimentaires (hors transformation des viandes)", 
                "Autres ouvriers qualifi\u00e9s en verre, c\u00e9ramique, m\u00e9tallurgie, mat\u00e9riaux de construction et \u00e9nergie ", 
                "Ouvriers qualifi\u00e9s des industries lourdes du bois et de la fabrication de papier-carton", 
                "Agents qualifi\u00e9s de laboratoire", "Autres ouvriers qualifi\u00e9s de type industriel", 
                "Techniciens des industries de process", "Agents de ma\u00eetrise et assimil\u00e9s des industries de process", 
                "Tous m\u00e9tiers", "Ouvriers non qualifi\u00e9s du textile et du cuir", 
                "Ouvriers qualifi\u00e9s du travail industriel du textile et du cuir", 
                "Ouvriers qualifi\u00e9s du travail artisanal du textile et du cuir", 
                "Ouvriers non qualifi\u00e9s du travail du bois et de l\'ameublement", 
                "Artisans du travail du bois et de l\'ameublement", "Ouvriers qualifi\u00e9s du travail du bois et de l\'ameublement", 
                "Ouvriers non qualifi\u00e9s de l\'imprimerie, de la presse et de l\'\u00e9dition", 
                "Ouvriers qualifi\u00e9s de l\'impression et du fa\u00e7onnage des industries graphiques", 
                "Techniciens et agents de ma\u00eetrise des mat\u00e9riaux souples, du bois et des industries graphiques", 
                "Ouvriers qualifi\u00e9s de la maintenance en m\u00e9canique", "Ouvriers qualifi\u00e9s de la maintenance en \u00e9lectricit\u00e9 et en \u00e9lectronique", 
                "Mainteniciens en biens \u00e9lectrodomestiques", "Ouvriers qualifi\u00e9s polyvalents d\'entretien du b\u00e2timent", 
                "Carrossiers automobiles", "M\u00e9caniciens et \u00e9lectroniciens de v\u00e9hicules", 
                "Techniciens et agents de ma\u00eetrise de la maintenance et de l\'environnement", 
                "Techniciens experts", "Agents de ma\u00eetrise en entretien", "Ing\u00e9nieurs et cadres de fabrication et de la production", 
                "Cadres techniques de la maintenance et de l\'environnement", 
                "Ing\u00e9nieurs des m\u00e9thodes de production, du contr\u00f4le qualit\u00e9", 
                "Ouvriers non qualifi\u00e9s de l\'emballage et manutentionnaires", 
                "Ouvriers qualifi\u00e9s du magasinage et de la manutention", "Responsables magasinage", 
                "Conducteurs de v\u00e9hicules l\u00e9gers", "Conducteurs de transport en commun sur route", 
                "Conducteurs et livreurs sur courte distance", "Conducteurs routiers", 
                "Conducteurs sur rails et d\'engins de traction", "Agents d\'exploitation des transports", 
                "Contr\u00f4leurs des transports", "Responsables logistiques (non cadres)", 
                "Agents et h\u00f4tesses d\'accompagnement", "Agents administratifs des transports", 
                "Employ\u00e9s des transports et du tourisme", "Techniciens des transports et du tourisme", 
                "Cadres des transports", "Personnels navigants de l\'aviation", 
                "Ing\u00e9nieurs et cadres de la logistique, du planning et de l\'ordonnancement", 
                "Ouvriers non qualifi\u00e9s divers de type artisanal", "Artisans et ouvriers qualifi\u00e9s divers de type artisanal", 
                "Secr\u00e9taires bureautiques et assimil\u00e9s", "Employ\u00e9s de la comptabilit\u00e9", 
                "Agents d\'accueil et d\'information", "Agents administratifs divers", 
                "Secr\u00e9taires de direction", "Techniciens des services administratifs", 
                "Techniciens des services comptables et financiers", "Cadres administratifs, comptables et financiers (hors juristes)", 
                "Juristes", "Cadres des ressources humaines et du recrutement", 
                "Dirigeants de petites et moyennes entreprises", "Cadres dirigeants des grandes entreprises", 
                "Employ\u00e9s et op\u00e9rateurs en informatique", "Techniciens d\'\u00e9tude et de d\u00e9veloppement en informatique", 
                "Techniciens de production, d\'exploitation, d\'installation, et de maintenance, support et services aux utilisateurs en informatique", 
                "Ing\u00e9nieurs et cadres d\'\u00e9tude, recherche et d\u00e9veloppement en informatique, chefs de projets informatiques", 
                "Ing\u00e9nieurs et cadres d\'administration, maintenance en informatique", 
                "Ing\u00e9nieurs et cadres des t\u00e9l\u00e9communications", "Ing\u00e9nieurs et cadres d\'\u00e9tude, recherche et d\u00e9veloppement (industrie)", 
                "Chercheurs (sauf industrie et enseignement sup\u00e9rieur)", "Agents des imp\u00f4ts et des douanes", 
                "Employ\u00e9s des services au public", "Employ\u00e9s de la Poste et des t\u00e9l\u00e9communications", 
                "Contr\u00f4leurs des imp\u00f4ts et des douanes", "Autres cadres B de la fonction publique", 
                "Professions interm\u00e9diaires de la Poste et des t\u00e9l\u00e9communications", 
                "Cadres A de la fonction publique (hors sp\u00e9cialit\u00e9s juridiques) et assimil\u00e9s", 
                "Cadres de la Poste et des t\u00e9l\u00e9communications", "Cadres de l\'arm\u00e9e et de la gendarmerie", 
                "Professionnels du droit", "Magistrats", "Agents de s\u00e9curit\u00e9 et de l\'ordre public", 
                "Agents de polices municipales", "Cadres interm\u00e9diaires de la police et de l\'arm\u00e9e", 
                "Employ\u00e9s de la banque et des assurances", "Techniciens de la banque", 
                "Techniciens des assurances", "Cadres de la banque", "Cadres des assurances", 
                "Employ\u00e9s de libre service", "Caissiers", "Vendeurs en produits alimentaires", 
                "Vendeurs en ameublement, \u00e9quipement du foyer, bricolage", "Vendeurs en habillement et accessoires, articles de luxe, de sport, de loisirs et culturels", 
                "Vendeurs en gros de mat\u00e9riel et \u00e9quipements", "Vendeurs g\u00e9n\u00e9ralistes", 
                "T\u00e9l\u00e9vendeurs", "Attach\u00e9s commerciaux", "Repr\u00e9sentants aupr\u00e8s des particuliers", 
                "Ma\u00eetrise des magasins", "Interm\u00e9diaires du commerce", "Professions interm\u00e9diaires commerciales", 
                "Cadres commerciaux, acheteurs et cadres de la mercatique", "Ing\u00e9nieurs et cadres technico-commerciaux", 
                "Cadres des magasins", "Agents immobiliers, syndics", "Apprentis et ouvriers non qualifi\u00e9s de l\'alimentation (hors industries agro-alimentaires)", 
                "Bouchers", "Charcutiers, traiteurs", "Boulangers, p\u00e2tissiers", 
                "Aides de cuisine, apprentis de cuisine et employ\u00e9s polyvalents de la restauration", 
                "Cuisiniers", "Chefs cuisiniers", "Employ\u00e9s de l\'h\u00f4tellerie", 
                "Serveurs de caf\u00e9s restaurants", "Ma\u00eetres d\'h\u00f4tel", "Ma\u00eetrise de l\'h\u00f4tellerie", 
                "Patrons d\'h\u00f4tels, caf\u00e9s, restaurants", "Cadres de l\'h\u00f4tellerie et de la restauration", 
                "Coiffeurs, esth\u00e9ticiens", "Employ\u00e9s de maison et personnels de m\u00e9nage", 
                "Aides \u00e0 domicile et aides m\u00e9nag\u00e8res", "Assistantes maternelles", 
                "Concierges", "Agents de s\u00e9curit\u00e9 et de surveillance", "Agents d\'entretien de locaux", 
                "Agents de services hospitaliers", "Ouvriers de l\'assainissement et du traitement des d\u00e9chets", 
                "Employ\u00e9s des services divers", "Assistants de communication", 
                "Interpr\u00e8tes", "Cadres de la communication", "Cadres et techniciens de la documentation", 
                "Journalistes et cadres de l\'\u00e9dition", "Professionnels des spectacles", 
                "Photographes", "Graphistes, dessinateurs, stylistes, d\u00e9corateurs et cr\u00e9ateurs de supports de communication visuelle", 
                "Artistes (musique, danse, spectacles)", "\u00c9crivains", "Artistes plasticiens", 
                "Aides-soignants", "Infirmiers", "Sages-femmes", "M\u00e9decins", 
                "Dentistes", "V\u00e9t\u00e9rinaires", "Pharmaciens", "Techniciens m\u00e9dicaux et pr\u00e9parateurs", 
                "Sp\u00e9cialistes de l\'appareillage m\u00e9dical", "Autres professionnels para-m\u00e9dicaux", 
                "Psychologues, psychoth\u00e9rapeutes", "Professionnels de l\'orientation", 
                "Educateurs sp\u00e9cialis\u00e9s", "Professionnels de l\'action sociale", 
                "Exploitants d\'\u00e9quipements sportifs et culturels", "Professionnels de l\'animation socioculturelle", 
                "Sportifs et animateurs sportifs", "Surveillants d\'\u00e9tablissements scolaires", 
                "Professeurs des \u00e9coles", "Professeurs du secondaire", "Directeurs d\'\u00e9tablissement scolaire et inspecteurs", 
                "Professeurs du sup\u00e9rieur", "Formateurs", "Professionnels de la politique", 
                "Clerg\u00e9", "Non renseign\u00e9 ou autre")
  )
  
  # code_by_label_fap_avec_domaine = c(
  #   "Ensemble du domaine A : Agriculture, marine, peche" = "A",
  #   "Agriculteurs, \u00e9leveurs, sylviculteurs, b\u00fbcherons" = "A0Z",
  #   "Mara\u00eechers, viticulteurs, jardiniers" = "A1Z",
  #   "Techniciens et cadres de l\'agriculture" = "A2Z",
  #   "Marins, p\u00eacheurs, aquaculteurs" = "A3Z",
  #   "Ensemble du domaine B : Batiment, travaux publics" = "B",
  #   "ONQ du gros oeuvre du b\u00e2timent, travaux publics, b\u00e9ton et extraction" = "B0Z",
  #   "OQ des travaux publics, du b\u00e9ton et de l\'extraction" = "B1Z",
  #   "OQ du gros oeuvre du batiment" = "B2Z",
  #   "ONQ du second oeuvre du b\u00e2timent" = "B3Z",
  #   "OQ du second oeuvre du b\u00e2timent" = "B4Z",
  #   "Conducteurs d\'engins du b\u00e2timent et des travaux publics" = "B5Z",
  #   "Techniciens et AM du b\u00e2timent et des travaux publics" = "B6Z",
  #   "Architectes et cadres du batiment et des travaux publics" = "B7Z",
  #   "Ensemble du domaine C : \u00e9lectricit\u00e9, \u00e9lectronique" = "C",
  #   "ONQ de l\'\u00e9lectricit\u00e9 et de l\'\u00e9lectronique" = "C0Z",
  #   "OQ de l\'\u00e9lectricit\u00e9 et de l\'\u00e9lectronique" = "C1Z",
  #   "Techniciens et AM de l\'\u00e9lectricit\u00e9 et de l\'\u00e9lectronique" = "C2Z",
  #   "Ensemble du domaine D : M\u00e9canique, travail des m\u00e9taux" = "D",
  #   "ONQ travaillant par enl\u00e8vement ou formage de m\u00e9tal" = "D0Z",
  #   "OQ travaillant par enl\u00e8vement de m\u00e9tal" = "D1Z",
  #   "OQ travaillant par formage de m\u00e9tal" = "D2Z",
  #   "ONQ de la m\u00e9canique" = "D3Z",
  #   "OQ de la m\u00e9canique" = "D4Z",
  #   "Techniciens et AM des industries m\u00e9caniques" = "D6Z",
  #   "Ensemble du domaine E : Industries de process" = "E",
  #   "ONQ des industries de process" = "E0Z",
  #   "OQ des industries de process" = "E1Z",
  #   "Techniciens et AM des industries de process" = "E2Z",
  #   "Ensemble du domaine F : Mat\u00e9riaux souples, bois, industries graphiques" = "F",
  #   "ONQ du textile et du cuir" = "F0Z",
  #   "OQ du textile et du cuir" = "F1Z",
  #   "ONQ du travail du bois et de l\'ameublement" = "F2Z",
  #   "OQ du travail du bois et de l\'ameublement" = "F3Z",
  #   "Ouvriers des industries graphiques" = "F4Z",
  #   "Techniciens et AM des mat\u00e9riaux souples, bois et industries graphiques" = "F5Z",
  #   "Ensemble du domaine G : Maintenance" = "G",
  #   "OQ de la maintenance" = "G0A",
  #   "OQ de la r\u00e9paration automobile" = "G0B",
  #   "Techniciens et AM de la maintenance" = "G1Z",
  #   "Ensemble du domaine H : Ing\u00e9nieurs et cadres de l\'industrie" = "H",
  #   "Ing\u00e9nieurs et cadres techniques de l\'industrie" = "H0Z",
  #   "Ensemble du domaine J : Transports, logistique et tourisme" = "J",
  #   "ONQ de la manutention" = "J0Z",
  #   "OQ de la manutention" = "J1Z",
  #   "Conducteurs de v\u00e9hicules" = "J3Z",
  #   "Agents d\'exploitation des transports" = "J4Z",
  #   "Agents administratifs et commerciaux des transports et du tourisme" = "J5Z",
  #   "Cadres des transports, de la logistique et navigants de l\'aviation" = "J6Z",
  #   "Ensemble du domaine K : Artisanat" = "K",
  #   "Artisans et ouvriers artisanaux" = "K0Z",
  #   "Ensemble du domaine L : Gestion, administration des entreprises" = "L",
  #   "Secr\u00e9taires" = "L0Z",
  #   "Employ\u00e9s de la comptabilit\u00e9" = "L1Z",
  #   "Employ\u00e9s administratifs d\'entreprise" = "L2Z",
  #   "Secr\u00e9taires de direction" = "L3Z",
  #   "Techniciens des services administratifs, comptables et financiers" = "L4Z",
  #   "Cadres des services administratifs, comptables et financiers" = "L5Z",
  #   "Dirigeants d\'entreprise" = "L6Z",
  #   "Ensemble du domaine M : Informatique" = "M",
  #   "Employ\u00e9s et op\u00e9rateurs de l\'informatique" = "M0Z",
  #   "Techniciens de l\'informatique" = "M1Z",
  #   "Ing\u00e9nieurs de l\'informatique" = "M2Z",
  #   "Ensemble du domaine N : \u00e9tudes et recherche" = "N",
  #   "Personnels d\'\u00e9tudes et de recherche" = "N0Z",
  #   "Ensemble du domaine P : Administration publique, professions juridiques, arm\u00e9e et police" = "P",
  #   "Employ\u00e9s administratifs de la fonction publique (cat\u00e9gorie C et assimil\u00e9s)" = "P0Z",
  #   "Professions interm\u00e9diaires administratives de la fonction publique (cat\u00e9gorie B et assimil\u00e9s)" = "P1Z",
  #   "Cadres de la fonction publique (cat\u00e9gorie A et assimil\u00e9s)" = "P2Z",
  #   "Professionnels du droit (hors juristes en entreprise)" = "P3Z",
  #   "Militaires, policiers, pompiers" = "P4Z",
  #   "Ensemble du domaine Q : Banque et assurances" = "Q",
  #   "Employ\u00e9s de la banque et des assurances" = "Q0Z",
  #   "Techniciens de la banque et des assurances" = "Q1Z",
  #   "Cadres de la banque et des assurances" = "Q2Z",
  #   "Ensemble du domaine R : Commerce" = "R",
  #   "Caissiers, employ\u00e9s de libre-service" = "R0Z",
  #   "Vendeurs" = "R1Z",
  #   "Attach\u00e9s commerciaux et repr\u00e9sentants" = "R2Z",
  #   "Ma\u00eetrise des magasins et interm\u00e9diaires du commerce" = "R3Z",
  #   "Cadres commerciaux et technico-commerciaux" = "R4Z",
  #   "Ensemble du domaine S : Hotellerie, restauration, alimentation" = "S",
  #   "Bouchers, charcutiers, boulangers" = "S0Z",
  #   "Cuisiniers" = "S1Z",
  #   "Employ\u00e9s et AM de l\'hotellerie et de la restauration" = "S2Z",
  #   "Patrons et cadres d\'hotels, caf\u00e9s, restaurants" = "S3Z",
  #   "Ensemble du domaine T : Services aux particuliers et aux collectivit\u00e9s" = "T",
  #   "Coiffeurs, esth\u00e9ticiens" = "T0Z",
  #   "Employ\u00e9s de maison" = "T1Z",
  #   "Aides \u00e0 domicile et aides m\u00e9nag\u00e8res" = "T2A",
  #   "Assistantes maternelles" = "T2B",
  #   "Agents de gardiennage et de s\u00e9curit\u00e9" = "T3Z",
  #   "Agents d\'entretien" = "T4Z",
  #   "Employ\u00e9s des services divers" = "T6Z",
  #   "Ensemble du domaine U : Communication, information, arts et spectacles" = "U",
  #   "Professionnels de la communication et de l\'information" = "U0Z",
  #   "Professionnels des arts et des spectacles" = "U1Z",
  #   "Ensemble du domaine V : Sant\u00e9, action sociale, culturelle et sportive" = "V",
  #   "Aides-soignants" = "V0Z",
  #   "Infirmiers" = "V1Z",
  #   "M\u00e9decins et assimil\u00e9s" = "V2Z",
  #   "Professions param\u00e9dicales (hors infirmiers)" = "V3Z",
  #   "Professionnels de l\'action sociale et de l\'orientation" = "V4Z",
  #   "Professionnels de l\'action culturelle, sportive et surveillants" = "V5Z",
  #   "Ensemble du domaine W :Enseignement, formation" = "W",
  #   "Enseignants" = "W0Z",
  #   "Formateurs" = "W1Z"
  # )
  
  # fap_codes <- data.table(
  #   libelle = names(code_by_label_fap_avec_domaine),
  #   fap = unname(code_by_label_fap_avec_domaine)
  # )
  
  fap_codes[, domaine := 0]
  fap_codes[nchar(fap) == 1, domaine := 1]
  fap_codes[, fap_detaillee := 0]
  fap_codes[nchar(fap) == 5, fap_detaillee :=1]
  fap_codes[, fap_agregee := 0]
  fap_codes[nchar(fap) == 3, fap_agregee :=1]
  fap_codes[!fap %chin% c("Y", "Z")]
  
  fap_codes[, fap_87 := substr(fap_codes$fap, 1, 3)]
  fap_codes[fap_detaillee != 1, fap_87 := ""]
  
  nb_fap_225_by_fap_87 <- fap_codes[, .N, by=.(fap_87)]
  fap_codes <- merge(fap_codes, nb_fap_225_by_fap_87,
                     by.x="fap_87", by.y = "fap_87",
                     all.x = TRUE, all.y = FALSE)
  
  fap_codes
  }




#' Obtenir le libelle d'un code FAP
#'
#' @param code_fap Code FAP valide.
#'
#' @return Une chaine de caracteres.
#' @export
#'
#' @examples
#' 
#' obtenir_libelle("A1Z")
#' 
#' obtenir_libelle("A1Z00")
#' 
obtenir_libelle <- function(code_fap) {
  code_fap <- tolower(code_fap)
  libelles <- libelles_fap()
  libelles[tolower(fap) == code_fap, c(libelle)]
}




#' Obtenir la liste des codes FAP selon un domaine
#'
#' @param domaine Code du domaine.
#' @param inclure_domaine Inclure ou non dans la sortie la ligne concernant le domaine.
#'
#' @return Un \code{data.table}.
#' @export
#'
#' @examples
#' 
#' obtenir_faps_par_domaine("A")
#' 
#' obtenir_faps_par_domaine("A", inclure_domaine = TRUE)
obtenir_faps_par_domaine <- function(domaine, inclure_domaine = FALSE) {
  domaine_ <- tolower(domaine)
  faps <- libelles_fap()
  faps <- faps[tolower(fap) %like% paste0("^", domaine_)]
  if (isFALSE(inclure_domaine)) {
    faps <- faps[domaine == 0]
  }
  faps[]
}




#' Liste des codes FAP et leur libelles
#'
#' @param domaines Obtenir les domaines ? \code{TRUE} or \code{FALSE}.
#' @param par_domaines Ajouter la hierarchie par domaine dans la sortie,
#'  fonctionne uniquement si \code{domaines = FALSE}.
#'
#' @return Une \code{list} (pour utiliser dans \code{selectInput} ou \code{pickerInput}.
#' @export
#' 
#' @importFrom data.table :=
#' @importFrom stats setNames
#'
#' @examples
#' 
#' liste_libelles()
#' 
#' 
#' if (interactive()) {
#'   library(shiny)
#'   library(shinyWidgets)
#'   
#'   ui <- fluidPage(
#'     pickerInput(
#'       inputId = "domaine",
#'       label = "Domaines :",
#'       choices = liste_libelles(domaines = TRUE)
#'     ),
#'     verbatimTextOutput("res_domaine"),
#'     pickerInput(
#'       inputId = "fap_seul",
#'       label = "FAP :",
#'       choices = liste_libelles(par_domaines = FALSE)
#'     ),
#'     verbatimTextOutput("res_fap_seul"),
#'     pickerInput(
#'       inputId = "fap",
#'       label = "FAP (par domaine) :",
#'       choices = liste_libelles()
#'     ),
#'     verbatimTextOutput("res_fap")
#'   )
#'   
#'   server <- function(input, output, session) {
#'     
#'     output$res_domaine <- renderPrint(input$domaine)
#'     output$res_fap_seul <- renderPrint(input$fap_seul)
#'     output$res_fap <- renderPrint(input$fap)
#'     
#'   }
#'   
#'   shinyApp(ui, server)
#' }
liste_libelles <- function(domaines = FALSE, par_domaines = TRUE) {
  libelles <- libelles_fap()
  if (isTRUE(domaines)) {
    libelles <- libelles[domaine == 1]
    return(setNames(as.list(libelles$fap), libelles$libelle))
  } else {
    if (isFALSE(par_domaines)) {
      libelles <- libelles[domaine == 0]
      return(setNames(as.list(libelles$fap), libelles$libelle))
    } else {
      libelles[, nom_domaine := substr(fap, start = 1, stop = 1)]
      res <- lapply(
        X = unique(libelles$nom_domaine),
        FUN = function(x) {
          faps <- libelles[nom_domaine == x & domaine == 0]
          setNames(as.list(faps$fap), faps$libelle)
        }
      )
      names(res) <- libelles[fap == nom_domaine, c(libelle)]
      return(res)
    }
  }
}




#' Ajout du libelle selon le code FAP dans une table
#'
#' @param data Une table contenant une variable \code{"fap"}.
#'
#' @return Une table avec une colonne \code{"libelle"} supplementaire.
#' @export
#'
#' @examples
#' 
#' effectifs <- lecture_effectif("inputs/graph_effectifs.csv")
#' ajout_libelle(effectifs)
#' effectifs
#' 
#' fap_data <- lecture_fap("inputs/")
#' fap_data <- ajout_libelle(fap_data)
#' fap_data[[1]]
#' 
ajout_libelle <- function(data) {
  if (inherits(data, "list")) {
    lapply(data, ajout_libelle)
  } else {
    libelles <- libelles_fap()
    libelles <- libelles[, list(libelle, fap)]
    if (!is.data.table(data))
      data <- as.data.table(data)
    data[, fap := toupper(fap)]
    data[libelles, libelle := i.libelle, on = "fap"]
    data[is.na(libelle), libelle := fap]
    data[libelle == "ENSEMBLE", libelle := "Tous m\u00e9tiers"]
    setorder(data, libelle)
    data[]
  }
}

#' Ajout des code et libelle du domaine selon le code FAP dans une table
#'
#' @param data Une table contenant une variable \code{"fap"}.
#'
#' @return Une table avec deux colonnes \code{"code_domaine"} et \code{"libelle_domaine"} supplementaires.
#' @export
#'
#' @examples
#' 
#' effectifs <- lecture_effectif("inputs/graph_effectifs.csv")
#' ajout_cod_lib_domaine(effectifs)
#' effectifs
#' 
#' fap_data <- lecture_fap("inputs/")
#' fap_data <- ajout_cod_lib_domaine(fap_data)
#' fap_data[[1]]
#' 
ajout_cod_lib_domaine <- function(data) {
  if (inherits(data, "list")) {
    lapply(data, ajout_cod_lib_domaine)
  } else {
    libelles <- libelles_fap()
    libelles <- libelles[, list(libelle, fap)]
    libelles <- libelles[, code:=fap]
    if (!is.data.table(data))
      data <- as.data.table(data)
    data[, fap := toupper(fap)]
    data[fap == "ENS", fap:="Y"]
    data[, code:=substr(fap,1,1)]
    data[libelles, libelle_domaine := i.libelle, on = "code"]
    setnames(data, old = "code", new = "code_domaine")
    data[is.na(libelle_domaine), libelle_domaine := code_domaine]
    data[libelle_domaine == "Y", libelle_domaine := "Tous m\u00e9tiers"]
    #setorder(data, libelle_domaine)
    data[]
  }
}

#' Ajout des code et libelle de la fap agrégée (fap87) selon le code FAP dans une table
#'
#' @param data Une table contenant une variable \code{"fap"}.
#'
#' @return Une table avec deux colonnes \code{"code_domaine"} et \code{"libelle_domaine"} supplementaires.
#' @export
#'
#' @examples
#' 
#' effectifs <- lecture_effectif("inputs/graph_effectifs.csv")
#' ajout_cod_lib_fap_agregee(effectifs)
#' effectifs
#' 
#' fap_data <- lecture_fap("inputs/")
#' fap_data <- ajout_cod_lib_fap_agregee(fap_data)
#' fap_data[[1]]
#' 
ajout_cod_lib_fap_agregee <- function(data) {
  if (inherits(data, "list")) {
    lapply(data, ajout_cod_lib_fap_agregee)
  } else {
    libelles <- libelles_fap()
    libelles <- libelles[, list(libelle, fap)]
    libelles <- libelles[, code:=fap]
    if (!is.data.table(data))
      data <- as.data.table(data)
    data[, fap := toupper(fap)]
    data[, code:=substr(fap,1,3)]
    data[nchar(fap) == 1, code:=NA]
    data[libelles, libelle_fap_agregee := i.libelle, on = "code"]
    setnames(data, old = "code", new = "code_fap_agregee")
    data[is.na(libelle_fap_agregee), libelle_fap_agregee := code_fap_agregee]
    data[libelle_fap_agregee == "ENS", libelle_domaine := "Tous m\u00e9tiers"]
    #setorder(data, libelle_domaine)
    data[]
  }
}



