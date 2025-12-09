TD Modèles Mixtes. ENGEES. Décembre 2025. JP Vacher

#-----------------------------------------------------------------------------------#


EXERCICE 1 : 
Issu de Barnagaud & Gimenez 2025. Analyse de données en écologie.

On va étudier la dynamique temporelle de l'infestation de forêts de conifères par la chenille processionnaire du pin.

Ouvrir le script script_chenilles_TD1.R
Le jeu de données associés chenille_processionnaire.csv doit se trouver dans le même dossier que le script.

#-----------------------------------------------------------------------------------#


EXERCICE 2 : 

Cheynel et al. 2025. Does nocturnal light pollution impair immune function in a wild-living amphibian? Functional Ecology


https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2435.70212

On télécharge les données en suivant les recommandations sur le site (open access)
On regarde comment elles se présentent, et on regarde le M&M de l'article. Que peut-on en dire ? Avez-vous des observations/remarques/critiques par rapport à ce que l'on a vu en cours ?

Avec ce jeu de données, on va tenter de retrouver les résultats de l'article (Figures 2 et 3).


#-----------------------------------------------------------------------------------#

EXERCICE 3 : VARIATION DU TAUX MÉTABOLIQUE ET DES PERTES HYDRIQUES CHEZ LES VIPÈRES EUROPÉENNES.

Source : Lucchini et al. 2025. Hydrothermal physiology and vulnerability to climatic change: insight from European vipers. Journal of Thermal Biology, 129: 104115.

doi: https://doi.org/10.1016/j.jtherbio.2025.104115

Deux tables de données associées :

vipera_males.xlsx
ViperTb2003.xlxs

Deux scripts R associés:

Analyses_5sp_and_Q10.R
equations_SMR_EWL_MDEE_MDEWL.R

Les données et scripts proviennent des supplementary data de l'article.

Intérêt de ce jeu de données : on sort un peu du contexte habituel en écologie, avec des données complexes et une hiérarchisation simple mais inhabituelle + on utilise un autre package que lme4, donc des codes un peu différents.

Les buts de ces exercices sont :
1. Explorer ensemble le jeu de données et comprendre le sens des variables au regard de la question scientifique posée.

2. Ajuster dans R un modèle linéaire mixte avec des données écologiques complexes à l'aide d'un package différent de celui vu en cours.

3. On va interpréter ensemble les sorties des modèles et voir comment ils sont valorisés (quelles sorties graphiques ?) et discutés dans les articles.

Durant le TD, nous déroulerons ensemble les scripts afin de bien comprendre les différentes étapes, explorer les données, ajuster les modèles, les interpréter et les évaluer, et voir s'il y aurait des choses à modifier ou à mieux expliquer dans les annotations.
