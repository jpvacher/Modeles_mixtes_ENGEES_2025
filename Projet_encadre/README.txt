Projet encadré ENGEES Décembre 2025 - Janvier 2026. G David, JP Vacher


#------------------------------------------------------------------------------#
CONTEXTE
Le Triton crêté est un amphibien protégé et menacé à l'échelle européenne. Un suivi est mené dans le Grand Est depuis 2021 pour comprendre la dynamique de population et la relier à des facteurs d'influence (positive ou négative). Dans ce cadre, une stratégie d'échantillonnage a été mise en place pour récolter des données et appréhender l'évolution des populations à l'échelle de la région.

#------------------------------------------------------------------------------#
QUESTIONS

1. Quels sont les déterminants de la présence, l'abondance et la détection du Triton crêté dans les mares du Grand Est ? 

2. Trouve-t-on plus de tritons dans les grandes mares que les petites mares ? 

3. Est-ce qu'il y a un effet de proximité sur l'effectif, c'est-à-dire est-ce que l'on trouve plus de tritons dans des petites mares groupées géographiquement ? On s'attend à une structuration en métapopulations.

4. Existe-t-il une saisonnalité dans la fréquence du Triton crêté dans les mares ? On s'attend à plus de tritons en début de saison de reproduction qu'en fin de saison de reproduction.

Est-ce que vous auriez une autre idée de question en examinant le jeu de données ? Si oui, question libre et justifier votre choix.

#-----------------------------------------------------------------------------------#
CE QUI EST MIS À DISPOSITION

- Un protocole de suivi qui décrit le contexte, la stratégie d'échantillonnage, et ce qui est mesuré (quoi, comment ?).

- On dispose de trois jeux de données :

1. data_triturus.txt
Ce jeu de données contient des relevés de comptage de Tritons crêtés dans 174 mares du Grand Est durant deux années, et avec trois sessions de comptage par année.


2. variables envir.txt
Ce jeu de données contient 48 variables biotiques et abiotiques associées aux 174 mares.

Variables détection :

température de l’air en °C
température de l’eau en °C
pluviosité (deux facteurs : 0/1)
méthode (cinq facteurs : nasse_flottante, nasse_immergee, amphicapt, vue, epuisette)
date au format JJ/MM/AAAA

Variables occurrence/abondance :

altitude = altitude en m
foret = surface de forêt (m2) dans zone tampon de 500 m
haies = linéaires de haies (m) dans zone tampon de 500 m
route = Emprise du réseau routier  (m2) dans zone tampon de 500 m, une sous-catégorie de niveau 4 du
libellé de niveau 2 « Infrastructures et superstructures des réseaux de transport »
nb_mares = nombre de mares dans zone tampon 500 m
dist_presence = distance à la mare occupée la plus proche
surface = surface de la mare (m2)
ensoleillement = catégories d'ensoleillement
hydrophytes = recouvrement d'hydrophytes en pourcentages à 5% près


3. Regions_naturelles
Couche SIG téléchargée sur https://www.datagrandest.fr/data4citizen/visualisation/information/?id=fr-417566924-180706_001


- Un script R :

script_explore_data.R

Ce script est annoté partiellement. À vous de voir s'il est nécessaire de compléter les annotations.


#------------------------------------------------------------------------------#
COMMENT S'Y PRENDRE ?

- Le script “script_explore_data.R” est donné pour commencer à explorer et mettre en forme les données. Il reste encore des choses à faire (exploration graphique, nouvelles variables, transformations…).

Continuer à partir de ce script à développer votre démarche. Penser à annoter le script. Vous pouvez soit avoir un script et un document de rapportage séparés, soit tout faire dans un fichier RMarkdown.

Si vous voulez avoir plusieurs scripts, vous pouvez. Dans ce cas, il faudra bien expliquer dans M&M, ou dans un fichier de métadonnées, où trouver les informations.

N'hésitez pas durant votre à consigner des notes à part pour la discussion, par exemple : ce que nous disent les résultats, est-ce que c'est cohérent avec ce qui est connu dans la littérature, ce qu'il n'est pas possible de calculer avec le jeu de données, ce qu'il aurait fallu faire autrement ?

(Optionel) Vous pouvez construire un logigramme (flowchart) pour structurer votre démarche analytique et les différentes étapes qui permettent d'arriver aux résultats.

On pourra voir ensemble les solutions pour mettre en place des penses-bêtes pour les résultats et la discussion.
