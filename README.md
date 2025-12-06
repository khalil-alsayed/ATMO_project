TITRE DU PROJET

Outil d’analyse des émissions et consommations par secteur – Atmo Hauts-de-France



DESCRIPTION

Ce projet correspond à un stage de fin d’études (5 mois) réalisé au sein d’Atmo Hauts-de-France (Lille).

L’objectif est de développer un outil complet permettant d’analyser les émissions et les consommations

énergétiques de différents secteurs, de détecter des anomalies et de restituer les résultats via des

visualisations interactives et des applications web.



OBJECTIFS DU PROJET

\- Analyser les émissions et les consommations pour différents secteurs d’activité.

\- Fournir des outils d’exploration interactive pour les équipes internes et les partenaires.

\- Mettre en évidence les anomalies dans les données (univariées et multivariées).

\- Représenter la répartition spatiale des émissions à l’aide de cartographies.

\- Déployer une application web Shiny pour la restitution et le partage des résultats.



FONCTIONNALITÉS PRINCIPALES

\- Interaction avec les bases de données via DBeaver (SQL) pour l’extraction des données sources.

\- Traitement, nettoyage et agrégation des données en R (packages dplyr, data.table).

\- Création de visualisations interactives en R (ggplot2, plotly) pour l’exploration des données.

\- Détection d’anomalies :

&nbsp; \* Univariées : méthode de l’IQR (Interquartile Range).

&nbsp; \* Multivariées : distance de Mahalanobis, ILR (Isometric Log-Ratio).

\- Production de cartographies :

&nbsp; \* Utilisation de données raster.

&nbsp; \* Visualisation avec leaflet.

&nbsp; \* Synchronisation de cartes pour comparer plusieurs couches/spécifications.

\- Développement d’applications web Shiny pour :

&nbsp; \* Naviguer dans les indicateurs.

&nbsp; \* Filtrer par secteur, zone géographique, période temporelle, etc.

&nbsp; \* Visualiser dynamiquement les résultats (graphes, tableaux, cartes).

&nbsp; \* Mettre l’outil à disposition des équipes internes et de certains partenaires externes.



CONTENU DU DÉPÔT

Le dépôt contient :

\- Des scripts de code (principalement en R, et éventuellement SQL) permettant :

&nbsp; \* d’extraire les données depuis les bases de données,

&nbsp; \* de prétraiter et transformer les données,

&nbsp; \* de générer les graphiques, cartes et indicateurs,

&nbsp; \* d’exécuter l’application Shiny.

\- Un document PDF décrivant le projet (contexte, méthodologie, résultats, conclusions).

\- Une vidéo de démonstration du projet (présentation de l’outil, navigation dans l’application, exemples

&nbsp; de cas d’usage).



TECHNOLOGIES ET OUTILS

\- Langages :

&nbsp; \* R

&nbsp; \* SQL

\- Principaux packages R :

&nbsp; \* dplyr, data.table (manipulation de données)

&nbsp; \* ggplot2, plotly (visualisation)

&nbsp; \* leaflet (cartographie interactive)

&nbsp; \* shiny (applications web)

\- Outils de base de données :

&nbsp; \* DBeaver pour l’interaction avec les bases SQL

\- Autres :

&nbsp; \* Git / GitHub pour la gestion de version et le partage du code.



UTILISATION GÉNÉRALE (IDÉE)

1\. Configurer la connexion à la base de données dans les scripts R/SQL (identifiants, host, etc.).

2\. Lancer les scripts d’extraction et de prétraitement pour générer les tables et indicateurs nécessaires.

3\. Exécuter le script de visualisation pour produire les graphiques et cartes.

4\. Lancer l’application Shiny (par exemple : `shiny::runApp("chemin/vers/le/projet")`) pour accéder à

&nbsp;  l’interface web interactive.

5\. Consulter le rapport PDF et la vidéo pour une présentation détaillée de la méthodologie et des résultats.



AUTEUR

\- Projet réalisé dans le cadre d’un stage de fin d’études (5 mois) – Atmo Hauts-de-France, Lille.



\## Project material



\- 📄 \[Project report (PDF)](report.pdf)

\- 🎬 \[Demo video](demo.mp4)





