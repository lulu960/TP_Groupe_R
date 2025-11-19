# Projet Trafic Aerien NYC - Aeroports de Paris (ADP)

## Description

Projet d'analyse du trafic aerien des aeroports de New York City (JFK, LGA, EWR) realise dans le cadre d'une mission de Consultant Data pour Aeroports de Paris (ADP).

Ce projet comprend :
- Une analyse exploratoire complete des donnees de vols
- Une application web interactive Shiny pour la visualisation des donnees
- Des analyses sur les retards, les destinations, les compagnies aeriennes

## Donnees

Les donnees proviennent du package R `nycflights13` qui contient des informations sur 326,776 vols ayant decolle de NYC en 2013.

Source: Bureau of Transportation Statistics (BTS)

## Structure du Projet

```
TP_Groupe_R/
|-- main.r           # Script d'analyse exploratoire (Mission 1)
|-- app.r            # Application Shiny interactive (Mission 3)
|-- README.md        # Documentation du projet
|-- .gitignore       # Fichiers a ignorer par Git
```

## Installation

### Prerequisites

- R (version >= 4.0)
- RStudio (recommande)

### Packages Requis

```r
install.packages(c(
  "nycflights13",
  "dplyr",
  "ggplot2",
  "tidyr",
  "shiny",
  "shinydashboard",
  "plotly",
  "DT"
))
```

## Utilisation

### Analyse Exploratoire (Mission 1)

Pour executer l'analyse complete :

```r
source("main.r")
```

Ce script repond aux 8 questions de la Mission 1 :
1. Statistiques generales (aeroports, compagnies, vols annules)
2. Aeroports et destinations les plus empruntes
3. Destinations par compagnie avec graphiques
4. Vols vers Houston et Seattle
5. Tri et jointures des donnees
6. Couverture des compagnies
7. Destinations exclusives
8. Filtrage des vols United, American, Delta

### Application Web Shiny (Mission 3)

Pour lancer l'application web interactive :

```r
shiny::runApp("app.r")
```

Ou dans RStudio : ouvrir `app.r` et cliquer sur "Run App"

## Fonctionnalites de l'Application Shiny

L'application comprend 6 onglets :

### 1. Vue d'ensemble
- Indicateurs cles (KPIs)
- Top 10 destinations
- Repartition des vols par aeroport
- Distribution mensuelle

### 2. Destinations
- Carte geographique interactive des destinations
- Filtres par origine et compagnie
- Statistiques detaillees

### 3. Compagnies
- Analyse des destinations par compagnie
- Couverture geographique
- Destinations exclusives

### 4. Vols
- Recherche avancee de vols
- Filtres multiples
- Tableau des resultats

### 5. Retards & Performance
- Distribution des retards
- Performance par compagnie
- Taux d'annulation par aeroport

### 6. Donnees Brutes
- Acces aux tables completes
- Export possible

## Missions du Projet

### Mission 1 : Se familiariser avec les donnees ✅
Analyse exploratoire complete des donnees de vols, aeroports, compagnies et avions.

### Mission 2 : Creer la DB ⏳
Creation et mise en production d'une base de donnees SQL/NoSQL.

### Mission 3 : Creer une WebApp ✅
Application Shiny interactive pour la visualisation et l'analyse des donnees.

## Auteur

Consultant Data - Aeroports de Paris (ADP)

## Licence

Projet educatif - Donnees publiques du BTS
