# WFP Data Quality Check

![Version](https://img.shields.io/badge/version-0.2.0-blue.svg)
![License](https://img.shields.io/badge/license-MIT-green.svg)

## 📋 Description

**WFP Data Quality Check** est une application Shiny pour l'analyse de la qualité des données et la visualisation d'indicateurs clés de sécurité alimentaire dans les enquêtes du Programme Alimentaire Mondial (PAM).

### ✨ Fonctionnalités principales

- 📊 **Analyse multi-indicateurs** : FCS, HDDS, rCSI, HHS, LCS
- 📁 **Support multi-formats** : SPSS (.sav), Excel (.xlsx/.xls), CSV, Stata (.dta), Text
- 🗺️ **Analyse géographique** : By Admin1, Admin2, et enquêteur
- 📈 **Visualisations interactives** : Graphiques dynamiques avec Plotly et ECharts4r
- 📋 **Rapports exportables** : Génération automatique de rapports HTML
- 🔍 **Contrôle qualité** : Validation automatique des données

### 🎯 Indicateurs supportés

- **FCS** (Food Consumption Score) - Score de Consommation Alimentaire
- **HDDS** (Household Dietary Diversity Score) - Score de Diversité Alimentaire des Ménages
- **rCSI** (Reduced Coping Strategy Index) - Indice de Stratégies de Survie Réduit
- **HHS** (Household Hunger Scale) - Échelle de la Faim des Ménages
- **LCS** (Livelihood Coping Strategy Index) - Indice de Stratégies de Survie des Moyens d'Existence





## 🚀 Installation

download the file **WFPdataqualitycheck_0.2.0.tar.gz** from Github


```r
install.packages("your file repositories/WFPdataqualitycheck_0.2.0.tar.gz", repos = NULL, type = "source")
```
## Requirements

The application requires datasets with standard variable names. For more information on required variables, please see the in-app documentation or refer to [this guide](https://wfp-vam.github.io/RBD_FS_CH_guide_EN/combined-questionnaire-syntaxes-for-all-5-indicators.html).

## Usage

```r
# Charger le package
library(WFPdataqualitycheck)

# Lancer l'application
run_app()
```

L'application s'ouvrira dans votre navigateur web par défaut.

## 📊 Format des données

### Variables requises

#### Variables administratives (Obligatoires)
- `ADMIN1Name` : Nom de la division administrative niveau 1
- `ADMIN2Name` : Nom de la division administrative niveau 2  
- `EnuName` : Nom/ID de l'enquêteur

#### Variables métadonnées
- `@_submission_time` : Horodatage de soumission
- `@_location_latitude` : Latitude GPS
- `@_location_longitude` : Longitude GPS

#### Variables d'indicateurs
- **FCS** : `FCSStap`, `FCSPulse`, `FCSPr`, `FCSVeg`, `FCSFruit`, `FCSDairy`, `FCSFat`, `FCSSugar`
- **HDDS** : `HDDSStapCer`, `HDDSStapRoot`, `HDDSVeg`, `HDDSFruit`, `HDDSPrMeat`, `HDDSPrEggs`, `HDDSPrFish`, `HDDSPulse`, `HDDSDairy`, `HDDSFat`, `HDDSSugar`, `HDDSCond`
- **rCSI** : `rCSILessQlty`, `rCSIBorrow`, `rCSIMealSize`, `rCSIMealAdult`, `rCSIMealNb`
- **HHS** : `HHSNoFood_FR`, `HHSBedHung_FR`, `HHSNotEat_FR`
- **LCS** : Variables de stress, crise et urgence

## 🔧 Configuration système requise

- **R** ≥ 4.0.0
- **Dépendances** : Les packages requis seront installés automatiquement

## 📝 Licence

Ce projet est sous licence MIT. Voir le fichier [LICENSE](LICENSE) pour plus de détails.

## 👤 Auteur

**Idrissa Dabo**
- 📧 Email : ididabo@gmail.com
- 🏢 Organisation : World Food Programme (WFP)


## Contact

For questions or support, please contact:
- Idrissa Dabo: idrissa.dabo@wfp.org
- Paolo Lucchino: paolo.lucchino@wfp.org
- Hatem Kotb: hatem.kotb@wfp.org
- Alessandra Gherardelli: alessandra.gherardelli@wfp.org
- Aliou Badara Samake: alioubadara.samake@wfp.org
- Moctar Aboubacar: moctar.aboubacar@wfp.org


## 🤝 Support

Pour toute question ou problème :
1. Vérifiez que vos données contiennent les variables requises
2. Consultez la documentation intégrée dans l'application
3. Contactez l'auteur pour support technique

---