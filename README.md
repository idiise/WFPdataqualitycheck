# WFP Data Quality Check

![Version](https://img.shields.io/badge/version-0.2.0-blue.svg)
![License](https://img.shields.io/badge/license-MIT-green.svg)

## 📋 Description

**WFP Data Quality Check** is a Shiny application for data quality analysis and visualization of key food security indicators in World Food Programme (WFP) surveys.

### ✨ Main Features

- 📊 **Multi-indicator analysis** : FCS, HDDS, rCSI, HHS, LCS
- 📁 **Multi-format support** : SPSS (.sav), Excel (.xlsx/.xls), CSV, Stata (.dta), Text
- 🗺️ **Geographic analysis** : By Admin1, Admin2, and Enumerator
- 📈 **Interactive visualizations** : Dynamic charts using Plotly and ECharts4r and WFPTheme color palette
- 📋 **Exportable reports** : Automatic generation of HTML reports
- 🔍 **Quality control** : Automatic data validation

### 🎯 Supported Indicators

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

The application requires datasets with standard variable names. For more information on required variables, please see the in-app documentation or refer to [Surveydesigner](https://www.surveydesigner.vam.wfp.org/).

## Usage

```r
# Charger le package
library(WFPdataqualitycheck)

# Lancer l'application
run_app()
```

The application will open in your default web browser.

## 📊 Data Format

### Required Variables
Mandatory variables are available on the main page of the application
#### Administrative variables (Mandatory)
 
- `ADMIN1Name` : Name of the first-level administrative division
- `ADMIN2Name` : Name of the second-level administrative division 
- `EnuName` : Enumerator name or ID

#### Variables métadonnées
- `@_submission_time` : ubmission timestamp

#### Indicator variables
- **FCS** : `FCSStap`, `FCSPulse`, `FCSPr`, `FCSVeg`, `FCSFruit`, `FCSDairy`, `FCSFat`, `FCSSugar`
- **HDDS** : `HDDSStapCer`, `HDDSStapRoot`, `HDDSVeg`, `HDDSFruit`, `HDDSPrMeat`, `HDDSPrEggs`, `HDDSPrFish`, `HDDSPulse`, `HDDSDairy`, `HDDSFat`, `HDDSSugar`, `HDDSCond`
- **rCSI** : `rCSILessQlty`, `rCSIBorrow`, `rCSIMealSize`, `rCSIMealAdult`, `rCSIMealNb`
- **HHS** : `HHSNoFood_FR`, `HHSBedHung_FR`, `HHSNotEat_FR`
- **LCS** : Variables de stress, crise et urgence

## 🔧 System Requirements

- **R** ≥ 4.0.0



## 👤 Author

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
- Virginia Leape: virginia.leape@wfp.org


## 🤝 Support

For any questions or issues:
1. Ensure your data includes all required variables and if there from surveydesigner
2. Refer to the in-app documentation from Github
3. Contact one of the persons mentioned above for technical support

---
