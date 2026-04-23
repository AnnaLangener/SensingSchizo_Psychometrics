
# Read Me Sensing Schizophrenia Psychometrics


## 📌 Overview

This repository contains the code that was used for analyses in the paper "Capturing Schizophrenia Symptoms in Daily Life via the Smartphone: Psychometric Evaluation of Newly Developed Symptom Scales
"

---

### 🔧 Scripts

#### `Data_Cleaning.R` [Cleaning and Descriptives]

* Imports raw EMA data
* Handles missingness and exclusions
* Recodes items and formats variables
* Outputs cleaned dataset used in all downstream analyses
* Calculates "Descriptives"

---

#### `HelperFunctions.R` [Reliability]

* Contains reusable functions used for `PsychometricProperties_Scales.R`

---

#### `PsychometricProperties_Scales.R` [Reliability]
* reliability analyses


---
#### `ConvergentValidity.R` [Validity]

* Examines associations between EMA-derived scales and related constructs


---

## 📊 Figures

* `inter_item_heatmaps.png`
  → Correlation matrices of participant-level mean item scores within each EMA category

* `convergent_validity_plot.png`
  → associations between constructs

* `convergent_validity_overtime.png`
  → Stability of associations across time

* `des.png`
  → Descriptive statistics visualization

