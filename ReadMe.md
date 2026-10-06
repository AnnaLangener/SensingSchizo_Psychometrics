
# Read Me Sensing Schizophrenia Psychometrics


## 📌 Overview

This repository contains the code that was used for analyses in the paper "Capturing Schizophrenia Symptoms in Daily Life via the Smartphone: Psychometric Evaluation of Newly Developed Symptom Scales
"

---

### 🔧 Scripts

#### `Participant_EMA_TimeSeries.Rmd`

* Creates one self-contained interactive HTML report with a participant dropdown and four separate charts: three domains showing item traces and all domain means together.
* The output is `Participant_EMA_TimeSeries.html`, beside the R Markdown file. Drag, scroll, or enter a time range to zoom all four charts together; hover over points to inspect scores. Each chart has its own legend. Data and scripts are embedded for offline use. Responses are already scored; missing values are preserved and no smoothing is applied.

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

#### `TraditionalQuestionnaires_Reliability.R` [Traditional questionnaire reliability]

* Estimates weekly PS-R and NSI-PR generalizability coefficients and multilevel omega, and post-assessment NSI-PR and PNS Cronbach alpha in the included sample.


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
