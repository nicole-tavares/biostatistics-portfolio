# Outcomes Research Analysis in SAS

## Overview
This project presents an outcomes research analysis conducted using SAS,
based on a simulated hospital cardiac surgery dataset.

The goal was to prepare clean, analysis-ready data and generate
tables and statistical summaries for presentation to a principal
investigator.

## Data Description
Two datasets were used:

- **SURGERIES**
  - Patient ID, event date, sex, race, outcome, procedure type
  - Multiple procedures per patient per day possible
  - Age recorded only at first event date

- **SMOKING**
  - Patient ID
  - History of smoking (yes/no)

To protect privacy, raw data are not included.

## Key Data Challenges
- Reconstructing **time-varying age** for each event date
- Identifying and counting **multiple procedures on the same day**
- Correctly merging smoking history to all patient records
- Creating analysis datasets with no missing values

## Methods
- DATA step processing with BY-group logic
- Retained counters to identify combination procedures
- PROC FREQ and PROC TABULATE for descriptive summaries
- PROC MEANS for age-at-death comparisons
- Two-sample t-test to assess mortality differences by smoking status
- ODS RTF for publication-ready tables

## Selected Results
- No patient underwent more than two procedures on a single day
- Clear demographic differences by race, sex, and smoking history
- Smoking history was associated with younger age at death
  (two-sample t-test, p < 0.001)

## Software
- SAS (Base SAS, PROC FREQ, TABULATE, MEANS)