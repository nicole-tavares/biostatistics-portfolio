# Survival Analysis Midodrine Use on ED Transfer Time

## Overview
This project evaluates whether midodrine use is associated with time to transfer
from the emergency department (ED) using survival analysis methods implemented in R.

A retrospective cohort of adult ED patients was followed for up to two hours
after presentation. Midodrine use was treated as a time-dependent covariate.

## Methods
- Time-to-event analysis with transfer from the ED as the event
- Time-dependent Cox proportional hazards model
- Adjustment for demographic and clinical covariates
- Hazard ratios with 95% confidence intervals reported

## Results
After adjustment for clinical factors, midodrine use was not significantly
associated with time to transfer from the ED. Several physiologic and clinical
variables were identified as important predictors of transfer.

Full tables and figures are provided in the accompanying report.

## Software
- R
- survival package
