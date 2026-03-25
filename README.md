# European Hake Abundance Modelling Across the Mediterranean Sea

**Bachelor's Thesis — University of Glasgow**  
**Author:** Davide Rossi

---

## Overview

This thesis analyses the environmental and temporal drivers of European hake (*Merluccius merluccius*) abundance across the Mediterranean Sea over the period 2000–2021, using data from bottom trawl surveys.

The core methodological contribution is a novel EM-based estimation algorithm for **Zero-Inflated Negative Binomial Mixture-of-Experts (ZINBMoE)** models with GAM experts and gating networks, extending existing GLM-based frameworks to support flexible, nonlinear modelling of overdispersed, zero-inflated count data.

---

## Repository Structure
```
Thesis-Project-6/
├── data/           # Raw and processed trawl survey data
├── scripts/        # R scripts (modelling) & Python scripts (data retrieval)
├── figures/        # Spatio-temporal maps and plots
└── results/        # Model outputs and validation metrics
```

---

## Methods

- Zero-Inflated Negative Binomial GAM (ZINB-GAM)
- Mixture-of-Experts models (ZINBMoE, ZINBMoE2)
- EM algorithm with penalised likelihood M-steps
- Models fitted in **R** (`mgcv`); data retrieved via API using **Python**
- Training period: 2000–2020 | Validation: 2021

---

## Author

**Davide Rossi** | University of Glasgow
