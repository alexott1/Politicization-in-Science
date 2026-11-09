# Politicization-in-Science
Termpaper:What about the Politicization of Science in Europe? Evaluating public trust in Science, Germany from 1980 – 2021.

Replication code for the paper *"What about the Politicization of Science in Europe? Evaluating public trust in Science, Germany from 1980–2021"* by Alexander Ott.

The paper replicates the study by Gauchat (2012) on the politicization of science in the US for Germany. It uses the German General Social Survey (ALLBUS) to examine whether trust in universities and higher education differs between political-ideological subgroups (left / moderate / right, conservative) and whether the founding of the AfD in 2013 marks a break in trust among the political right.

---

## Repository structure

```
.
├── README.md
├── analysis.R          # complete analysis: data preparation, figures, descriptives, models
├── data/               # place the ALLBUS files here (not included, see below)
└── output/             # optional: exported figures and tables
```

## Data

The analysis uses two ALLBUS data sets provided by GESIS – Leibniz Institute for the Social Sciences:

| Data set | Study number | Version | File |
|---|---|---|---|
| ALLBUS cumulation 1980–2018 | ZA5274 | 1.1.0 | `ZA5274_v1-1-0.dta` (Stata) |
| ALLBUS 2021 | ZA5280 | 2.0.0 | `ZA5280_v2-0-0.sav` (SPSS) |

**The data are not included in this repository.** ALLBUS data may not be redistributed. They are free for academic use after registration at the [GESIS Data Archive](https://search.gesis.org/). Download both files and place them in the `data/` folder.

## Requirements

- R (≥ 4.0) — developed with RStudio
- R packages:

```r
install.packages(c("tidyverse", "haven", "lme4", "lmerTest", "gridExtra", "sjPlot"))
```

## How to run

1. Clone the repository and open the folder as an RStudio project.
2. Put the two ALLBUS files into `data/`.
3. Run `analysis.R` from top to bottom (takes about 2 minutes). All results are written to `output/`.

| Section in `analysis.R` | Output |
|---|---|
| 1–3 Data preparation | merges both surveys, recodes missing values, builds dummies and the conservatism index |
| 4 Descriptive figures | `fig_ideology_distribution.png`, `fig_afd_voters.png`, `fig_trust_ideology.png`, `fig_trust_conservative.png` |
| 5 Descriptive table | `descriptives.csv` (model sample) |
| 6 Models | `table_logit.html` (mixed-effects logit models 1–3), `table_lpm.html` (linear probability models, robustness) |
| 7 Average marginal effects | `ame_model1.csv` |

## Key variables

| Variable | ALLBUS source | Coding |
|---|---|---|
| `pt11_01` (dependent variable) | `pt11` – trust in universities / higher education (1–7) | 1 = 5–7 (trust), 0 = 1–4 |
| `left`, `moderate`, `right` | `pa01` – left-right self-placement (1–10) | left = 1–3, moderate = 4–7 (reference), right = 8–10 |
| `moderately_conservative` / `strictly_conservative` | 12 items, based on Minkenberg (1992) | share of fulfilled items among answered items ≥ 4/12 / ≥ 5/12 (at least 5 items answered) |
| `after_afd` | `year` | 1 = survey year 2013 or later (after the founding of the AfD) |
| `time10` | `year` | decades since 1980 |
| `cohort` | `year − age` | random intercept in the mixed models |
| `age_c`, `agesq_c` | `age` | age in decades, centered at 50 years, and its square |
| `edu_upper_sec`, `edu_post_sec`, `edu_tertiary` | `iscd975` (2021: `isced97`) | ISCED 3, 4, 5–6; reference: ISCED 1–2 |
| `rp01_01` | `rp01` – church attendance | 1 = at least once a month |
| `z_income` | `di01a` (2021 additionally `incc`) | log net income, z-standardized within each survey year |
| `female`, `east` | `sex`, `eastwest` | dummies |

All negative values are ALLBUS missing codes and are set to `NA`. The dependent variable was collected in 1984, 1994, 2000, 2002, 2008, 2012, 2018 and 2021. The left-right scale is missing in 1984, so the models cover 1994–2021.

## Method

Following Gauchat (2012), the models are hierarchical age-period-cohort models (Yang & Land 2006, 2008): period and age enter as fixed effects, birth cohort as a random intercept. Because the dependent variable is binary, the main models are logistic mixed models (`glmer`, `family = binomial`). Linear mixed models (`lmer`) serve as a robustness check.

- **Model 1:** main effects
- **Model 2:** + ideology × time
- **Model 3:** + ideology × after AfD founding

Descriptive figures for "Germany" are weighted with the east-west weight `wghtpew`, because East Germans are oversampled in ALLBUS.

### Notes on the data

- The 12 conservatism items are not asked in every survey year (e.g. only 5–7 in 2012). The index therefore uses the share of fulfilled items rather than a sum.
- In ALLBUS 2021 the ISCED variable is called `isced97` and the open income question was answered by only ~25 % of respondents. The script harmonizes both.
- ALLBUS 2021 was conducted as a self-administered survey (online/mail) instead of face-to-face interviews.

## References

- Gauchat, G. (2012). Politicization of Science in the Public Sphere: A Study of Public Trust in the United States, 1974 to 2010. *American Sociological Review*, 77(2), 167–187.
- Minkenberg, M. (1992). The New Right in Germany: The Transformation of Conservatism and the Extreme Right. *European Journal of Political Research*, 22(1), 55–81.
- Yang, Y., & Land, K. C. (2006). A Mixed Models Approach to the Age-Period-Cohort Analysis of Repeated Cross-Section Surveys. *Sociological Methodology*, 36, 75–97.
- Yang, Y., & Land, K. C. (2008). Age-Period-Cohort Analysis of Repeated Cross-Section Surveys: Fixed or Random Effects? *Sociological Methods & Research*, 36(3), 297–326.
- GESIS – Leibniz Institute for the Social Sciences. ALLBUS cumulation 1980–2018 (ZA5274) and ALLBUS 2021 (ZA5280).

## Author

Alexander Ott
