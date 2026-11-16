# JGZ analyses and export

Analyses of the youth health care (JGZ) pilot data. The preprocessing (raw
JGZ files -> panel, contact moments, catchment-area sample) is in
[Opportunity-Preprocessing/src/jgz](https://github.com/Netherlands-Opportunity-Lab/Opportunity-Preprocessing).

The R scripts below read `JGZ/config.yml` and are run from the repository
root:

```r
setwd("H:/Opportunity-Analysis-Map")
source("JGZ/Scripts/map/jgz_map_tables.R")
```

They are written for the CBS RA versions (R 3.6.2, dplyr 1.0.2).

## Inside CBS Remote Access

| Script | Input | Output | What it does |
|---|---|---|---|
| `kaart/jgz_kaart.R` | `output/jgz_cohort.rds` (preprocessing, cohort `jgz`) | `jgz_kaart_estimates.rds` (internal), `jgz_kaart_results.rds/.csv` | **Kaart with the kansenkaart method** (16-11-2026): per outcome, area (organisation, PC3, PC4 at the contact used for that age) and group, `kansenkaart_expect()` of the national kaart (`src/income_parents/*_01_expectation_function_income.R`): the mean for income "all", `lm(outcome ~ income_parents_perc)` predicted at 25/50/75 for Low/Mid/High. Only children seen by an organisation in `export_orgs`. CBS rules as the national post-processing (rules 1-4, `n` rounded to tens). Groups and rules in `config.yml` (`kaart_*`). Functions in `kaart/jgz_kaart_functions.R`, tests in `tests/testthat/test-jgz-kaart.R` |
| `map/jgz_map_tables.R` | `cm_<visit>.rds` (preprocessing 02) | `jgz_maps_{bmi,wiechen,sdq}.xlsx` | BMI / Wiechen / SDQ per PC3 and PC4, one measurement per child per visit (the one closest to the nominal age, `target_age`), small-cell suppression |
| `gradients/jgz_tidy_gradients.R` | gradient Excel files | `*_tidy.xlsx` | Reshapes gradient output (20 income bins) from wide to long |
| `gradients/jgz_check_gradients.R` | `*_tidy.xlsx` | `*_tidy_export.xlsx` | Combines speech/language files and runs the disclosure checks in `gradients/check_function.R` |
| `export/jgz_export_bundle.R` | map tables, checked gradients, proportions, bereik regressions, descriptives | export workbooks | Bundles everything for the organisations in `export_orgs` (config). Map workbooks keep the 2025 file and sheet names that `map/pc4_map_JGZ.R` reads |
| `regression/Uni and Multi regressions Do.do` | catchment sample (preprocessing 03) | bereik regression tables | Coen's bereik regressions |
| `descriptives/14102026_descriptivesscript.R` | catchment sample | descriptives tables | Characteristics of never/always/sometimes observed children |
| `jgz_helpers.R` | – | – | Config loading and disclosure helpers |

Not in this repository: the script that produces the gradient output that
`jgz_tidy_gradients.R` reads (Coen's "ANALYSIS GRADIENTS - cases" do-file).

## Outside CBS (on exported results)

| Script | What it does |
|---|---|
| `map/pc4_map_JGZ.R`, `map/2e_maps_absolute.R`, `map/2g_maps_selected.R` | Maps of the exported results |
| `webmapper/jgz_to_webmapper.R` | Exported results in the webmapper format |

## Open points

- **Kaart run time (benchmark on a synthetic cohort of 115,160 children, 68
  outcomes, 16-11-2026).** Default groups (income all/Low/Mid/High x sex
  all/Men/Women) for organisation, PC3 and PC4: ~145k estimates, ~8 minutes
  on one core here (the RA machine may be slower). The full national grid
  (432 groups: also migration and household type) is ~5 million estimates,
  roughly 3-7 hours on one core: still an overnight RA run, and most of those
  PC4 cells fail the CBS rules anyway. No supercomputer needed; if the full
  grid is wanted, parallelise over outcomes (parallel::parLapply) first.
- **Kaart income percentile.** `income_parents_perc` in the JGZ cohort is
  ranked within the JGZ cohort per birth year, not nationally, so Low/Mid/High
  are not the same incomes as on kansenkaart.nl. To decide with Bastian.
- **Kaart and the old map tables.** `map/jgz_map_tables.R` (shares from the
  `cm_<visit>` files) and the export bundle still use the old route; switch the
  export to the kaart results once they are checked in the RA.

- **Disclosure.** Tables are suppressed cell by cell; complementary cells
  (e.g. boys = total - girls) are not checked.
- **Wiechen export.** All three categories (+, M, -) are exported per item,
  so one can be derived from the other two (Bastian, 20 Oct 2026). Decide
  which two-way split is exported before the next export request.
- **Top-10% BMI.** `bmi_p90_by_sex`: within sex (current) or boys and girls
  together (Coen). The flag is `BMI > p90`, as in Coen's do-file.
- **BMI percentile gradients.** The p10-p90 columns hold the number of
  children above each percentile per income bin (the share in Coen's older
  version); `check_function.R` applies the small-cell rule to that count and
  to N - count.
