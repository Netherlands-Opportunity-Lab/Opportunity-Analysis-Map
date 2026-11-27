# Parallel estimation for the Kansenkaart on the OSSC

![pipeline.png](pipeline.png)

This repository holds the code for parallel estimation of the Kansenkaart project estimates on the ODISSEI Secure Supercomputer.

This is a work in progress!


## Steps to run the analysis

Paths are in `config/A_2_0_M1_config.yml` (`config/config.yml` in the CBS RA),
not in the scripts. One set of scripts per parental SES measure:
`src/income_parents/`, `src/wealth_parents/`, `src/educ_parents/`.

| Step | Where | Script | Output |
|---|---|---|---|
| 00 | CBS RA | `src/A_2_0_M1_00_preprocess_cohorts.R` | slimmed cohort files in `input/` |
| 02 | CBS RA | `src/<ses>_parents/A_2_0_M1_02_create_grid_<ses>.R` | `input/<ses>/model_grid.feather` |
| 03 | OSSC | `<ses>_job.sh` runs `src/<ses>_parents/A_2_0_M1_03_array_job_<ses>.R` per chunk (uses `01_expectation_function`) | `output/<ses>/out_matrix_*.rds` |
| 04 | OSSC | `src/<ses>_parents/A_2_0_M1_04_combine_results_<ses>.R` | combined results |
| 05 | OSSC | `src/<ses>_parents/A_2_0_M1_05_post_process_<ses>.R` | `export/<ses>/` |

### OSSC running
1. Copy `input/` to the OSSC and run from the repository root there (`$PROJECT`).
2. Test one chunk: `Rscript src/income_parents/A_2_0_M1_03_array_job_income.R 1 input`.
3. Number of array jobs: `src/A_2_0_M1_slurm_command.R` (`nrow(model_grid) / chunk_size`).
4. Submit: `sbatch --array=1-<n> income_job.sh` (likewise `wealth_job.sh`, `educ_job.sh`).

### Versions and archive
A version of the scripts is archived as a **git tag** when results are
exported, not as copies in an `archive/` folder. The old `archive/` folder
(which the job scripts ran until 30-10-2026) is kept in the tag
`archive-snapshot-2026-10-30`.

### Known differences between the income, wealth and education scripts
The three sets were copies that have drifted apart. They are not merged into
one parametrised set, because that would change the results of at least one
of them. Differences to resolve first:
- `02_create_grid`: only income removes the COROP level for cohorts other than
  age35, and income and wealth select different columns from the COROP and
  PC6-gwb tables.
- `03_array_job`: income and education use 64 cores per node, wealth 96.
- Fixed on 27-11-2026: the disclosure rules of `05_post_process` differed
  between the three (in education and wealth, rule 4 also suppressed
  `_class_` / `_neighborhood_` outcomes with an estimate of 0, without a
  reason). All three now call `kaart_suppress()` from the shared
  `src/lib/suppression_functions.R` (see below).
- Fixed on 30-10-2026: the wealth grid referred to the data source
  `age35_cohxort` (typo) for `c35_individual_income`, so those models could not run.

## Suppression (CBS output rules)

Since 27-11-2026 every results table has the column `reason_of_na`: **all** the reasons why a cell is not published, ordered by code and separated by `; ` (e.g. `1. fewer than 50 obs; 2. est*n < 10`), or empty when the cell is published. Suppressed cells keep their row with the statistics set to `NA`, also in the export, so every missing number can be traced to its reason. The rules and codes are in `src/lib/suppression_functions.R`, a shared file that is the same in Opportunity-Analysis-Gap, -Intragenerational and -Map (`tests/testthat/test-structure.R` checks this when the repositories are cloned next to each other); the thresholds are in the config.

| Code | Reason |
|---|---|
| `0. no estimate` | the model gave no estimate (Map) |
| `0. not computed: fewer than 50 obs in the subgroup` | the subgroup is too small for a mean (Gap, Intragenerational) |
| `1. fewer than k obs` | N below the minimum per cell (50 in Gap and Intragenerational; 11 for a mean and 12 for a regression in Map) |
| `2. est*n < 10` | binary outcome: too few cases |
| `3. (1-est)*n < 10` | binary outcome: too few non-cases |
| `4. est == 0` | continuous estimate of exactly 0 (Map) |
| `5. n of school < 3` | PC4 area with fewer than 3 schools (Map, `_class_` outcomes) |
| `6. sd == 0` | no variation in the outcome (Intragenerational) |
| `7. fewer than 3 non-zero x values` | (Intragenerational) |
| `8. other cell of the gradient or group suppressed` | complementary suppression: a gradient (all bins) or group (all categories) is published whole or not at all |

In Map the rules are applied in `05_post_process` (national kaart) and in `JGZ/Scripts/kaart/jgz_kaart_functions.R` (`kaart_disclosure()`, JGZ kaart, including the bereik outcomes), both with `kaart_suppress()`. A cell with one reason has the same text as before 27-11-2026; before, only the first rule that applied was recorded, rule 5 overwrote an earlier reason, and a cell without an estimate kept its `n` without a reason (now `0. no estimate`, `n` NA). Suppressed cells have `est`, `lwr`, `upr` and `n` NA; `n` is rounded to tens.

## License

This repository uses two licenses:

- **Code** (scripts, notebooks, and other source files) is licensed under the [MIT License](LICENSE).
- **Results** (data outputs, figures, and other generated content) are licensed under [CC BY 4.0](LICENSE-RESULTS).

See the respective license files for full terms.

## Contact
This project is developed and maintained by the [ODISSEI Social Data Science
(SoDa)](https://odissei-data.nl/nl/soda/) team.

Do you have questions, suggestions, or remarks? File an issue in the issue
tracker or feel free to contact [Erik-Jan van
Kesteren](https://github.com/vankesteren)
([@ejvankesteren](https://twitter.com/ejvankesteren)) or [Helen Lam](https://github.com/Helenlam20).
