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
- `05_post_process`: the disclosure rules differ between income and wealth
  (e.g. `not_disclose` vs `reason_of_na` in the same rule).
- Fixed on 30-10-2026: the wealth grid referred to the data source
  `age35_cohxort` (typo) for `c35_individual_income`, so those models could not run.

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
