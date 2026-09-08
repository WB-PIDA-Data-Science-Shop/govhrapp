<!-- badges: start -->
[![R-CMD-check](https://github.com/WB-PIDA-Data-Science-Shop/govhrapp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/WB-PIDA-Data-Science-Shop/govhrapp/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/WB-PIDA-Data-Science-Shop/govhrapp/graph/badge.svg)](https://app.codecov.io/gh/WB-PIDA-Data-Science-Shop/govhrapp)
<!-- badges: end -->

# govhrapp

`govhrapp` is a World Bank package of interactive Shiny applications for quality checking and analyzing government human resources data. It ships two independent application suites, each with its own entry point and Posit Connect deployment:

| Suite | Entry point | Purpose |
|---|---|---|
| [Quality Check](#quality-check-suite) | `run_govhrapp_qcheck()` | Diagnose coverage, consistency and validity of the raw data |
| [Standard Analytics](#standard-analytics-suite) | `run_govhrapp_analytics()` | Analyze wage bill and workforce dynamics |

The suites are country-agnostic: they run on any HR data harmonized to the `govhr` data template — an **establishment**, a **personnel** and a **contract** module. The companion [`govhr`](https://github.com/WB-PIDA-Data-Science-Shop/govhr) package defines that template and supplies the underlying computations; `govhrapp` provides the visual interface.

---

## Installation

`govhrapp` can be installed from GitHub:

```r
remotes::install_github("WB-PIDA-Data-Science-Shop/govhrapp")
```

---

## Quality Check suite

Identifies data quality issues, inconsistencies and gaps before analysis. Each tab applies its diagnostic to all three modules, so results can be inspected module by module.

| Tab | What it does |
|---|---|
| **Coverage** | Share of complete values by module, variable and group, over time |
| **Consistency** | Record- and value-level consistency over time — duplicate records and values that change when they should not |
| **Validation** | Pass rates against the `govhr` personnel and contract rule sets, with per-rule download of violating records as `.xlsx` |

```r
run_govhrapp_qcheck(
  est_data,
  personnel_data,
  contract_data,
  personnel_validation,  # govhr::validate_data(personnel_data, personnel_rules)
  contract_validation    # govhr::validate_data(contract_data, contract_rules)
)
```

---

## Standard Analytics suite

A standard set of indicators and visualizations on the public sector wage bill and workforce, based on the World Bank's [Public Sector Employment and Compensation Assessment Framework](https://documents1.worldbank.org/curated/en/324801640074379484/pdf/Public-Sector-Employment-and-Compensation-An-Assessment-Framework.pdf).

| Tab | Panels |
|---|---|
| **Overview** | Headline headcount and wage bill indicators |
| **Workforce** | *Overview* (headcount levels, shares, growth) · *Movement* (hiring and exit rates) · *Transitions* (movement between groups over time) · *Retirement* (actual and projected) |
| **Wage Bill** | *Overview* (levels, shares, growth) · *Equity* (distributions, deciles, compression ratios) · *Movement* (labor movement costs) · *Retirement* (current and projected pension liabilities) |

```r
run_govhrapp_analytics(
  workforce_data,  # personnel attributes (headcount)
  wagebill_data,   # contract and salary attributes (wage bill)
  cache = build_analytics_cache(workforce_data, wagebill_data)
)
```

`build_analytics_cache()` precomputes a data cache, which improves performance of the dashboard. `generate_analytics_report(workforce_data, wagebill_data)` renders that same cache to a Word document.

---

## Running locally

Each suite has a sourceable demo script under [`inst/app/`](inst/app/) that loads the package, prepares example data and launches the app. These are the same scripts Posit Connect deploys.

The scripts call `pkgload::load_all(".")`, so **run them from the package root** — there is no `app.R` and the RStudio/Positron "Run App" button is not wired up:

```r
source("inst/app/qcheck/qcheck_app.R")        # Quality Check
source("inst/app/analytics/analytics_app.R")  # Standard Analytics
```

Once the package is loaded, `run_qcheck_demo()` and `run_analytics_demo()` launch either suite on the bundled example data without going through the scripts.

To run against your own data, load the package and call an entry point directly:

```r
pkgload::load_all(".")

run_govhrapp_qcheck(
  est_data, personnel_data, contract_data,
  personnel_validation = govhr::validate_data(personnel_data, govhr::personnel_rules),
  contract_validation  = govhr::validate_data(contract_data, govhr::contract_rules)
)

run_govhrapp_analytics(workforce_data, wagebill_data)
```

Run tests with `devtools::test()`. `R CMD check` runs on every push via GitHub Actions.

---

## Directory structure

```
govhrapp/
├── DESCRIPTION                       # Metadata, imports, govhr remote
├── NAMESPACE                         # Exports (roxygen2-generated)
├── manifest.json                     # Posit Connect bundle manifest
├── renv.lock                         # Pinned versions for reproducible deploys
│
├── R/
│   ├── analytics_app.R               # run_govhrapp_analytics(): navbar shell and theme
│   ├── analytics_overview.R          # Overview tab
│   ├── analytics_workforce*.R        # Workforce tab and its four panels
│   ├── analytics_wagebill*.R         # Wage Bill tab and its four panels
│   ├── analytics_plots.R             # Analytics plot builders and summary helpers
│   ├── analytics_report.R            # generate_analytics_report(): Word report
│   ├── demo.R                        # run_qcheck_demo(), run_analytics_demo()
│   ├── analytics_utils.R             # Shared analytics helpers
│   ├── qcheck_app.R                  # run_govhrapp_qcheck(): navbar shell and caching
│   ├── qcheck_coverage.R             # Coverage tab and per-module panel
│   ├── qcheck_consistency.R          # Consistency tab and per-module panel
│   ├── qcheck_validation.R           # Validation tab and violation downloads
│   ├── qcheck_plots.R                # Coverage and consistency plots
│   ├── qcheck_helpers.R              # Quality check value boxes
│   ├── server_utils.R                # Filtering, group controls, build_analytics_cache()
│   ├── ui_utils.R                    # Shared sidebar controls
│   ├── govhr_port.R                  # Decile, percentile and compression ratio helpers
│   ├── deploy.R                      # deploy_govhrapp()
│   └── zzz.R                         # globalVariables() for R CMD check
│
├── inst/
│   ├── app/
│   │   ├── qcheck/qcheck_app.R       # Entry script: data prep + run_govhrapp_qcheck()
│   │   └── analytics/analytics_app.R # Entry script: data prep + run_govhrapp_analytics()
│   ├── markdown/                     # In-app copy (.md) and report templates (.qmd)
│   ├── www/                          # styles.css and logos, served at /assets
│   └── db/govhrapp.sqlite            # Bundled database (not referenced by app code)
│
├── man/                              # Roxygen-generated documentation
├── tests/testthat/                   # testthat suite
├── renv/                             # renv project library metadata
└── .github/workflows/                # CI: R CMD check and coverage
```

---

## Deployment to Posit Connect

`deploy_govhrapp(suite, type)` deploys a suite to Posit Connect. Each suite has a **dev** and a **prod** deployment, so four GUIDs are read from `.Renviron` (open it with `usethis::edit_r_environ()`):

```
govhrapp_qcheck_dev_guid=<guid>
govhrapp_qcheck_prod_guid=<guid>
govhrapp_analytics_dev_guid=<guid>
govhrapp_analytics_prod_guid=<guid>
```

Deployment targets the server registered as `internal-server`; register it once with `rsconnect::addServer()` and `rsconnect::connectApiUser()` if needed. Then:

```r
pkgload::load_all(".")

deploy_govhrapp("qcheck",    type = "dev")   # type defaults to "dev"
deploy_govhrapp("analytics", type = "prod")
```

This bundles the full package directory and updates the existing deployment in place, using the suite's script in `inst/app/` as `appPrimaryDoc`. Notes:

- `renv.lock` pins the versions Connect installs; the server must be able to reach the `govhr` GitHub remote.
- The entry scripts call `pkgload::load_all(".")`, so `pkgload` must be installed on the server.
- `generate_analytics_report()` renders `inst/markdown/analytics_report.qmd` via `rmarkdown::render()`, so Quarto must be available wherever reports are generated.
