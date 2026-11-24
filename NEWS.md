# govhrapp (development version)
* This patch introduces the following enhancements:
    * Supports govhr 0.4.4: the workforce transition panel counts moves from `compute_transition()`'s new totals, and the Overview tab draws its charts with `govhr::plot_trend()`.
    * Updates the movement help text: separations no longer include retirements, and rates divide by the previous period's headcount.
    * Shows "No separations" instead of "Inf" when the replacement rate has no separations to divide by.
    * Checks that the data has the columns the analytics app needs before it starts, and names any that are missing.
    * Rewrites the function documentation in American English with sentence-case titles, and lists the required columns of each input.
* Breaking changes:
    * `run_wagebillapp()` is renamed `run_wagebill_app()`. The old name still works, with a deprecation warning.
    * `run_workforce_app()` and `workforce_ui()` are now exported, to match `run_wagebill_app()` and `wagebill_ui()`.
    * `workforce_transition_server()` is no longer exported, like the other panel servers.
* Bug fixes:
    * The movement value boxes report the same date for data frames and database tables.
    * The standalone coverage, consistency and validation apps pass their data and cache correctly.
    * The pay measure and grouping pickers read the column names of database tables.

# govhrapp 0.1.8
* This patch introduces the following enhancements:
    * Fully port `govhr` functions, implementing a `dbi` compatible method.
    * Improve user experience and interface.

# govhrapp 0.1.7
* This patch introduces the following enhancements:
    * Fixes UI issues described in #47.
    * Prerenders profile of new hires.
    * Adds average wages to the Overview tab.

# govhrapp 0.1.6
* This patch introduces the following enhancements:
    * Port all data transformation and visualization functions to `govhr`.
    * Fixes minor bugs in the `qcheck` `consistency` tab.
    
# govhrapp 0.1.5

* This patch introduces the following enhancements:
    * Improves the performance of the analytics suite by serving the key indicator boxes from the pre-computed cache and sharing each panel's aggregate across its plots.
    * Standardizes the documentation and argument names of the analytics functions.
    * Adds tests for the helpers shared across the analytics modules.
    * Fixes errors in the wage bill equity and workforce retirement panels.

# govhrapp 0.1.4
* This patch introduces the following enhancements:
    * Refactors the `qcheck` suite into three separate modules: coverage, consistency, and completeness.
    * Improves the UI for controlling visualizations.
    * Refactors implementation of `compute_*` functions to `data.table`.

# govhrapp 0.1.3

* This patch introduces the following enhancements:
    * Refactors the `workforce` and `wagebill` modules.
    * Introduces a set of new back-end functions that enable the computation of additional summary statistics.

# govhrapp 0.1.2

* This patch introduces the following enhancements:
    * Refactors the `qcheck` suite into three separate modules: coverage, consistency, and completeness.
    * Improves the UI for controlling visualizations.
    * Refactors implementation of `compute_*` functions to `data.table`.
    
# govhrapp 0.1.1

* Initial release.
