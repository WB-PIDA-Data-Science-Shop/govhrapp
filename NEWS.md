# govhrapp 0.1.5

* This patch introduces the following enhancements:
    * Improves the performance of the analytics suite by serving the key indicator boxes from the pre-computed cache and sharing each panel's aggregate across its plots.
    * Standardizes the documentation and argument names of the analytics functions. This is a breaking change: the data argument is now `.data`, a single grouping column is now `group_col`, and the unused `build_wagebill_group_choices()` is removed.
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
