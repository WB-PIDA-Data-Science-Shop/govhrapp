# govhrapp 0.1.9
* This patch introduces the following enhancements:
    * Adds an optional "Shares by:" breakdown to the overview, retirement and movement panels: each category's share of the measure over time, one chart per group.
    * Adds pensioners and pension payments over time to the retirement panels.
    * Adds a "Filter by:" population filter to every panel, separate from "Group by:".
    * Filter and subgroup choices list only the values in the filtered population and dates.
    * Shows a message, rather than an error, when no records match the filters.
    * Adds optional password protection to `run_govhrapp_qcheck()`, as in `run_govhrapp_analytics()`.
    * Checks the data for required columns before the analytics app starts.
    * Supports govhr 0.4.4, including its new movement definitions and trend charts.
    * Uses consistent terms across the app text and documentation.
* Breaking changes:
    * Renames `run_wagebillapp()` to `run_wagebill_app()`. The old name is deprecated.
    * Exports `run_workforce_app()` and `workforce_ui()`, and no longer exports `workforce_transition_server()`.
* Bug fixes:
    * Counts only active personnel in the Overview tab and the overview and equity panels, so pensions no longer inflate the wage bill or pay distribution.
    * Fixes the sidebar filters and movement value boxes on database tables, and the display of `Inf` values.
    * Fixes the standalone coverage, consistency and validation apps.

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
