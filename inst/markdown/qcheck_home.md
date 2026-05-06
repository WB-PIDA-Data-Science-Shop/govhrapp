This dashboard provides a comprehensive quality control assessment of harmonized payroll data comprising the Contract, Personnel, and Establishment modules according to the standard harmonization dictionary.

#### Purpose

All diagnostic tables and visualizations are generated automatically from the govhrapp R package, which evaluates the harmonized modules to identify data quality issues, inconsistencies, and patterns requiring attention in a static report. This dashboard provides an interactive interface which provides further flexibility to our users to further customize data quality assurance to suit the unique needs of their country contexts. 

#### Report Structure

The quality control dashboard is organized into three main sections:

##### 1. Data Basics
Foundational diagnostics including module dimensions, variable structure and dictionary conformity, primary key integrity, cross-module orphan checks, salary logic validation, and date logic checks. This section provides essential structural and consistency assessments before diving into detailed data quality patterns.

##### 2. Data Coverage Report
Comprehensive measurement and visualization of missing data patterns overall and across key analytical dimensions (occupation, ISCO mapping, reference period, and establishment). Helps identify variables requiring cleaning, imputation, or further validation. Regression analytics may be used to study non-random coverage gaps in this section as well. 

##### 3. Data Validation
Rule-based quality checks applied separately to the Contract and Personnel modules. Each rule is evaluated against all records and summarised with a pass rate, pass/fail counts, and a status badge (Pass = 100%, Warning ≥ 80%, Fail < 80%). Rules that cannot be evaluated due to missing variables are flagged as errors. Summary indicators at the top of the tab show the overall weighted pass rate for each module.

##### 4. Volatility Analysis
Time-series diagnostics evaluating temporal stability of salary aggregates, contract counts, and work hours across reference periods as well as other fields as selected by the user. High volatility may indicate data quality issues, organizational changes, or policy shifts requiring investigation.



