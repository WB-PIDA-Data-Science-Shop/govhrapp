This dashboard provides a comprehensive quality control assessment of harmonized payroll data comprising the Contract, Personnel, and Establishment modules according to the standard harmonization dictionary.

#### Purpose

All diagnostic tables and visualizations are generated automatically from the govhr R package, which evaluates the harmonized modules to identify data quality issues, inconsistencies, and patterns requiring attention in a static report. This dashboard provides an interactive interface which provides further flexibility to our users to further customize data quality assurance to suit the unique needs of their country contexts. 

#### Report Structure

The quality control dashboard is organized into four main sections:

##### 1. Data Basics
Foundational diagnostics including module dimensions, variable structure and dictionary conformity, primary key integrity, cross-module orphan checks, salary logic validation, and date logic checks. This section provides essential structural and consistency assessments before diving into detailed data quality patterns.

##### 2. Data Coverage (Missingness) Report
Comprehensive measurement and visualization of missing data patterns overall and across key analytical dimensions (occupation, ISCO mapping, reference period, and establishment). Helps identify variables requiring cleaning, imputation, or further validation. Regression analytics may be used to study non-random coverage gaps in this section as well. 

##### 3. Volatility Analysis
Time-series diagnostics evaluating temporal stability of salary aggregates, contract counts, and work hours across reference periods as well as other fields as selected by the user. High volatility may indicate data quality issues, organizational changes, or policy shifts requiring investigation.

