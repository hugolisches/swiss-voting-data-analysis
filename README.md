# Swiss Voting Data Analysis

## Project Overview

This project was developed as part of a Data Analysis course during my exchange semester at Humboldt University of Berlin.

The goal was to analyze municipality-level voting patterns in Switzerland for the popular initiative *“Keine 10-Millionen-Schweiz! (Nachhaltigkeitsinitiative)”*.

The analysis examines how three municipality-level characteristics are related to the share of Yes votes:

- share of foreign residents
- settlement pressure
- city status

The project combines data preparation, descriptive analysis, correlation analysis, multiple linear regression, robustness checks and model diagnostics.

## Research Question

The main research question is:

> How are the share of foreign residents, settlement pressure and city status of a Swiss municipality associated with its share of Yes votes?

The analysis is descriptive and focuses on statistical relationships at the municipality level.

## Data

The project combines several public datasets, including:

- municipality-level voting results
- population data
- building-zone data
- statistical city classifications

The datasets come from different sources and reference years, so they required harmonization before they could be combined.

An important part of the project was adjusting municipality identifiers and values to account for:

- municipality mergers
- newly created municipalities
- territorial changes
- changes in canton assignment

## Data Preparation

The data preparation script is stored in:

```text
src/
