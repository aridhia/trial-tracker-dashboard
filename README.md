# Introduction

A triage tool for clinical trials of interest. Intermediary between feeds of clinical trial metadata and a CRM-like system to follow up with individual trials. The purpose of this application is to help a team screen clinical trials by reviewing metadata. Screening is supported in a number of ways:

- Filtering trials by their parameters
- User or scripted flags on trials of interest

This app was developed by as part of the COVID-19 Workbench technology programme.

## Database

Clinical trial data is loaded into a PostgreSQL database. When running standalone, this should be called `tracker`. Assuming the database exists and the user can create tables etc:
```sh
psql tracker < script/data_model.sql
```

Staging tables, designed to be overwritten:

- staging_trials

Main tables designed to be appended to (multiple points in time):

- trials
- reviews

And views the show the latest values from the main tables 

- trials_view
- review_view
- combined_view

## Staging data - Cytel

The Cytel tracker data is provided in an .RData file in a parallel folder. This needs to be staged at regular intervals to update the main database.

### Pre-requistes

The [ELT](https://en.wikipedia.org/wiki/Extract,_load,_transform) scripts for the Cytel tracker data require:

- R 3.6.3
- R DBI
- R PostgreSQL
- R loggit

### Staging

The Cytel data is staged to the database table `staging_trials` using the script [`scripts/cytel/staging.R`](./scripts/cytel/staging.R):

```sh
Rscript scripts/cytel/staging.R
```
Incoming files are copied to the `archive` folder, and given name with the patter `cytel-<md5sum>.RData` - where `md5sum` is the md5sum of the original file.

During the staging process in-bound data is validated against a data dictionary [`dat.json`](./scripts/cytel/dat.json) that contains information on names and types of inbound fields.

### Loading

The staged data is then appended to the main `trials` table with the source `cytel` and a timestamp.

TODO

## Shiny application

TODO
