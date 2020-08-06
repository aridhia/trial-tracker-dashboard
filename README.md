# Introduction
A triage tool for clinical trials of interest. Intermediary between feeds of clinical trial metadata and a CRM-like system to follow up with individual trials.

# Development TODO List
Check out the Cytel Github in a parallel a folder.

- Script to: 
    - Extract the main table from the RData file to CSV.
    - Load into a Staging table in Postgres.
        Replace any prev staged data.

- Data Model for the app.
    - Main trial table.
    - Reviews table.
        - source
        - trial_id
        - flag (For inclusion)
        - Rating (0-10)
        - Note
        - created_date

- Script to:
    - Load from staging to the main trials table.
        - Append new records.
        - Add date_created and source columns 

- A Database view of the latest trials by source and trial_id 
- A Database view of the the latest reviews by source and trial_id 
- A Database view combining the latest trials and reviews.
- When you create a new reviews, app should prepopulate based on the old review (if any).  This review would then be submitted as a new record.

## Scripts
- SQL Script for the Data modelling.
- R script for staging.
- R script for loading.

