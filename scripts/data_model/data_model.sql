DROP DATABASE IF EXISTS covid_trial_tracker;

CREATE DATABASE covid_trial_tracker;

\c covid_trial_tracker

CREATE TABLE IF NOT EXISTS trk_staging_trials (
  id                       SERIAL PRIMARY KEY,
  source_registry          VARCHAR(200),
  trial_id                 VARCHAR(200),
  public_title             VARCHAR(200),
  scientific_title         VARCHAR(200),
  institution              VARCHAR(200),
  date_registered          VARCHAR(200),
  date_updated             VARCHAR(200),
  trial_start_date         VARCHAR(200),
  date_primary_completion  VARCHAR(200),
  therapy_target           VARCHAR(200),
  recruitment_status       VARCHAR(200), 
  expected_enrollment      VARCHAR(200),
  tx6_category             VARCHAR(200),
  tx7_category             VARCHAR(200),
  age                      VARCHAR(200),
  covid19_status           VARCHAR(200),
  patient_setting          VARCHAR(200),
  study_design_final       VARCHAR(200),
  blinding_final           VARCHAR(200),
  number_of_arms_final     VARCHAR(200),
  state_name               VARCHAR(200),
  state_lon                VARCHAR(200),
  state_lat                VARCHAR(200),
  country_name             VARCHAR(200),
  iso3_code                VARCHAR(200),
  pdf_link                 VARCHAR(200),
  corrected_treatment_name VARCHAR(200),
  corrected_treatment_name_display VARCHAR(200),
  tx1_category             VARCHAR(200),
  tx2_category             VARCHAR(200),
  tx3_category             VARCHAR(200),
  tx4_category             VARCHAR(200),
  count                    VARCHAR(200),
  phase                    VARCHAR(200),
  outcome                  VARCHAR(200)
); 

CREATE TABLE IF NOT EXISTS trk_trials (
  id                       SERIAL PRIMARY KEY,
  source_registry          VARCHAR(1000),
  trial_id                 VARCHAR(1000),
  public_title             VARCHAR(1000),
  scientific_title         VARCHAR(1000),
  institution              VARCHAR(1000),
  date_registered          DATE,
  date_updated             DATE,
  trial_start_date         DATE,
  date_primary_completion  DATE,
  therapy_target           VARCHAR(1000),
  recruitment_status       VARCHAR(1000), 
  expected_enrollment      INTEGER,
  tx6_category             VARCHAR(1000),
  tx7_category             VARCHAR(1000),
  age                      VARCHAR(1000),
  covid19_status           VARCHAR(1000),
  patient_setting          VARCHAR(1000),
  study_design_final       VARCHAR(1000),
  blinding_final           VARCHAR(1000),
  number_of_arms_final     INTEGER,
  state_name               VARCHAR(1000),
  state_lon                DECIMAL,
  state_lat                DECIMAL,
  country_name             VARCHAR(1000),
  iso3_code                VARCHAR(1000),
  pdf_link                 VARCHAR(1000),
  corrected_treatment_name VARCHAR(1000),
  corrected_treatment_name_display VARCHAR(1000),
  tx1_category             VARCHAR(1000),
  tx2_category             VARCHAR(1000),
  tx3_category             VARCHAR(1000),
  tx4_category             VARCHAR(1000),
  count                    DECIMAL,
  phase                    VARCHAR(1000),
  outcome                  VARCHAR(1000),
  source                   VARCHAR(1000),
  date_created             DATE
); 

CREATE TABLE IF NOT EXISTS trk_reviews (
  flagging_id              SERIAL PRIMARY KEY,
  trial_id                 VARCHAR(1000),
  flag                     BOOLEAN,
  rating                   INTEGER,
  user_submitted           VARCHAR(1000),
  source                   VARCHAR(1000),
  note                     VARCHAR(1000),
  date_created             DATE     
); 

CREATE VIEW trials_view AS 
SELECT * FROM trk_trials
WHERE source = 'Cytel'
GROUP BY id
HAVING date_created = MAX(date_created);
-- UNION
-- SELECT * FROM trials
-- WHERE source = 'source_2'
-- GROUP BY id
-- HAVING date_created = MAX(date_created);

CREATE VIEW review_view AS 
SELECT * FROM trk_reviews
WHERE source = 'Cytel'
GROUP BY flagging_id
HAVING date_created = MAX(date_created);
-- UNION
-- SELECT * FROM reviews
-- WHERE source = 'source_2'
-- GROUP BY flagging_id
-- HAVING date_created = MAX(date_created);

CREATE VIEW combined_view AS 
SELECT t.*, r.flagging_id, r.flag, r.rating, r.user_submitted, r.source as review_source, r.note, r.date_created AS review_date_created FROM trials_view t
 LEFT JOIN review_view r ON t.trial_id = r.trial_id AND t.source = r.source;