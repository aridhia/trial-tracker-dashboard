-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

CREATE TABLE clinical_trials (
  source_registry          VARCHAR(200),
  trial_id                 VARCHAR(200),
  public_title             VARCHAR(200),
  scientific_title         VARCHAR(200),
  institution              VARCHAR(200),
  date_registered          DATE,
  date_updated             DATE,
  trial_start_date         DATE,
  date_primary_completion  DATE,
  therpay_target           VARCHAR(200),
  recruitment_status       VARCHAR(200), 
  expected_enrollment      INTEGER,
  tx6_category             VARCHAR(200),
  tx7_category             VARCHAR(200),
  age                      VARCHAR(200),
  covid19_status           VARCHAR(200),
  patient_setting          VARCHAR(200),
  stidy_design_final       VARCHAR(200),
  blinding_final           VARCHAR(200),
  number_of_arms_final     INTEGER,
  state_name               VARCHAR(200),
  state_lon                DOUBLE,
  state_lat                DOUBLE,
  country_name             VARCHAR(200),
  iso3_code                VARCHAR(200),
  pdf_link                 VARCHAR(200),
  number_of_arms_finalcorrected_treatment_name VARCHAR(200),
  corrected_treatment_name_display VARCHAR(200),
  tx1_catgeory             VARCHAR(200),
  txt_category             VARCHAR(200),
  tx3_category             VARCHAR(200),
  tx4_category             VARCHAR(200),
  count                    DOUBLE,
  phase                    VARCHAR(200),
  outcome                  VARCHAR(200),
  id                       INTEGER,
  source                   VARCHAR(200),
  date_created             DATE
); 

CREATE TABLE clinical_trials_tracking (
  flagging_id              INTEGER,
  trial_id                 INTEGER,
  flag                     BOOLEAN,
  Rating                   INTEGER,
  user                     VARCHAR(200),
  date_created             DATE     
); 

