library(DBI)

# Configuration
clinical_trial_data_file_path   <- paste("clinical_trial_live_backup_",Sys.Date(),".RData", sep="")
archive_folder                  <- './archive'
tracker_db_host                 <- 'localhost'
tracker_db_name                 <- 'covid_trial_tracker'

# Archive existing tables.
#dat <- xap.read_table("trials")
#tracking_dat <- xap.read_table("reviews")

#save(dat, tracking_dat, file=clinical_trial_data_file_path)

# Move Staging Data to Load
con <- dbConnect(RPostgres::Postgres(), dbname=tracker_db_name, host=tracker_db_host)
dbSendQuery(con, 'INSERT INTO trk_trials 
(
  source_registry,          
  trial_id,                 
  public_title,             
  scientific_title,         
  institution,              
  date_registered,          
  date_updated,             
  trial_start_date,         
  date_primary_completion,  
  therapy_target,           
  recruitment_status,       
  expected_enrollment,      
  tx6_category,             
  tx7_category,             
  age,                      
  covid19_status,           
  patient_setting,          
  study_design_final,       
  blinding_final,           
  state_name,               
  state_lon,                
  state_lat,                
  country_name,             
  iso3_code,                
  pdf_link,                 
  number_of_arms_final,
  corrected_treatment_name, 
  corrected_treatment_name_display,
  tx1_category, 
  tx2_category,             
  tx3_category,             
  tx4_category,             
  count,                   
  phase,                    
  outcome,                  
  source,                   
  date_created        
)
SELECT
  source_registry,          
  trial_id,                 
  public_title,             
  scientific_title,         
  institution,              
  date_registered,          
  date_updated,             
  trial_start_date,         
  date_primary_completion,  
  therapy_target,           
  recruitment_status,       
  expected_enrollment,      
  tx6_category,             
  tx7_category,             
  age,                      
  covid19_status,           
  patient_setting,          
  study_design_final,       
  blinding_final,           
  state_name,               
  state_lon,                
  state_lat,                
  country_name,             
  iso3_code,                
  pdf_link,                 
  number_of_arms_final,
  corrected_treatment_name, 
  corrected_treatment_name_display,
  tx1_category, 
  tx2_category,             
  tx3_category,             
  tx4_category,             
  count,                   
  phase,                    
  outcome,                  
  \'Cytel\',                   
  CURRENT_DATE
FROM trk_staging_trials;')  
