library(DBI)

# Configuration
clinical_trial_data_file_path   <- paste("clinical_trial_live_backup_",Sys.Date(),".RData", sep="")
archive_folder                  <- './archive'
tracker_db_tbl                  <- 'trk_staging_trials'
con                             <- ''

# Archive existing tables.
#dat <- xap.read_table("trials")
#tracking_dat <- xap.read_table("reviews")

#save(dat, tracking_dat, file=clinical_trial_data_file_path)

# Set Variables for Enviorment
if(exists("xap.conn")){
    con <- xap.conn
} else {
   con <- dbConnect(RPostgres::Postgres(), dbname=Sys.getenv("PGDATABASE"), host=Sys.getenv("PGHOST"), user=Sys.getenv("PGUSER"), password=Sys.getenv("PGPASSWORD"))
}

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
  url,
  iso3_code,                
  pdf_link,                 
  number_of_arms_final,
  corrected_treatment_name, 
  corrected_treatment_name_display,
  tx1_category, 
  tx2_category,             
  tx3_category,             
  tx4_category, 
  tx5_category,                 
  count,                   
  phase,                    
  outcome,                  
  source,                   
  created_at        
)
SELECT
  source_registry,          
  trial_id,                 
  public_title,             
  scientific_title,         
  institution,              
  date_registered::date,          
  date_updated::date,             
  trial_start_date::date,         
  date_primary_completion::date,  
  therapy_target,           
  recruitment_status,       
  expected_enrollment::integer,      
  tx6_category,             
  tx7_category,             
  age,                      
  covid19_status,           
  patient_setting,          
  study_design_final,       
  blinding_final,           
  state_name,               
  state_lon::decimal,                
  state_lat::decimal,                
  country_name,
  url,
  iso3_code,                
  pdf_link,                 
  number_of_arms_final::integer,
  corrected_treatment_name, 
  corrected_treatment_name_display,
  tx1_category, 
  tx2_category,             
  tx3_category,             
  tx4_category,
  tx5_category,             
  count::decimal,                   
  phase,                    
  outcome,                  
  \'Cytel\',                   
  CURRENT_DATE
FROM trk_staging_trials;')  

