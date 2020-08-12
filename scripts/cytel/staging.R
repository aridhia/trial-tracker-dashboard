# Script to stage data from the Cytel trial tracker
#
# 0. Clear the session of any previous variables
# 1. Check RData file exists in expected location
# 2. Check RData file is not one already seen (via MD5 hash)
# 3. Copy the RData file and place in archive folder, calculate MD5
#    TODO - Zip the file up?
# 4. Parse RData file and run some validation checks
# 5. Read data RData file into memory, check expected tables exist
# 6. Check database is in place
# 7. Stage 2x tables from RData file to database
#
# Note - best to run this script with "R --no-save --no-restore-data --quiet"
library(tools)
library(loggit)
library(jsonlite)
library(DBI)
# 0. Clear the session and any previous variables
# This is important as we rely on loading an RData file,
# and expecting new variables in memory
rm(list = ls(all.names = TRUE))
gc() 

# Configuration
latest_rdata_file   <- '../COVID-19-Clinical-trial-tracker/dat_processed_and_network.RData'
archive_folder      <- './archive'
log_file            <- paste0('./archive/cytel-log-', format(Sys.time(), '%Y%m%d-%H%M%S'), '.log')
tracker_db_host     <- 'localhost'
tracker_db_name     <- 'tracker'
tracker_db_tbl      <- 'staging_trials'


set_logfile(log_file)

# 1. Check the RData file exists
if (!file.exists(latest_rdata_file)) {
    message(paste('RData file is missing:', latest_rdata_file), echo=FALSE)
} else {
    message(paste('Processing RData file', latest_rdata_file), echo=FALSE)

    # Ensure the archive folder is in place
    if (!file.exists(archive_folder)) {
        message(paste('Create archive folder:', archive_folder), echo=FALSE)
        dir.create(archive_folder)
    }

    # 2. Check the file is not one we've processed before
    # If we've processed the file already, there should be a file in the archive folder

    # Use the md5sum as a check on the *contents* of the file
    md5 <- md5sum(latest_rdata_file)
    message(paste('RData MD5 sum:', md5), echo=FALSE)

    archive_file <- paste0(archive_folder, '/cytel-', md5, '.RData')

    if (file.exists(archive_file)) {
        message(paste('Please check archive files, md5 check file exists:', archive_file), echo=FALSE)
    } else {
        file.copy(latest_rdata_file, archive_file)
        message(paste('RData file copied:', archive_file), echo=FALSE)

        # OK, now we can load the archive file, validate and parse the data
        load(archive_file)
        
        # Validation
        # 'dat' and 'dat_network_pairs' data frames are present
        df_dat_name <- 'dat'
        if (!(exists(df_dat_name)) && is.data.frame(get(df_dat_name))) {
            message(paste('Data frame missing:', df_dat_name), echo=FALSE)
        }
        else {
            message('Processing "dat" data frame')

            # Read in the expected metadata
            expected_dictionary <- fromJSON('./scripts/cytel/dat.json')
            expected_fields = expected_dictionary$fields

            df_dat <- get(df_dat_name)

            # Does the metadata match?
            name_mismatch <- length(setdiff(colnames(df_dat), expected_fields$name))
            type_mismatch <- length(setdiff(sapply(df_dat, typeof), expected_fields$type))
            if (name_mismatch > 0) {
                message(paste0('Validation of names failed: ', name_mismatch, ' mismatch(es)'), echo=FALSE)
            } else if (type_mismatch > 0) {
                message(paste0('Validation of types failed: ', type_mismatch, ' mismatch(es)'), echo=FALSE)
            } else {
                # Check database connection and table exist
                message('Testing database connection')
 
                con <- dbConnect(RPostgres::Postgres(), dbname=tracker_db_name, host=tracker_db_host)
                if (!is.element('staging_trials', dbListTables(con))) {
                    message(paste('Missing table:', tracker_db_tbl), echo=FALSE)
                } else {
                    message(paste0('Writing "dat" to "', tracker_db_tbl, '" ...'), echo=FALSE)
                    dbWriteTable(con, tracker_db_tbl, df_dat, overwrite=TRUE)
                    message(paste0('Writing "dat" to "', tracker_db_tbl, '" ... Done'), echo=FALSE)
                }
            }
        }
    }
}
message('Done', echo=FALSE)
