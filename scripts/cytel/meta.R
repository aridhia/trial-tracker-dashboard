# Utility to extract metadata summary from a Cytel RData file
library(jsonlite)

latest_rdata_file   <- '../COVID-19-Clinical-trial-tracker/dat_processed_and_network.RData'

load(latest_rdata_file)
df_dat <- get('dat')

df_meta <- data.frame(colnames(df_dat), sapply(df_dat, typeof))
names(df_meta) <- c('name', 'type')

dictionary <- list(fields = df_meta)

print(toJSON(dictionary, pretty=TRUE))
