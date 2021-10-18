# Code to create the credential files
dir.create("admin/") # creates folder if it doesn't exist

credentials_df <- data.frame(
  user = c("username"), # mandatory
  password = c("password"), # mandatory
  stringsAsFactors = FALSE)

saveRDS(credentials_df, "admin/credentials.rds")
