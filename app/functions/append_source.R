#########################################

## Bind data frames and append source ##

########################################


#append_source() takes a vector of names of df you want to rbind 
#and appends a source column with the name of the dfs the data come from

append_source <- function(df_names) {
  do.call(rbind, lapply(df_names, function(x) {
    cbind(get(x), source = x)
  }))
}