#unique identifier function
unique_identifier <- function(df){
  for(i in 1:nrow(df)){
    x <- 0
    x <- x + i
    df$unique_id[i] <- x
  }
  return(df)
}
#test the function
cd_df <- unique_identifier(cd_df)
print(cd_df)