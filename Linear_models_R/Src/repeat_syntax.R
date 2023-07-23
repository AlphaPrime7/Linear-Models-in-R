out_df <- data.frame(A=c("")) #given this is hard code but a good look at R syntax
print(out_df)
for(i in cd_df$cooksd){
  if(i > 4*mean(cd_df$cooksd)){
    x <- 0
    repeat{
      x <- x + i #repeat the outlier value with starting value of 0
      out_df$A[1] <- x
      if(x <= nrow(out_df)){
        break
      }
    }
  }
}
print(out_df)