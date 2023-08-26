


#' 
#' `winsor()` - The top and bottom trim values are given values of the trimmed and 1- trimmed quantiles.
#' 
#' adapte code from [psych](https://github.com/cran/psych/blob/master/R/winsor.R)
#' https://search.r-project.org/CRAN/refmans/psych/html/winsor.html
#' 


"winsorPro" <-
  function(x, trim=.2, trim.to.na = FALSE, na.rm=TRUE) {  
    if(is.vector(x) ) {
      ans <-  winsPro(x, trim=trim, trim.to.na=trim.to.na, na.rm=na.rm) } else {
        if (is.matrix(x) | is.data.frame(x)) {ans <- apply(x,2,winsPro,trim=trim,na.rm=na.rm) } }
    return(ans)
  }


#added winsor
"winsPro" <- 
  function(x, trim=.2, trim.to.na = FALSE, na.rm=TRUE) {
    if ((trim < 0) | (trim>0.5) ) 
      stop("trimming must be reasonable")
    qtrim <- quantile(x,c(trim,.5, 1-trim),na.rm = na.rm)
    xbot <- qtrim[1]
    xtop <- qtrim[3]
    if(trim.to.na == T){
      if(trim<.5) { 
        x[x < xbot] <- NA
        x[x > xtop] <- NA
      } else {
        x[!is.na(x)] <- qtrim[2]}
    } else{
      if(trim<.5) { 
        x[x < xbot] <- xbot
        x[x > xtop] <- xtop
      } else{
        x[!is.na(x)] <- qtrim[2]}
    }
    
    return(x) } 


### test
# x <- c(12.111111, 0.27742921, 0.61111111, -2.3456977,  0.4291247,
#        0.5060559, -0.5747400, -0.5466319, -0.5644520, -0.8900378)
# trim=0.1
# x
# winsor(x, trim = trim, na.rm = TRUE)       ## the function in the original package
# winsorPro(x, trim = trim, na.rm = TRUE)  ## same function in the revised one
# winsorPro(x, trim = trim, trim.to.na = TRUE, na.rm = TRUE)
