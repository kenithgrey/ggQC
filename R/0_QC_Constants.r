# Formula for constants ---------------------------------------------------
# d2 & d3
#   Barbosa, Emanuel Pimentel, Mario Antonio Gneri and Ariane Meneguetti.
#   “Range Control Charts Revisited: Simpler Tippett-like Formulae, Its Practical Implementation,
#   and the Study of False Alarm.” Communications in Statistics - Simulation and Computation
#   42 (2013): 247-262.
# c4 & c5
#   Wheeler et., al

d2 <- function(n) {
  Ew <- function(x){1 - stats::ptukey(q = x, nmeans = n, df = Inf)}
  stats::integrate(Ew, 0, Inf)$value
}
#d2(2)

d3 <- function(n) {
  Ew <- function(x){1 - stats::ptukey(q = x, nmeans = n, df = Inf)}
  Ew2 <-function(x){x * Ew(x)}
  sqrt(2*stats::integrate(Ew2, 0, Inf)$value-d2(n)^2)
}
#d3(2)

d4 <- function(n) {stats::qtukey(p = .5, nmeans = n, df = Inf)}
#d4(100)

c4 <- function(n) {sqrt(2/(n-1)) * gamma(n/2)/gamma((n-1)/2)}
#c4(2)

c5 <- function(n) {sqrt(1-c4(n)^2)}
#c5(2)


# #b2b4_ratio --------------------------------------------------------------
#  filelist <- list.files(path = "../Stat/1e4/", pattern = "csv", full.names = T)
# #
# loadfiles <- function(FILE){
#   temp <- read.csv(file = FILE, header=T)
#   temp$file <- FILE
#   return(temp)
# }
# b2b4 <- do.call(rbind, lapply(filelist, loadfiles))
# ratio <- aggregate(sd_median_per_sd_mean_ratio~n, FUN = mean, data=b2b4)
# colnames(ratio) <- c("n", "ratio")
# write.csv(ratio, file = "b2b4constants.csv", quote = F, row.names = F)
# b2b4 <- read.csv("b2b4constants.csv")
# devtools::use_data(b2b4, internal = TRUE, overwrite = TRUE)

b2b4_ratio <- function(n){
  if(n <= 200){
    b2b4_out <- b2b4$ratio[b2b4$n == n]
  }else{
    #mean(sd_median_per_sd_mean_ratio[n>200])
    b2b4_out <- 1.25286
  }
  return(b2b4_out)
}


QC_constants <- function(n) {
   round(data.frame(n = n,
  ####R-bar Mean(mean(X))
  d2 = d2(n), #BaseConstants["mean","RANGE_x"], #Root X-bar
  E2 = 3/d2(n), #BaseConstants["mean","RANGE_x"], #3 Sigma non stdentized
  A2 = 3/(d2(n)*sqrt(n)), # (BaseConstants["mean","RANGE_x"]*sqrt(n)), #3 sigma Sutdentized
  d3 = d3(n), #BaseConstants["sd","RANGE_x"], #Root for R-bar
  D3 = ifelse(1 - (3*d3(n)/d2(n)) < 0 , # Minus 3 Sigma for Range X
              0 ,
              1 - (3*d3(n)/d2(n))),
  D4 = 1 + (3*d3(n)/d2(n)), # Plus 3 Sigma for Range X

  #s-bar mean(mean(x))
  c4 = c4(n), # BaseConstants["mean","SD_x"],
  A3 = 3/c4(n)*sqrt(n), #(BaseConstants["mean","SD_x"]*sqrt(n)),
  B3 = ifelse(1 - 3/c4(n)*sqrt(1-c4(n)^2) < 0, # Minus 3 Sigma for Range X
              0,
              1 - 3/c4(n)*sqrt(1-c4(n)^2)),
  B4 = 1 + 3/c4(n)*sqrt(1-c4(n)^2),

  ###Median R Mean(mean(X))
  d4 = d4(n),
  A4 = 3/(d4(n)*sqrt(n)),
  D5 = ifelse((d2(n) - 3*d3(n))/d4(n) < 0, # Minus 3 Sigma for Range X
              0 ,
              (d2(n) - 3*d3(n))/d4(n)),

  D6 =(d2(n) + 3*d3(n))/d4(n), # Plus 3 Sigma for Range X

  ###R-Bar Mean(Median(X))
  A6 = 3/(d2(n)*sqrt(n))*b2b4_ratio(n),
  b2 = d2(n)/b2b4_ratio(n), # used for median(x) Rbar
  ####Median R Mean(Median(X))
  A9 = 3/(d4(n)*sqrt(n))*b2b4_ratio(n),
  b4 = d4(n)/b2b4_ratio(n) # used for median(x) RMedian
  ), digits = 6)
}


