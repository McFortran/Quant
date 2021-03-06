library(Quandl)
library(h2o)
library(data.table)
library(ggplot2)
library(scales)
library(bit64)
library(magrittr)
options(max.print=10000000)
options(scipen=9999)

Quandl.api_key("518M6RHWuPCiDJfd-ijx")

#Utility
seq_end <- function(n) {
  if(!length(n)) return(c())
  vec <- c()
  for(i in 1:length(n)) {
    vec <- c(vec,1:n[i])
  }
  vec
}

fillVector <- function(vec) {
  if(all(is.na(vec))) return(vec)
  hasLeadingNA <- is.na(vec[1])
  if(hasLeadingNA) {
    firstReal <- which(!is.na(vec))[1]
    leadingNAs <- vec[1:(firstReal-1)]
    vec <- vec[firstReal:length(vec)]
  } else {
    leadingNAs <- c()
  }
  realidx <- which(!is.na(vec))
  real <- vec[realidx]
  naidx <- which(is.na(vec))
  vec[naidx] <- real[findInterval(naidx,realidx)]
  vec <- c(leadingNAs,vec)
  vec
}

is.bad.numeric <- function(...) {
  x <- list(...)
  b <- rep(FALSE,length(x[[1]]))
  for(i in 1:length(x)) {
    b <- b | is.na(x[[i]]) | is.nan(x[[i]]) | is.infinite(x[[i]])
  }
  b
}
#Derive fields
normalize_fields <- function(fields,factor,dt) {
  #returns names of new fields
  new_fields <- paste0(fields,"_",factor,"_ratio")
  for(i in 1:length(new_fields)) {
    val <- as.numeric(dt[[fields[i]]])/as.numeric(dt[[factor]])
    dt[,(new_fields[i]):=val]
  }
  new_fields
}
decileize <- function(field,dt) {
  #Note, need a way to do a RELATIVE ranking (by time, industry, etc)
  newfield <- c()
  for(i in 1:length(field)) {
    deciles <- sort(dt[[field[i]]])
    deciles <- deciles[!is.bad.numeric(deciles)]
    deciles <- deciles[length(deciles)*(1:10)/10]
    deciles[10] <- Inf
    newfield[i] <- paste0(field[i],"_decile")
    dt[,(newfield[i]):=findInterval(dt[[field[i]]],deciles)+1]
  }
  newfield
}
filterbad <- function(fields,dt) {
  dt[!do.call(is.bad.numeric,as.list(dt[,.SD,.SDcols=fields]))]
}
lookahead <- function(nm,field,days,dt) {
  setkeyv(dt,c("merge_date",field))
  command <- paste0(field,"[findInterval(merge_date+",days,",merge_date)+1]")
  dt[,(nm):=eval(parse(text=command)),by=ticker]
}
lookbehind <- function(nm,field,days,dt) {
  lookahead(nm,field,-days,dt)
}
priorqtr <- function(date,periods) {
  m <- substr(date,6,7)
  y <- substr(date,1,4)
  months <- c("03","06","09","12")
  days <- c(31,30,30,31)
  mn <- match(m,months)
  monthno <- (mn-periods-1)%%4+1
  yearoffset <- (mn-periods-1)%/%4
  paste0(as.integer(y)+yearoffset,"-",months[monthno],"-",days[monthno])
}
lookbehindqtr <- function(nm,field,periods,dt) {
  coerce_int64 <- function(cmd) {
    if(is.integer64(dt[[field]])) {
      paste0("as.numeric(",cmd,")")
    } else {
      cmd
    }
  }
  command <- paste0(coerce_int64(field),"[match(as.Date(priorqtr(calendardate,periods)),calendardate)]")
  print(nm)
  print(command)
  dt[,(nm):=eval(parse(text=command)),by=ticker]
  dt
}
deriveName <- function(field,years) {
  paste0(field,"_",years,"_year")
}
restatement <- function(restated,original) {
  if(is.character(restated) || is.character(original)) return(0)
  r <- restated[length(restated)]
  o <- original[1]
  if(length(r)) {
    r-o
  } else {
    0
  }
}
calc_restatement_generator_quarterly <- function(dimension) {
  function(field) {
    restatement(field[dimension=="MRQ"],field[dimension=="ARQ"])
  }
}
calc_restatement_generator_yearly <- function(dimension) {
  function(field) {
    restatement(field[dimension=="MRY"],field[dimension=="ARY"])
  }
}
#inter-record calculations - quarterly
psum <- function(...) {
  args <- list(...)
  val <- rep(0,length(args[[1]]))
  for(i in 1:length(args)) {
    ival <- args[[i]]
    ival[is.na(ival)] <- 0
    val <- val+ival
  }
  val
}
pmult <- function(...) {
  args <- list(...)
  val <- rep(0,length(args[[1]]))
  for(i in 1:length(args)) {
    ival <- args[[i]]
    ival[is.na(ival)] <- 1
    val <- val*ival
  }
  val
}
pnzlength <- function(...) {
  args <- list(...)
  val <- rep(0,length(args[[1]]))
  for(i in 1:length(args)) {
    ival <- args[[i]]
    ival[is.na(ival)] <- 0
    ival[!is.na(ival)] <- 1
    val <- val+ival
  }
  val
}
pmean <- function(...) {
  psum(...)/pnzlength(...)
}
pvar <- function(...) {
  args <- list(...)
  for(i in 1:length(args)) {
    args[[i]] <- (args[[i]]-pmean(...)) ^2
  }
  n <- pnzlength(...)
  do.call(pmean,args)*n/(n-1)
}
psd <- function(...) {
  sqrt(pvar(...))
}
pcv <- function(...) {
  psd(...)/pmean(...)
}
#covariance of y and 1:length(y)
plcov <- function(...) {
  args <- list(...)
  xargs <- args
  xyargs <- args
  for(i in 1:length(args)) {
    xargs[[i]] <- i
    xyargs[[i]] <- xargs[[i]]*args[[i]]
  }
  ey <- pmean(...)
  ex <- do.call(pmean,xargs)
  exy <- do.call(pmean,xyargs)
  n <- pnzlength(...)
  (exy-ex*ey)*n/(n-1)
}
pbeta <- function(...) {
  args <- list(...)
  xargs <- args
  for(i in 1:length(args)) {
    xargs[[i]] <- i
  }
  plcov(...)/do.call(pvar,xargs)
}
pconseqtrue <- function(...) {
  args <- list(...)
  idx <- rep(0,length(args[[1]]))
  cumtrue <- rep(TRUE,length(idx))
  for(i in 1:length(args)) {
    cumtrue <- cumtrue & args[[i]]
    idx <- idx + cumtrue
  }
  idx
}
# 3 2 1 -> TRUE TRUE, 1 3 2 4 -> FALSE TRUE FALSE
pgreaterchain <- function(...) {
  args <- list(...)
  b <- c()
  for(i in 1:(length(args)-1)) {
    b[[i]] <- args[[i]] > args[[i+1]]
  }
  b
}
plesschain  <- function(...) {
  args <- list(...)
  b <- c()
  for(i in 1:(length(args)-1)) {
    b[[i]] <- args[[i]] < args[[i+1]]
  }
  b
}
pconseqincrease <- function(...) {
  do.call(pconseqtrue,pgreaterchain(...))
}
pconseqdecrease <- function(...) {
  do.call(pconseqtrue,plesschain(...))
}
rowvolatility <- function(char_fields,dt) {
  dt[,do.call(pcv,.SD),.SDcols=char_fields]
}
#Call these with 1:12, which is newest first
rowtrend <- function(char_fields,dt) {
  char_fields <- char_fields[length(char_fields):1]
  dt[,do.call(pbeta,.SD),.SDcols=char_fields]/dt[[char_fields[[1]]]]
}
rowconseqincrease <- function(char_fields,dt) {
  dt[,do.call(pconseqdecrease,.SD),.SDcols=char_fields]
}
rowconseqdecrease <- function(char_fields,dt) {
  dt[,do.call(pconseqincrease,.SD),.SDcols=char_fields]
}
rowconseqchange <- function(char_fields,dt) {
  increase <- rowconseqincrease(char_fields,dt)
  decrease <- rowconseqdecrease(char_fields,dt)
  decrease <- decrease * -1
  decrease[which(decrease==0)] <- increase[which(decrease==0)]
  decrease
}