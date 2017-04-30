#Read Fundamental Data - core US premium
#' @note appends delisted companies with a numeric index if the ticker is recycled
fun <- fread("~/Core_US_Fundamentals/fun_4_23_2017.csv",integer64="numeric")
fun[,Date:=as.Date(datekey,"%Y-%m-%d")]
fun[,calendardate:=as.Date(calendardate,"%Y-%m-%d")]
funtick <- fread("~/Core_US_Fundamentals/tickers.txt",select=c("Ticker","Name","SIC"))
fun <- merge(fun,funtick,by.x="ticker",by.y="Ticker")
fun[,ticker:=gsub("[.-]","",ticker)] #Core US removes . and - from their ticker
fun[,tick:=paste0(ticker,SIC)]
setkeyv(fun,c("ticker","calendardate","datekey"))

#Add ratios


#Prep financial restatements
restatement_fields <- c("assets","assetsc","assetsnc","bvps","cashneq","cor","currentratio","de","debt","depamor","ebit","equity","fcf","gp",
                        "investments","liabilities","ncfo","netinc","payables","receivables","revenue",
                        "tangibles","intangibles","inventory","workingcapital")
rsdc <- c("dimension",restatement_fields)
q_restatement <- fun[dimension%in%c("ARQ","MRQ"), lapply(.SD,calc_restatement_generator_quarterly(dimension)) ,keyby=.(ticker,calendardate),.SDcols=rsdc]
y_restatement <- fun[dimension%in%c("ARY","MRY"), lapply(.SD,calc_restatement_generator_yearly(dimension)) ,keyby=.(ticker,calendardate),.SDcols=rsdc]
q_restatement[,dimension:=NULL]
names(q_restatement) <- c("ticker","calendardate",paste0("re_diff_",restatement_fields))
y_restatement[,dimension:=NULL]
names(y_restatement) <- c("ticker","calendardate",paste0("re_diff_",restatement_fields))

#restatement - quarterly
funq <- fun[dimension=="ARQ"]
funq <- merge(funq,q_restatement,by=c("ticker","calendardate"))
rasset <- normalize_fields(paste0("re_diff_",c("assets","assetsnc","cashneq","investments","tangibles","intangibles")),"assets",funq)
rquick <- normalize_fields(paste0("re_diff_",c("assetsc","receivables","payables","inventory","workingcapital")),"workingcapital",funq)
rliab <- normalize_fields(paste0("re_diff_",c("debt","liabilities")),"liabilities",funq)
requity <- normalize_fields(paste0("re_diff_",c("equity")),"equity",funq)
rcash <- normalize_fields(paste0("re_diff_",c("fcf","ncfo")),"ncfo",funq)
rincome <- normalize_fields(paste0("re_diff_",c("cor","depamor","ebit","gp","netinc","revenue")),"revenue",funq)
rfields <- c(rasset,rquick,rliab,requity,rcash,rincome)
nqtrs <- 12
qdiff_fields <- c()
for(i in 1:length(rfields)) {
  for(j in 1:nqtrs) {
    nm <- paste0(rfields[i],"_",j,"_qtr")
    funq <- lookbehindqtr(nm,rfields[i],j,funq)
    qdiff_fields <- c(qdiff_fields,nm)
  }
}

# #restatement - yearly
# funy <- fun[dimension=="ARY"]
# funy <- merge(funy,y_restatement,by=c("ticker","calendardate"))
# rasset <- normalize_fields(paste0("re_diff_",c("assets","assetsnc","cashneq","investments","tangibles","intangibles")),"assets",funq)
# rquick <- normalize_fields(paste0("re_diff_",c("assetsc","receivables","payables","inventory","workingcapital")),"workingcapital",funq)
# rliab <- normalize_fields(paste0("re_diff_",c("debt","liabilities")),"liabilities",funq)
# requity <- normalize_fields(paste0("re_diff_",c("equity")),"equity",funq)
# rcash <- normalize_fields(paste0("re_diff_",c("fcf","ncfo")),"ncfo",funq)
# rincome <- normalize_fields(paste0("re_diff_",c("cor","depamor","ebit","gp","netinc","revenue")),"revenue",funq)
# rfields <- c(rasset,rquick,rliab,requity,rcash,rincome)
# nyrs <- 3
# ydiff_fields <- c()
# for(i in 1:length(rfields)) {
#   for(j in 1:nyrs) {
#     nm <- paste0(rfields[i],"_",j,"_yr")
#     funy <- lookbehindqtr(nm,rfields[i],j*4,funy)
#     ydiff_fields <- c(ydiff_fields,nm)
#   }
# }


#Add prior fields
chg_fields <- c("assets","assetsc","assetsnc","assetturnover","bvps","capex","cashneq","cor","currentratio","de","debt","deferredrev","depamor",
                "dps","ebit","ebitda","ebitdamargin","ebt","eps","equity","fcf","gp","grossmargin","intangibles","intexp","invcap","inventory",
                "investments","liabilities","liabilitiesc","liabilitiesnc","ncf","ncfdebt","ncff","ncfi","ncfo","netinc","opex","opinc","payables",
                "receivables","retearn","revenue","rnd","roa","roe","roic","ros","sgna","tangibles","tbvps","workingcapital")
qchg_fields <- c()
for(i in 1:length(chg_fields)) {
  for(j in 1:nqtrs) {
    nm <- paste0(chg_fields[i],"_",j,"_qtr")
    funq <- lookbehindqtr(nm,chg_fields[i],j,funq)
    qchg_fields <- c(qchg_fields,nm)
  }
}



# ychg_fields <- c()
# for(i in 1:length(chg_fields)) {
#   for(j in 1:nyrs) {
#     nm <- paste0(chg_fields[i],"_",j,"_yr")
#     funy <- lookbehindqtr(nm,chg_fields[i],j,funy)
#     ychg_fields <- c(ychg_fields,nm)
#   }
# }

#Add prior field statistics
for(i in 1:length(chg_fields)) {
  field <- chg_fields[i]
  fields <- c(field,paste0(field,"_",1:nqtrs,"_qtr"))
  yfields <- fields[(0:(nqtrs%/%4))*4+1]
  yfields <- yfields[!is.na(yfields)]
  funq[[paste0(field,"_volatility_",nqtrs)]] <- rowvolatility(fields,funq)
  funq[[paste0(field,"_trend_",nqtrs)]] <- rowtrend(fields,funq)
  funq[[paste0(field,"_streak_qtr_",nqtrs)]] <- rowconseqchange(fields,funq)
  funq[[paste0(field,"_streak_yr_",nqtrs)]] <- rowconseqchange(yfields,funq)
}


#Add competitor (industry) fields


#Add future (target) fields


#Write file
write.csv(funq,"~/Core_US_Fundamentals/fun_quarter_4_23_2017.csv") #includes restatements and changes, but not industry comparisons or ratios
# write.csv(funy,"~/Core_US_Fundamentals/fun_year_4_23_2017.csv") #includes restatements and changes, but not industry comparisons or ratios
