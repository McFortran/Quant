#Read Fundamental Data - core US premium
#' @note appends delisted companies with a numeric index if the ticker is recycled
fun <- fread("~/Core_US_Fundamentals/fun_4_23_2017.csv")
fun[,Date:=as.Date(datekey,"%Y-%m-%d")]
fun[,calendardate:=as.Date(calendardate,"%Y-%m-%d")]
funtick <- fread("~/Core_US_Fundamentals/tickers.txt",select=c("Ticker","Name","SIC"))
fun <- merge(fun,funtick,by.x="ticker",by.y="Ticker")
fun[,ticker:=gsub("[.-]","",ticker)] #Core US removes . and - from their ticker
fun[,tick:=paste0(ticker,SIC)]
setkeyv(fun,c("ticker","calendardate","datekey"))
restatement_fields <- c(assets="assets",balance_sheet="bvps",assets_short="currentratio",leverage="de",debt="debt",operating_earnings="ebit",equity="equity",cash_flow="fcf",profit="gp",
                        investments="investments",liabilities="liabilities",operating_cash_flow="ncfo",income="netinc",payables="payables",receivables="receivables",revenue="revenue",
                        tangibles="tangibles",intangibles="intangibles",inventory="inventory",working_capital="workingcapital")
rsdc <- c("dimension",restatement_fields)
begin <- proc.time()
q_restatement <- fun[dimension%in%c("ARQ","MRQ"), lapply(.SD,calc_restatement_generator_quarterly(dimension)) ,keyby=.(ticker,calendardate),.SDcols=rsdc]
y_restatement <- fun[dimension%in%c("ARY","MRY"), lapply(.SD,calc_restatement_generator_yearly(dimension)) ,keyby=.(ticker,calendardate),.SDcols=rsdc]
end <- proc.time()
print(end-begin)
q_restatement[,dimension:=NULL]
names(q_restatement) <- c("ticker","calendardate",paste0("re_diff_",restatement_fields))
y_restatement[,dimension:=NULL]
names(y_restatement) <- c("ticker","calendardate",paste0("re_diff_",restatement_fields))
funq <- fun[dimension=="ARQ"]
funq <- merge(funq,q_restatement,by=c("ticker","calendardate"))
rasset <- normalize_fields(paste0("re_diff_",c("assets","investments","tangibles","intangibles")),"assets",funq)
rquick <- normalize_fields(paste0("re_diff_",c("receivables","payables","inventory","workingcapital")),"workingcapital",funq)
rliab <- normalize_fields(paste0("re_diff_",c("debt","liabilities")),"liabilities",funq)
requity <- normalize_fields(paste0("re_diff_",c("equity")),"equity",funq)
rcash <- normalize_fields(paste0("re_diff_",c("fcf","ncfo")),"ncfo",funq)
rincome <- normalize_fields(paste0("re_diff_",c("ebit","gp","netinc","revenue")),"revenue",funq)
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
chg_fields <- c("assets","bvps","capex","currentratio","de","dps","ebit","ebitda","ebitdamargin","eps","equity","fcf","gp","grossmargin","inventory","investments","liabilities","ncf","ncfo","netinc","opex","opinc","payables",
                "receivables","revenue","roa","roe","roic","ros","tbvps","workingcapital")
qchg_fields <- c()
for(i in 1:length(chg_fields)) {
  for(j in 1:nqtrs) {
    nm <- paste0(chg_fields[i],"_",j,"_qtr")
    funq <- lookbehindqtr(nm,chg_fields[i],j,funq)
    qchg_fields <- c(qchg_fields,nm)
  }
}

funy <- fun[dimension=="ARY"]
funy <- merge(funy,y_restatement,by=c("ticker","calendardate"))
rasset <- normalize_fields(paste0("re_diff_",c("assets","investments","tangibles","intangibles")),"assets",funy)
rquick <- normalize_fields(paste0("re_diff_",c("receivables","payables","inventory","workingcapital")),"workingcapital",funy)
rliab <- normalize_fields(paste0("re_diff_",c("debt","liabilities")),"liabilities",funy)
requity <- normalize_fields(paste0("re_diff_",c("equity")),"equity",funy)
rcash <- normalize_fields(paste0("re_diff_",c("fcf","ncfo")),"ncfo",funy)
rincome <- normalize_fields(paste0("re_diff_",c("ebit","gp","netinc","revenue")),"revenue",funy)
rfields <- c(rasset,rquick,rliab,requity,rcash,rincome)
nyrs <- 3
ydiff_fields <- c()
for(i in 1:length(rfields)) {
  for(j in 1:nyrs) {
    nm <- paste0(rfields[i],"_",j,"_yr")
    funy <- lookbehindqtr(nm,rfields[i],j*4,funy)
    ydiff_fields <- c(ydiff_fields,nm)
  }
}

ychg_fields <- c()
for(i in 1:length(chg_fields)) {
  for(j in 1:nyrs) {
    nm <- paste0(chg_fields[i],"_",j,"_yr")
    funy <- lookbehindqtr(nm,chg_fields[i],j,funy)
    ychg_fields <- c(ychg_fields,nm)
  }
}
write.csv(funq,"~/Core_US_Fundamentals/fun_quarter_4_23_2017.csv") #includes restatements and changes, but not industry comparisons or ratios
