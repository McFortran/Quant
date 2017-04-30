#Merge price and fundamentals
trading_days <- price[,.N,keyby=date]$date
fun[,merge_date:=as.Date(datekey,"%Y-%m-%d")]#set up fundamental merge date
fun[dimension=="ARQ",merge_date:=(merge_date + 30)[findInterval(merge_date+30,trading_days)]]
fun[dimension=="ART",merge_date:=(merge_date + 30)[findInterval(merge_date+30,trading_days)]]
fun[dimension=="ARY",merge_date:=(merge_date + 30)[findInterval(merge_date+30,trading_days)]]
agg <- merge(fun,price,by.x=c("ticker","merge_date"),by.y=c("ticker","date"))
agg[,divsplit:=div_amt/close+1]
agg[divsplit==0,divsplit:=1]
setkeyv(agg,c("ticker","merge_date"))
agg[,divsplit:=cumprod(divsplit),by=ticker]

for(i in 1:5) {
  lookahead(deriveName("close",i),"close",365*i,agg)
  agg[,(deriveName("chg",i)):=agg[[deriveName("close",i)]]/close-1]
  lookahead(deriveName("pe",i),"pe",365*i,fun)
}
agg <- agg[merge_date<as.Date("01-01-2012","%m-%d-%Y")]
agg <- agg[is.na(chg_5_year),chg_5_year:=-1]
#' @SG Also look for other biases, by survivorship, marketcap, sector, etc
agg <- agg[dimension=="ARQ"]


#set up predictors
pred <- names(agg)[8:match("workingcapital",names(agg))]
pred <- c(pred,paste0("pe_",1:5,"_year"),"sic_4_desc","zacks_x_sector_desc","zacks_m_ind_desc")