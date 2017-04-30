#Read price data - Zacks
#Note: sic4 code is unavailable for delisted companies in zacks but not in core US
start_date <- as.Date("2000-01-01","%Y-%m-%d")
price <- fread("~/zacks/price_4_23_2017.csv",select=c("m_ticker","ticker","date","close","volume"))
price[,date:=as.Date(date,"%Y-%m-%d")]
price <- price[date>=start_date]
sic <-  fread("~/zacks/master_4_23_2017.csv",select=c("m_ticker","sic_4_code"))
price <- merge(price,sic,by="m_ticker")
zmaster <- fread("~/zacks/master_4_23_2017.csv",select=c("m_ticker","comp_name","exchange","active_ticker_flag","ticker_type","sic_4_desc","zacks_x_sector_desc","zacks_m_ind_desc","country_name","asset_type"))
price <- merge(price,zmaster,by="m_ticker",all.x=TRUE)
div <- fread("~/zacks/div_4_23_2017.csv",select=c("m_ticker","currency_code","div_ex_date","div_amt"))
div[,div_ex_date:=as.Date(div_ex_date,"%Y-%m-%d")]
price <- merge(price,div,by.x=c("m_ticker","date"),by.y=c("m_ticker","div_ex_date"),all.x=TRUE)
price[is.na(div_amt),div_amt:=0]
setkeyv(price,c("m_ticker","date"))
price[,cum_dividends:=cumsum(div_amt),by=m_ticker]

#Adjust for mergers, acquisitions, bankruptcies, delistings, and ticker changes
maintenence <- fread("~/zacks/maintenence_4_23_2017.csv",select=c("action_type","m_ticker","ticker","change_txt","proc_date"))
ticker_change <- maintenence[action_type==5 & proc_date >= start_date]
death <- maintenence[action_type==2 & proc_date >= start_date]
death[,death_ind:=!grepl("[$]|[[:digit:]]",change_txt)]
death[,liquidation:=(gsub("^.*?[$]([[:digit:].]+).*?$","\\1",change_txt)]
acquired <- death[grepl("[[:digit:]]",change_txt)&!grepl("[$]",change_txt)]
acquired[,acquisition_ticker:=gsub("^.*?(?=[^$])([[:digit:].]+) ?([[:alpha:]]+).*?$","\\1 \\2",change_txt,perl=TRUE)]
acquired[,atick:=gsub("^.*? ([[:alpha:]]+)","\\1",acquisition_ticker)]
acquired[,shares:=gsub("([[:digit:].]+) .*?$","\\1",acquisition_ticker)]
liquidated <- death[!is.na(liquidation)]
bankrupt <- death[!!death_ind]