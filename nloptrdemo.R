

RebDates <- getRebDates(as.Date('2010-01-31'),as.Date('2017-05-31'),'month')
TS <- getTS(RebDates,'EI000985')

gf.NP_stat <- function(TS,Nbin=lubridate::years(-3),type=c('simple','lm')){
  type <- match.arg(type)
  
  #get report date 
  begT <- trday.offset(min(TS$date),Nbin)
  begT <- trday.offset(begT,lubridate::years(-1))
  endT <- max(TS$date)
  rptDate <- getrptDate(begT,endT,type = 'forward')
  
  rptTS <- expand.grid(rptDate = rptDate, stockID = unique(TS$stockID),
                       KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  rptTS <- dplyr::arrange(rptTS,rptDate,stockID)
  
  #remove report dates before IPO 
  tmp <- data.frame(date=max(TS$date),stockID = unique(TS$stockID),stringsAsFactors=FALSE)
  tmp <- TS.getTech_ts(tmp, funchar="Firstday()",varname='IPODay')
  tmp <- transform(tmp,date=NULL,IPODay=tsdate2r(IPODay))
  rptTS <- dplyr::left_join(rptTS,tmp,by='stockID')
  rptTS <- rptTS[rptTS$rptDate>rptTS$IPODay,c('rptDate','stockID')]
 
  funchar <- '"np_belongto_parcomsh",LastQuarterData(RDate,46078,0)'
  TSFdata <- rptTS.getFin_ts(rptTS,funchar)
  
  rtndata <- TSFdata %>% dplyr::group_by(stockID) %>%
    dplyr::mutate(growth =(np_belongto_parcomsh - dplyr::lag(np_belongto_parcomsh, 4))/abs(dplyr::lag(np_belongto_parcomsh, 4)))
  
  
  TSnew <- getrptDate_newest(TS)
  TSnew <- na.omit(TSnew)
  TSnew <- dplyr::rename(TSnew,rptDateEnd=rptDate)
  TSnew$rptDateBeg <- TSnew$rptDateEnd %m+% Nbin
  
  tmp <- dplyr::distinct(TSnew,rptDateBeg,rptDateEnd)
  tmp <- tmp %>% dplyr::rowwise() %>% 
    dplyr::do(rptDateBeg=.$rptDateBeg,rptDateEnd=.$rptDateEnd,rptDate = getrptDate(.$rptDateBeg, .$rptDateEnd,type = 'between')) %>% 
    dplyr::do(data.frame(rptDateBeg=.$rptDateBeg,rptDateEnd=.$rptDateEnd,rptDate = .$rptDate))
  
  TSnew <- dplyr::full_join(TSnew,tmp,by=c('rptDateBeg','rptDateEnd'))
  TSnew <- transform(TSnew,rptDateBeg=NULL,rptDateEnd=NULL)
  
  TSFdata <- dplyr::left_join(TSnew,rtndata[,c("rptDate","stockID","growth")],by=c('stockID','rptDate'))
  TSFdata <- na.omit(TSFdata)
  TSFdata <- TSFdata[!is.infinite(TSFdata$growth),]
  TSFdata <- TSFdata %>% dplyr::group_by(date,stockID) %>% dplyr::mutate(id =row_number())
  
  N <- max(TSFdata$id)
  TSF <- TSFdata %>% group_by(date,stockID) %>% filter(max(id) > N/2) %>%  summarise(factorscore=mean(growth)/sd(growth))
  TSF <- left_join(TS,TSF,by=c('date','stockID'))
  
  TSF <- TSFdata %>% group_by(date,stockID) %>% filter(max(id) > N/2) %>%  summarise(factorscore=mean(growth))
  TSF <- left_join(TS,TSF,by=c('date','stockID'))
  TSF <- RFactorModel:::factor.std(TSF,factorStd = 'sectorNe',sectorAttr = list(std=list(factorList1,33),level=list(5,1)))
  TSFR <- getTSR(TSF)
  chart.IC(TSFR)
  chart.Ngroup.spread(TSFR)
  table.Ngroup.overall(TSFR)
}


getrptDate <- function(begT,endT,type=c('between','forward','backward')){
  type <- match.arg(type)
  
  tmp <- seq(begT,endT,by='day')
  if(type=='forward'){
    tmp <- lubridate::floor_date(tmp, "quarter")-lubridate::days(1)
  }else if(type=='backward'){
    tmp <- lubridate::ceiling_date(tmp, "quarter")-lubridate::days(1)
  }else{
    tmp <- c(lubridate::floor_date(tmp, "quarter")-lubridate::days(1),
             lubridate::ceiling_date(tmp, "quarter")-lubridate::days(1))
    
  }

  rptDate <- sort(unique(tmp))
  if(type=='between'){
    rptDate <- rptDate[rptDate>=begT]
    rptDate <- rptDate[rptDate<=endT]
  }
  return(rptDate)
}



gf.EPS_stat <- function(TS,Nbin='3 years'){
  #get report date 
  rptDate <- trday.offset(min(TS$date),Nbin)
  rptDate <- getRebDates(rptDate,max(TS$date))
  rptDate <- unique(lubridate::floor_date(rptDate, "quarter")-lubridate::days(1))
  rptTS <- expand.grid(rptDate = rptDate, stockID = unique(TS$stockID),
                       KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  rptTS <- dplyr::arrange(rptTS,rptDate,stockID)
  
  #remove report dates before IPO 
  tmp <- data.frame(date=max(TS$date),stockID = unique(TS$stockID),stringsAsFactors=FALSE)
  tmp <- TS.getTech_ts(tmp, funchar="Firstday()",varname='IPODay')
  tmp <- transform(tmp,date=NULL,IPODay=tsdate2r(IPODay))
  rptTS <- dplyr::left_join(rptTS,tmp,by='stockID')
  rptTS <- rptTS[rptTS$rptDate>rptTS$IPODay,c('rptDate','stockID')]
  
  funchar <- '"eps",LastQuarterData(RDate,9900000,0)'
  TSFdata2 <- rptTS.getFin_ts(rptTS,funchar)
  
  
}

