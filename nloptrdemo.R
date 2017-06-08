

RebDates <- getRebDates(as.Date('2010-01-31'),as.Date('2017-05-31'),'month')
TS <- getTS(RebDates,'EI000985')

gf.NP_stat <- function(TS,Nbin=lubridate::years(-3)){
  
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
    dplyr::mutate(growth = np_belongto_parcomsh / dplyr::lag(np_belongto_parcomsh, 4) - 1)
  
  
  TSnew <- getrptDate_newest(TS)
  TSnew <- dplyr::rename(TSnew,rptDateEnd=rptDate)
  TSnew$rptDateBeg <- TSnew$rptDateEnd %m+% Nbin
  
  TSFdata <- TSFdata %>% dplyr::full_join(TSnew,by='stockID') %>% 
    dplyr::filter(rptDate>rptDateBeg,rptDate<=rptDateEnd) %>% 
    dplyr::arrange(date,stockID,rptDate)
  TSnew %>% rowwise() %>% do(i = seq(.$x, .$y))
  .Last.value %>% summarise(n = length(i))
  
  
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

