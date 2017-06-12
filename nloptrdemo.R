
#' TS.getFinStat_ts
#'
#' get stats of financial indicators from tinysoft.
#' @param TS
#' @param funchar see \link[QDataGet]{TS.getFin_rptTS}
#' @param Nbin
#' @param changerate
#' @param stattype
#' @examples 
#' RebDates <- getRebDates(as.Date('2013-01-31'),as.Date('2017-05-31'),'month')
#' TS <- getTS(RebDates,'EI000905')
#' funchar <- '"factorscore",LastQuarterData(RDate,46078,0)'
#' 
#' 
#' funchar <- '"eps",LastQuarterData(RDate,9900000,0)'
TS.getFinStat_ts <- function(TS,funchar,varname = funchar,Nbin=lubridate::years(-3),changerate=FALSE,
                       stattype=c('mean','slope','sd','mean/sd','slope/sd')){
  type <- match.arg(type)
  stattype <- match.arg(stattype)
  
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
 
  
  TSFdata <- rptTS.getFin_ts(rptTS,funchar)
  factorname <- colnames(TSFdata)[3]
  colnames(TSFdata) <- c("rptDate","stockID","factorscore")
  
  rtndata <- TSFdata %>% dplyr::group_by(stockID) %>%
    dplyr::mutate(growth =(factorscore - dplyr::lag(factorscore, 4))/abs(dplyr::lag(factorscore, 4)))
  rtndata <- na.omit(rtndata)
  rtndata <- rtndata[!is.infinite(rtndata$growth),c("rptDate","stockID","growth")]
  
  
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
  
  TSFdata <- dplyr::left_join(TSnew,rtndata,by=c('stockID','rptDate'))
  TSFdata <- na.omit(TSFdata)
  
  TSFdata <- TSFdata %>% dplyr::group_by(date,stockID) %>% dplyr::mutate(id =row_number())
  N <- max(TSFdata$id)
  TSFdata <- TSFdata %>% dplyr::group_by(date,stockID) %>% dplyr::filter(max(id) > N/2) 
  if(stattype=='mean'){
    TSF <- TSFdata %>%  dplyr::summarise(factorscore=mean(growth))
  }else if(stattype=='sd'){
    TSF <- TSFdata %>%  dplyr::summarise(factorscore=sd(growth))
  }else if(stattype=='mean/sd'){
    TSF <- TSFdata %>%  dplyr::summarise(factorscore=mean(growth)/sd(growth))
  }else if(stattype %in% c('slope','slope/sd')){
    tmp <- TSFdata[1:10000,] %>% do(mod = lm(growth ~ id, data = .))
    tmp <- data.frame(tmp %>% broom::tidy(mod))
    TSF <- tmp[tmp$term=='id',c("date","stockID","estimate")]
    if(stattype=='slope'){
      colnames(TSF) <- c("date","stockID","factorscore")
    }else{
      tmp <- TSFdata %>%  dplyr::summarise(sd=sd(growth))
      TSF <- dplyr::left_join(TSF,tmp,by=c('date','stockID'))
      TSF$factorscore <- TSF$estimate/TSF$sd
      TSF <- transform(TSF,estimate=NULL,sd=NULL)
    }

    
  }
  
  
  TSF <- dplyr::left_join(TS,TSF,by=c('date','stockID'))
  return(TSF)
}







