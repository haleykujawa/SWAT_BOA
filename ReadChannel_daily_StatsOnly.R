##reading stream output
## Same as read channel daily 7, but only reads raw output instead of reading processed output, just in case I make changes and run again
## Note monthly and annual summaries are only a SUM, for variables like discharge does not make sense to use these outputs..
## This script pulls Heidelberg obs data instead of SWMP data from the NERR database
## Depending on the version of SWAT+, you need to ensure the headers are correct
## Different from read channel daily 6 bc of directories it reads/saves to

xlib = c("readtext","dplyr","splitstackshape","stringr","rlang","reshape2","tidyr","tictoc",
         "here","rstudioapi","rapportools","lfstat","dplyr","splitstackshape","stringr",
         "rlang","hydroGOF","ggplot2","scales",
         "grid","ggpubr","cowplot","tibble","gg.gap","data.table",
         "qdapRegex","lubridate")
lapply(xlib, require, character.only=T) ; rm(xlib)





########################## Berlin Rd channel no ########################################
BR<-46
WM<-1

headers<-c("jday",	"mon",	"day",	"yr",	"unit",	"gis_id",	"name",	"areaha",	"precipha.m",	"evapha.m",	
"seepha.m",	"flo_storm.3.s",	"sed_stormtons",	"orgn_storkgN",	"sedp_storkgP",	"no3_storkgN",	"solp_storkgP",
"chla_storkg",	"nh3_storkgN",	"no2_storkgN",	"cbod_storkg",	"dox_storkg",	"san_stortons",	"sil_stortons",	"cla_stortons",	"sag_stortons",
"lag_stortons",	"grv_stortons",	"null1",	"flo_inm.3.s",	"sed_inmtons",	"orgn_inkgN",	"sedp_inkgP",	"no3_inkgN",
"solp_inkgP",	"chla_inkg",	"nh3_inkgN",	"no2_inkgN",	"cbod_inkg",	"dox_inkg",	"san_intons",	"sil_intons",	"cla_intons",
"sag_intons",	"lag_intons",	"grv_intons",	"null",	"flo_outm.3.s",	"sed_outmtons",	"orgn_outkgN",	"sedp_outkgP",	"no3_outkgN",
"solp_outkgP",	"chla_outkg",	"nh3_outkgN",	"no2_outkgN",	"cbod_outkg",	"dox_outkg",	"san_outtons",	"sil_outtons",	"cla_outtons",
"sag_outtons",	"lag_outtons",	"grv_outtons",	"null2","water_tempdegC")#"null3","null4","null5","null6","null7")



########################################################################################
######################### read in observed data  #######################################
########################################################################################
#currently just pull 'Date' column, could use grepl so wold find column that says 'date'

#This assumes the layering of text in out is 
#scenario directory
#Scenario (contains folders leading to scenarios and observed)
#text in out


Obs<-read.csv(here("ObsData","obsdata_HDBRG.csv")) #observed data
var_lookup<-read.csv(here("ObsData","var_lookup.csv")) 


Obs$date<-as.Date(Obs$date,format="%Y-%m-%d")

################################################################################
################## Read in channel output ######################################
################################################################################


##### Check if processed channel data already exists

  
  print("reading from channel output")


tmp <- file(here("TxtInOut",'channel_sd_day.txt'))
open(tmp, "r") #read

#read past headerlines
readLines(tmp, n = 3) 



###### read in simulated data columns #########

#this takes ~23-32 (?) minutes
tic("reading daily data")
data<-readLines(tmp, n = -1)
close(tmp)




DF<-strsplit(data,split=" ")
DF<-lapply(DF, function(z){ z[z != ""]}) 
DF<-data.frame(do.call(rbind, DF)) #unlist
colnames(DF)<-headers


DF$date<-as.Date(paste(DF$mon,DF$day,DF$yr,sep="/"), format="%m/%d/%Y")              # add date column
DF[,c(1:6,8:(ncol(DF)-1))]<-as.numeric(unlist(DF[,c(1:6,8:(ncol(DF)-1))]))           # convert to numerics

toc()

################### add simulated columns of concentrations, TP loads ################################################

#have to use grepl to grab bc depending on if file is read from 
#file or read from a table the flo_out variable is labeled differently

#currently there's days with 0 flow, replacing Inf with NA

DF$solpconc_outmgl <- (DF$solp_outkgP/DF[,grepl("flo_out",colnames(DF))])*(10^6/(86400*1000))
DF$solpconc_outmgl[DF$solpconc_outmgl == Inf] <- NA

DF$no3conc_outmgl <- (DF$no3_outkgN/DF[,grepl("flo_out",colnames(DF))])*(10^6/(86400*1000))
DF$no3conc_outmgl[DF$no3conc_outmgl == Inf] <- NA

DF$totp_outkgP <- DF$solp_outkgP + DF$sedp_outkgP 

DF$totn_outkgN <- DF$orgn_outkgN + DF$no2_outkgN + DF$no3_outkgN + DF$nh3_outkgN

DF$no23_outkgN <- DF$no2_outkgN + DF$no3_outkgN 


DF$totpconc_outmgl <- (DF$totp_outkgP/DF[,grepl("flo_out",colnames(DF))])*(10^6/(86400*1000))
DF$totpconc_outmgl[DF$totpconc_outmgl == Inf] <- NA

DF$totnconc_outmgl <- (DF$totn_outkgN/DF[,grepl("flo_out",colnames(DF))])*(10^6/(86400*1000))
DF$totnconc_outmgl [DF$totnconc_outmgl  == Inf] <- NA

DF$no23conc_outmgl <- (DF$no23_outkgN/DF[,grepl("flo_out",colnames(DF))])*(10^6/(86400*1000))
DF$no23conc_outmgl [DF$no23conc_outmgl  == Inf] <- NA

DF$sedconc_outmgl <- (DF$sed_outmtons/DF[,grepl("flo_out",colnames(DF))])*(10^9/(86400*1000))
DF$sedconc_outmgl [DF$sedconc_outmgl  == Inf] <- NA


###################################################################################################
###################### Combine simulated and observed data ########################################
###################################################################################################

#combine channel 47 with observed Berlin Rd data
#Use USGS discharge for 10 years and Heidelberg Observed data for the 5 year period


#pull channel data

BR_data<-DF[DF$gis_id==BR|DF$gis_id==WM ,]
BR_data$gis_id<-as.character(BR_data$gis_id)
#replace channel no with BR and WM identifiers

BR_data$gis_id[BR_data$gis_id == as.character(BR)]<-"BR"
BR_data$gis_id[BR_data$gis_id == as.character(WM)]<-"WM"



#format observed data
Obs<-reshape2::dcast(Obs,date + station ~variable)

var_lookup<-var_lookup[!is.empty(var_lookup$var_sim),] #remove observed variables with no observed equivalent
Obs<-Obs[,c(1:2,grep(paste(var_lookup$var_obs, collapse="|"),colnames(Obs)))] #remove observed data columns with no simulated equivalent
names(Obs)[!is.na( match(colnames(Obs),var_lookup$var_obs) )] <- as.character(var_lookup$var_sim[na.omit( match(colnames(Obs),var_lookup$var_obs) )])   #replace observed data names with simulated names
colnames(Obs)[3:length(colnames(Obs))]<-paste0("obs_",colnames(Obs)[3:length(colnames(Obs))]) #add obs to column names to distinguish from simulated values


BR_data<-left_join(BR_data,Obs,by=c("date","gis_id"="station")) #combine observed and simulated data


#Grab precipitation data
pcp<-data.frame(DF$date[DF$gis_id==1], (DF[DF$gis_id==1,grepl("precip",colnames(DF))]*1000/DF$areaha[DF$gis_id==1]))
colnames(pcp)<-c("date","pcp_mm")




#write.csv(BR_data,"SimObs_channelDaily.csv",row.names=F)
#write.csv(pcp,"dailypcp.csv",row.names=F)


SimObs<-BR_data
setwd(here("outputs"))


###### Hydro stats ##################################
#extract observed variable names
obs_dat<-colnames(SimObs)[grep("obs",colnames(SimObs))]
obs_dat<-unlist(ex_between(obs_dat, "obs_", "_out"))

WM_stats<-data.frame(obs_dat)
WM_stats$dailyNSE<-NA
WM_stats$monthlyNSE<-NA
WM_stats$dailypbias<-NA
WM_stats$monthlypbias<-NA
#hydrostats$dailymax<-NA
#hydrostats$dailymin<-NA
#hydrostats$dailyavg<-NA


BR_stats<-data.frame(obs_dat)
BR_stats$dailyNSE<-NA
BR_stats$monthlyNSE<-NA
BR_stats$dailypbias<-NA
BR_stats$monthlypbias<-NA
#hydrostats$dailymax<-NA
#hydrostats$dailymin<-NA
#hydrostats$dailyavg<-NA



###### Daily plot function ###########################

plotDaily <- function(mydf,max_yaxis,start_date,end_date) {
  
  plot1<-mydf %>%
    filter(date >= as.Date(start_date) & date <= as.Date(end_date)) %>%
    ggplot(aes(x = date, y = value, group=variable, color=variable))+geom_line()+
    scale_linetype_manual(values=c("dashed","solid"))+
    scale_color_manual(values=c("red","black")) + 
    aes(group=rev(variable)) + 
    scale_x_date(date_breaks="3 months",date_labels= "%b %Y",expand=c(0,0))+
    labs(y=y_axis_lab)+
    scale_y_continuous(expand=c(0,0),limits=c(0,max_yaxis))+
    
    theme(panel.background = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
          axis.line = element_line(colour = "black"),axis.text.x=element_text(angle=90,vjust=0.5),axis.title.x=element_blank(),
          legend.position="bottom")
  
  
  return(plot1)
  
}

plotDaily_point <- function(mydf,max_yaxis,start_date,end_date) {
  
  plot1<-mydf %>%
    filter(date >= as.Date(start_date) & date <= as.Date(end_date)) %>%
    ggplot(aes(x = date, y = value, group=variable, color=variable))+geom_point()+
    scale_linetype_manual(values=c("dashed","solid"))+
    scale_color_manual(values=c("red","black")) + 
    aes(group=rev(variable)) + 
    scale_x_date(date_breaks="3 months",date_labels= "%b %Y",expand=c(0,0))+
    labs(y=y_axis_lab)+
    scale_y_continuous(expand=c(0,0),limits=c(0,max_yaxis))+
    
    theme(panel.background = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
          axis.line = element_line(colour = "black"),axis.text.x=element_text(angle=90,vjust=0.5),axis.title.x=element_blank(),
          legend.position="bottom")
  
  
  return(plot1)
  
}

###### PCP plots for discharge, 2013-2017 ############
pcp_monthly<-data.frame(tapply(pcp$pcp_mm, paste0(substr(pcp$date,6,7),"-01-",substr(pcp$date, 1,4)) , sum))
setDT(pcp_monthly, keep.rownames = TRUE)[] #move row names (date) to a column
colnames(pcp_monthly)<-c("date","pcp_mm")
pcp_monthly$date<-as.Date(pcp_monthly$date,format="%m-%d-%Y")

#Daily
pcp_daily_plot<-pcp %>%
  filter(date >= as.Date("2013-01-01") & date <= as.Date("2017-12-31")) %>%
  ggplot( aes(x=date,y=pcp_mm))+geom_bar(stat="identity")+ scale_x_date(date_breaks="3 months",date_labels= "%b %Y",expand=c(0,0))+scale_y_reverse(expand=c(0,0))+ylab("precipitation (mm)")+
  theme(panel.background = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        axis.line = element_line(colour = "black"),axis.title.x=element_blank(),axis.text.x=element_text(angle=90,vjust=0.5))

pcp_monthly_plot<-pcp_monthly %>%
  filter(date >= as.Date("2013-01-01") & date <= as.Date("2017-12-31")) %>%
  ggplot(aes(x=date,y=pcp_mm))+geom_bar(stat="identity")+ scale_x_date(date_breaks="3 months",date_labels= "%b %Y",expand=c(0,0))+scale_y_reverse(expand=c(0,0))+ylab("precipitation (mm)")+
  theme(panel.background = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        axis.line = element_line(colour = "black"),axis.title.x=element_blank(),axis.text.x=element_text(angle=90,vjust=0.5))


#### Calculate daily NSE and pbias between two dates using an unmelted data frame ###############################
# One column has obs_var and one just var to be able to distinguish which is obs and which is sim 

NSE_filterDates<-function(mydf,start_date,end_date,varname){
  
  NSE(mydf[mydf$date >= as.Date(start_date) & mydf$date <= as.Date(end_date),!grepl("obs",colnames(mydf)) & grepl(varname,colnames(mydf))],
      mydf[mydf$date >= as.Date(start_date) & mydf$date <= as.Date(end_date),grepl("obs",colnames(mydf))],na.rm=T)
  
}

Pbias_filterDates<-function(mydf,start_date,end_date,varname){
  
  pbias(mydf[mydf$date >= as.Date(start_date) & mydf$date <= as.Date(end_date),!grepl("obs",colnames(mydf)) & grepl(varname,colnames(mydf))],
        mydf[mydf$date >= as.Date(start_date) & mydf$date <= as.Date(end_date),grepl("obs",colnames(mydf))],na.rm=T)
  
}




##### Discharge ##################################################
graphname<-"Discharge"
var<-"flo"
y_axis_lab<- "Discharge (cms)"


WM<-SimObs[grepl("WM",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]
BR<-SimObs[grepl("BR",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]

# daily stats #


WM_stats$dailyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2013-01-01","2017-12-31",var)
WM_stats$dailypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2013-01-01","2017-12-31",var)

BR_stats$dailyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2013-01-01","2017-12-31",var)
BR_stats$dailypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2013-01-01","2017-12-31",var)

##### calculate baseflow ##########
BR$baseflow_obs<-baseflow(BR$obs_flo_outm.3.s)
BR$baseflow_sim<-baseflow(BR$flo_outm.3.s)

BR_baseflow<-BR[,c("date","baseflow_obs","baseflow_sim")]
BR_baseflow_table<-rbind(c("obs_baseflow","sim_baseflow"),c(mean(BR_baseflow$baseflow_obs,na.rm=T), mean(BR_baseflow$baseflow_sim,na.rm=T)))

BR_baseflow<-reshape2::melt(BR_baseflow, id="date")
BR_baseflow$date<-as.Date(BR_baseflow$date)

baseflow_daily<- plotDaily(BR_baseflow,max(BR_baseflow[,c(3)]),"2013-01-01","2017-12-31")
ggsave("daily_baseflow_separation.png",last_plot(),height=300,width=500, unit="mm")
write.csv(BR_baseflow_table,"baseflow.csv",row.names=F)




BR<-subset(BR,select=-c(baseflow_obs,baseflow_sim)) #remove baseflow calculations from BR

# write obs / sim file for copy pasting into excel sheet
write.csv(BR,paste0(var,"BR.csv"),row.names=F) 
write.csv(WM,paste0(var,"WM.csv"),row.names=F) 


WM<-reshape2::melt(WM, id="date")
WM$date<-as.Date(WM$date)

BR<-reshape2::melt(BR, id="date")
BR$date<-as.Date(BR$date)

#plot timeseries of calibration

#find max value for plots in calibration period
if (max(WM$value[WM$date >= "2013-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2013-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2013-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2013-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_daily<- plotDaily(WM,max_yaxis,"2013-01-01","2017-12-31")
BR_daily<- plotDaily(BR,max_yaxis,"2013-01-01","2017-12-31")


plot_grid(pcp_daily_plot,WM_daily,BR_daily,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_daily.png"),last_plot(),height=300,width=500, unit="mm")

#monthly calculations
BR$date<-floor_date(BR$date, "month")
WM$date<-floor_date(WM$date, "month")

BR<-BR %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))
WM<-WM %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))

# monthly stats #
BR<-reshape2::dcast(BR,date~variable)
WM<-reshape2::dcast(WM,date~variable)

WM_stats$monthlyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2013-01-01","2017-12-31",var)
WM_stats$monthlypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2013-01-01","2017-12-31",var)

BR_stats$monthlyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2013-01-01","2017-12-31",var)
BR_stats$monthlypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2013-01-01","2017-12-31",var)

# remelt #
BR<-reshape2::melt(BR, id="date")
WM<-reshape2::melt(WM, id="date")

#find max value for plots in calibration period
if (max(WM$value[WM$date >= "2013-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2013-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2013-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2013-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_monthly<- plotDaily(WM,max_yaxis,"2013-01-01","2017-12-31")
BR_monthly<- plotDaily(BR,max_yaxis,"2013-01-01","2017-12-31")


plot_grid(pcp_daily_plot,WM_monthly,BR_monthly,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_monthly.png"),last_plot(),height=300,width=500, unit="mm")



###### PCP plots for water quality, 2015-2017 ############
#Daily
pcp_daily_plot<-pcp %>%
  filter(date >= as.Date("2015-01-01") & date <= as.Date("2017-12-31")) %>%
  ggplot( aes(x=date,y=pcp_mm))+geom_bar(stat="identity")+ scale_x_date(date_breaks="3 months",date_labels= "%b %Y",expand=c(0,0))+scale_y_reverse(expand=c(0,0))+ylab("precipitation (mm)")+
  theme(panel.background = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        axis.line = element_line(colour = "black"),axis.title.x=element_blank(),axis.text.x=element_text(angle=90,vjust=0.5))

pcp_monthly_plot<-pcp_monthly %>%
  filter(date >= as.Date("2015-01-01") & date <= as.Date("2017-12-31")) %>%
  ggplot(aes(x=date,y=pcp_mm))+geom_bar(stat="identity")+ scale_x_date(date_breaks="3 months",date_labels= "%b %Y",expand=c(0,0))+scale_y_reverse(expand=c(0,0))+ylab("precipitation (mm)")+
  theme(panel.background = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        axis.line = element_line(colour = "black"),axis.title.x=element_blank(),axis.text.x=element_text(angle=90,vjust=0.5))



################ TP conc ##########################################################
graphname<-"TP concentration"
var<-"totpconc"
y_axis_lab<- "TP (mg/l)"


WM<-SimObs[grepl("WM",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]
BR<-SimObs[grepl("BR",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]

# daily stats #


WM_stats$dailyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$dailypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$dailyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$dailypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# write obs / sim file for copy pasting into excel sheet
write.csv(BR,paste0(var,"BR.csv"),row.names=F) 
write.csv(WM,paste0(var,"WM.csv"),row.names=F) 

WM<-reshape2::melt(WM, id="date")
WM$date<-as.Date(WM$date)

BR<-reshape2::melt(BR, id="date")
BR$date<-as.Date(BR$date)

#plot timeseries of calibration

#find max value for plots
#find max value for plots in calibration period
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_daily<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_daily<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_daily,BR_daily,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_daily.png"),last_plot(),height=300,width=500, unit="mm")


BR$date<-floor_date(BR$date, "month")
WM$date<-floor_date(WM$date, "month")

BR<-BR %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))
WM<-WM %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))

# monthly stats #
BR<-reshape2::dcast(BR,date~variable)
WM<-reshape2::dcast(WM,date~variable)

WM_stats$monthlyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$monthlypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$monthlyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$monthlypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# remelt #
BR<-reshape2::melt(BR, id="date")
WM<-reshape2::melt(WM, id="date")

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_monthly<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_monthly<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_monthly,BR_monthly,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_monthly.png"),last_plot(),height=300,width=500, unit="mm")

########################################################################################################################################

################ TP load ##########################################################
graphname<-"TP load"
var<-"totp"
y_axis_lab<- "TP (kg)"


WM<-SimObs[grepl("WM",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]
BR<-SimObs[grepl("BR",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]

# daily stats #


WM_stats$dailyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$dailypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$dailyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$dailypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# write obs / sim file for copy pasting into excel sheet
write.csv(BR,paste0(var,"BR.csv"),row.names=F) 
write.csv(WM,paste0(var,"WM.csv"),row.names=F) 

WM<-reshape2::melt(WM, id="date")
WM$date<-as.Date(WM$date)

BR<-reshape2::melt(BR, id="date")
BR$date<-as.Date(BR$date)

#plot timeseries of calibration

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_daily<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_daily<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_daily,BR_daily,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_daily.png"),last_plot(),height=300,width=500, unit="mm")


BR$date<-floor_date(BR$date, "month")
WM$date<-floor_date(WM$date, "month")

BR<-BR %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))
WM<-WM %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))

# monthly stats #
BR<-reshape2::dcast(BR,date~variable)
WM<-reshape2::dcast(WM,date~variable)

WM_stats$monthlyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$monthlypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$monthlyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$monthlypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# remelt #
BR<-reshape2::melt(BR, id="date")
WM<-reshape2::melt(WM, id="date")

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_monthly<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_monthly<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_monthly,BR_monthly,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_monthly.png"),last_plot(),height=300,width=500, unit="mm")
########################################################################################################################################

################ solp conc ##########################################################
graphname<-"solp concentration"
var<-"solpconc"
y_axis_lab<- "SRP (mg/l)"


WM<-SimObs[grepl("WM",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]
BR<-SimObs[grepl("BR",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]

# daily stats #


WM_stats$dailyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$dailypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$dailyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$dailypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# write obs / sim file for copy pasting into excel sheet
write.csv(BR,paste0(var,"BR.csv"),row.names=F) 
write.csv(WM,paste0(var,"WM.csv"),row.names=F) 

WM<-reshape2::melt(WM, id="date")
WM$date<-as.Date(WM$date)

BR<-reshape2::melt(BR, id="date")
BR$date<-as.Date(BR$date)

#plot timeseries of calibration

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_daily<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_daily<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_daily,BR_daily,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_daily.png"),last_plot(),height=300,width=500, unit="mm")


BR$date<-floor_date(BR$date, "month")
WM$date<-floor_date(WM$date, "month")

BR<-BR %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))
WM<-WM %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))

# monthly stats #
BR<-reshape2::dcast(BR,date~variable)
WM<-reshape2::dcast(WM,date~variable)

WM_stats$monthlyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$monthlypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$monthlyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$monthlypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# remelt #
BR<-reshape2::melt(BR, id="date")
WM<-reshape2::melt(WM, id="date")

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_monthly<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_monthly<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_monthly,BR_monthly,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_monthly.png"),last_plot(),height=300,width=500, unit="mm")

########################################################################################################################################

################ solp load ##########################################################
graphname<-"Solp load"
var<-"solp"
y_axis_lab<- "SRP (kg)"


WM<-SimObs[grepl("WM",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]
BR<-SimObs[grepl("BR",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]

# daily stats #


WM_stats$dailyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$dailypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$dailyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$dailypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# write obs / sim file for copy pasting into excel sheet
write.csv(BR,paste0(var,"BR.csv"),row.names=F) 
write.csv(WM,paste0(var,"WM.csv"),row.names=F) 

WM<-reshape2::melt(WM, id="date")
WM$date<-as.Date(WM$date)

BR<-reshape2::melt(BR, id="date")
BR$date<-as.Date(BR$date)

#plot timeseries of calibration

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_daily<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_daily<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_daily,BR_daily,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_daily.png"),last_plot(),height=300,width=500, unit="mm")


BR$date<-floor_date(BR$date, "month")
WM$date<-floor_date(WM$date, "month")

BR<-BR %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))
WM<-WM %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))

# monthly stats #
BR<-reshape2::dcast(BR,date~variable)
WM<-reshape2::dcast(WM,date~variable)

WM_stats$monthlyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$monthlypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$monthlyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$monthlypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# remelt #
BR<-reshape2::melt(BR, id="date")
WM<-reshape2::melt(WM, id="date")

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_monthly<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_monthly<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_monthly,BR_monthly,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_monthly.png"),last_plot(),height=300,width=500, unit="mm")
########################################################################################################################################

################ no23 conc ##########################################################
graphname<-"no23 concentration"
var<-"no23conc"
y_axis_lab<- "NO23 (mg/l)"


WM<-SimObs[grepl("WM",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]
BR<-SimObs[grepl("BR",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]

# daily stats #


WM_stats$dailyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$dailypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$dailyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$dailypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# write obs / sim file for copy pasting into excel sheet
write.csv(BR,paste0(var,"BR.csv"),row.names=F) 
write.csv(WM,paste0(var,"WM.csv"),row.names=F) 

WM<-reshape2::melt(WM, id="date")
WM$date<-as.Date(WM$date)

BR<-reshape2::melt(BR, id="date")
BR$date<-as.Date(BR$date)

#plot timeseries of calibration

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_daily<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_daily<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_daily,BR_daily,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_daily.png"),last_plot(),height=300,width=500, unit="mm")


BR$date<-floor_date(BR$date, "month")
WM$date<-floor_date(WM$date, "month")

BR<-BR %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))
WM<-WM %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))

# monthly stats #
BR<-reshape2::dcast(BR,date~variable)
WM<-reshape2::dcast(WM,date~variable)

WM_stats$monthlyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$monthlypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$monthlyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$monthlypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# remelt #
BR<-reshape2::melt(BR, id="date")
WM<-reshape2::melt(WM, id="date")

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_monthly<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_monthly<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_monthly,BR_monthly,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_monthly.png"),last_plot(),height=300,width=500, unit="mm")

########################################################################################################################################

################ no23 load ##########################################################
graphname<-"NO23 load"
var<-"no23"
y_axis_lab<- "NO23 (kg)"


WM<-SimObs[grepl("WM",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]
BR<-SimObs[grepl("BR",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]

# daily stats #


WM_stats$dailyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$dailypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$dailyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$dailypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# write obs / sim file for copy pasting into excel sheet
write.csv(BR,paste0(var,"BR.csv"),row.names=F) 
write.csv(WM,paste0(var,"WM.csv"),row.names=F) 

WM<-reshape2::melt(WM, id="date")
WM$date<-as.Date(WM$date)

BR<-reshape2::melt(BR, id="date")
BR$date<-as.Date(BR$date)

#plot timeseries of calibration

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_daily<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_daily<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_daily,BR_daily,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_daily.png"),last_plot(),height=300,width=500, unit="mm")


BR$date<-floor_date(BR$date, "month")
WM$date<-floor_date(WM$date, "month")

BR<-BR %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))
WM<-WM %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))

# monthly stats #
BR<-reshape2::dcast(BR,date~variable)
WM<-reshape2::dcast(WM,date~variable)

WM_stats$monthlyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$monthlypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$monthlyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$monthlypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# remelt #
BR<-reshape2::melt(BR, id="date")
WM<-reshape2::melt(WM, id="date")

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_monthly<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_monthly<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_monthly,BR_monthly,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_monthly.png"),last_plot(),height=300,width=500, unit="mm")
########################################################################################################################################

################ TN Conc ##########################################################
graphname<-"TN Concentration"
var<-"totnconc"
y_axis_lab<- "TN (mg/l)"


WM<-SimObs[grepl("WM",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]
BR<-SimObs[grepl("BR",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]

# daily stats #


WM_stats$dailyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$dailypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$dailyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$dailypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# write obs / sim file for copy pasting into excel sheet
write.csv(BR,paste0(var,"BR.csv"),row.names=F) 
write.csv(WM,paste0(var,"WM.csv"),row.names=F) 

WM<-reshape2::melt(WM, id="date")
WM$date<-as.Date(WM$date)

BR<-reshape2::melt(BR, id="date")
BR$date<-as.Date(BR$date)

#plot timeseries of calibration

#find max value for plots
if (max(WM$value,na.rm=T) > max(BR$value,na.rm=T)){
  
  max_yaxis<- max(WM$value,na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value,na.rm=T)
  
}


WM_daily<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_daily<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_daily,BR_daily,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_daily.png"),last_plot(),height=300,width=500, unit="mm")


BR$date<-floor_date(BR$date, "month")
WM$date<-floor_date(WM$date, "month")

BR<-BR %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))
WM<-WM %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))

# monthly stats #
BR<-reshape2::dcast(BR,date~variable)
WM<-reshape2::dcast(WM,date~variable)

WM_stats$monthlyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$monthlypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$monthlyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$monthlypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# remelt #
BR<-reshape2::melt(BR, id="date")
WM<-reshape2::melt(WM, id="date")

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}

WM_monthly<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_monthly<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_monthly,BR_monthly,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_monthly.png"),last_plot(),height=300,width=500, unit="mm")
########################################################################################################################################

################ TN load ##########################################################
graphname<-"TN Load"
var<-"totn"
y_axis_lab<- "TN (mg/l)"


WM<-SimObs[grepl("WM",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]
BR<-SimObs[grepl("BR",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]

# daily stats #


WM_stats$dailyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$dailypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$dailyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$dailypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# write obs / sim file for copy pasting into excel sheet
write.csv(BR,paste0(var,"BR.csv"),row.names=F) 
write.csv(WM,paste0(var,"WM.csv"),row.names=F) 

WM<-reshape2::melt(WM, id="date")
WM$date<-as.Date(WM$date)

BR<-reshape2::melt(BR, id="date")
BR$date<-as.Date(BR$date)

#plot timeseries of calibration

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_daily<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_daily<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_daily,BR_daily,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_daily.png"),last_plot(),height=300,width=500, unit="mm")


BR$date<-floor_date(BR$date, "month")
WM$date<-floor_date(WM$date, "month")

BR<-BR %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))
WM<-WM %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))

# monthly stats #
BR<-reshape2::dcast(BR,date~variable)
WM<-reshape2::dcast(WM,date~variable)

WM_stats$monthlyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$monthlypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$monthlyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$monthlypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# remelt #
BR<-reshape2::melt(BR, id="date")
WM<-reshape2::melt(WM, id="date")

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_monthly<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_monthly<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_monthly,BR_monthly,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_monthly.png"),last_plot(),height=300,width=500, unit="mm")
########################################################################################################################################

################ Sed conc ##########################################################
graphname<-"Sed conc"
var<-"sedconc"
y_axis_lab<- "Suspended sediment (mg/l)"


WM<-SimObs[grepl("WM",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]
BR<-SimObs[grepl("BR",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]

# daily stats #


WM_stats$dailyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$dailypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$dailyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$dailypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# write obs / sim file for copy pasting into excel sheet
write.csv(BR,paste0(var,"BR.csv"),row.names=F) 
write.csv(WM,paste0(var,"WM.csv"),row.names=F) 

WM<-reshape2::melt(WM, id="date")
WM$date<-as.Date(WM$date)

BR<-reshape2::melt(BR, id="date")
BR$date<-as.Date(BR$date)

#plot timeseries of calibration

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_daily<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_daily<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_daily,BR_daily,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_daily.png"),last_plot(),height=300,width=500, unit="mm")


BR$date<-floor_date(BR$date, "month")
WM$date<-floor_date(WM$date, "month")

BR<-BR %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))
WM<-WM %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))

# monthly stats #
BR<-reshape2::dcast(BR,date~variable)
WM<-reshape2::dcast(WM,date~variable)

WM_stats$monthlyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$monthlypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$monthlyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$monthlypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# remelt #
BR<-reshape2::melt(BR, id="date")
WM<-reshape2::melt(WM, id="date")

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_monthly<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_monthly<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_monthly,BR_monthly,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_monthly.png"),last_plot(),height=300,width=500, unit="mm")
########################################################################################################################################

################ Sed load ##########################################################
graphname<-"Sed load"
var<-"sed"
y_axis_lab<- "Suspended sediment (metric ton)"


WM<-SimObs[grepl("WM",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]
BR<-SimObs[grepl("BR",SimObs$gis_id),grepl(paste(c("date",paste0(var,"_out")),collapse="|"),colnames(SimObs))]

# daily stats #


WM_stats$dailyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$dailypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$dailyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$dailypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# write obs / sim file for copy pasting into excel sheet
write.csv(BR,paste0(var,"BR.csv"),row.names=F) 
write.csv(WM,paste0(var,"WM.csv"),row.names=F) 

WM<-reshape2::melt(WM, id="date")
WM$date<-as.Date(WM$date)

BR<-reshape2::melt(BR, id="date")
BR$date<-as.Date(BR$date)

#plot timeseries of calibration

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_daily<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_daily<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_daily,BR_daily,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_daily.png"),last_plot(),height=300,width=500, unit="mm")


BR$date<-floor_date(BR$date, "month")
WM$date<-floor_date(WM$date, "month")

BR<-BR %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))
WM<-WM %>%
  group_by(date,variable) %>%
  summarize(value=mean(value,na.rm=T))

# monthly stats #
BR<-reshape2::dcast(BR,date~variable)
WM<-reshape2::dcast(WM,date~variable)

WM_stats$monthlyNSE[WM_stats$obs_dat == var]<-NSE_filterDates(WM,"2015-05-05","2017-12-31",var)
WM_stats$monthlypbias[WM_stats$obs_dat == var]<-Pbias_filterDates(WM,"2015-05-05","2017-12-31",var)

BR_stats$monthlyNSE[BR_stats$obs_dat == var]<-NSE_filterDates(BR,"2015-05-05","2017-12-31",var)
BR_stats$monthlypbias[BR_stats$obs_dat == var]<-Pbias_filterDates(BR,"2015-05-05","2017-12-31",var)

# remelt #
BR<-reshape2::melt(BR, id="date")
WM<-reshape2::melt(WM, id="date")

#find max value for plots
if (max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T) > max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)){
  
  max_yaxis<- max(WM$value[WM$date >= "2015-01-01" & WM$date <= "2017-12-31"],na.rm=T)
  
}else{
  
  max_yaxis<-max(BR$value[BR$date >= "2015-01-01" & BR$date <= "2017-12-31"],na.rm=T)
  
}


WM_monthly<- plotDaily(WM,max_yaxis,"2015-05-05","2017-12-31")
BR_monthly<- plotDaily(BR,max_yaxis,"2015-05-05","2017-12-31")


plot_grid(pcp_daily_plot,WM_monthly,BR_monthly,ncol=1,rel_heights  = c(1, 2,2),align="vh",axis="l",labels=c('','WM',"BR"),hjust=-2)

ggsave(paste0(var,"_monthly.png"),last_plot(),height=300,width=500, unit="mm")
########################################################################################################################################
BR_stats<-reshape2::melt(BR_stats, id="obs_dat")
WM_stats<-reshape2::melt(WM_stats, id="obs_dat")


write.csv(BR_stats,"BR_stats.csv",row.names=F)
write.csv(WM_stats,"WM_stats.csv",row.names=F)


