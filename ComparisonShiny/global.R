
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#ADSM-Shiny
#This should be the folder in which the folder "ADSMDemo" resides.
#make all slashes either double \\ or backslashes /
setwd("~/CEAH/ADSM_Missy/ADSM_Shiny") #user will have to point their setting directory

list.of.packages <- c("ggplot2", "shiny","plotly","xlsx", "jpeg","leaflet","RSQLite","DBI","knitr","gridExtra","igraph",
                      "reshape","maps","rmarkdown")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
#This should be the name of the Folder containing the app and shouldn't need to be changed
     
setwd("~/CEAH/ADSM_Missy/R code basics")
    
    
       #Daily exposure of a specific iteration
    dex1<-read.csv("ModSim18_CC_Moore_21Late_files/ModSim18_CC_Moore_21Late/daily_exposures_25.csv")
    dex2<-read.csv("ModSim18_CC_Moore_Early_files/ModSim18_CC_Moore_Early/daily_exposures_14.csv")
    dex3<-read.csv("ModSim18_CC_MooreVX_21Late_files/ModSim18_CC_MooreVX_21Late/daily_exposures_4.csv")
    dex4<-read.csv("ModSim18_CC_MooreVx_Early_files/daily_exposures_10.csv")
    
    dex1$Scenario<-c(rep("Late",nrow(dex1)))
    dex2$Scenario<-c(rep("Early",nrow(dex2)))
    dex3$Scenario<-c(rep("Vx Late",nrow(dex3)))
    dex4$Scenario<-c(rep("Vx Early",nrow(dex4)))
    
    dex1$lat<-dex1$S_Lat #to have same labels as maps
    dex1$long<-dex1$S_Lon
    dex2$lat<-dex2$S_Lat
    dex2$long<-dex2$S_Lon
    dex3$lat<-dex3$S_Lat #to have same labels as maps
    dex3$long<-dex3$S_Lon
    dex4$lat<-dex4$S_Lat
    dex4$long<-dex4$S_Lon
    
    Compared_dailyExposures<-rbind(dex1,dex2,dex3,dex4)
    
    #Daily event of a specific iteration
    de1<-read.csv("ModSim18_CC_Moore_21Late_files/ModSim18_CC_Moore_21Late/daily_events_25.csv")
    de2<-read.csv("ModSim18_CC_Moore_Early_files/ModSim18_CC_Moore_Early/daily_events_14.csv")
    de3<-read.csv("ModSim18_CC_MooreVX_21Late_files/ModSim18_CC_MooreVX_21Late/daily_events_4.csv")
    de4<-read.csv("ModSim18_CC_MooreVx_Early_files/daily_events_10.csv")
    
    
    de1$Scenario<-c(rep("Late",nrow(de1)))
    de2$Scenario<-c(rep("Early",nrow(de2)))
    de3$Scenario<-c(rep("Vx Late",nrow(de3)))
    de4$Scenario<-c(rep("Vx Early",nrow(de4)))
    
    Compared_dailyEvents<-rbind(de1,de2,de3,de4)
    
    
    
    Ty<-table(Compared_dailyEvents$Type,Compared_dailyEvents$Reason,Compared_dailyEvents$Scenario)
    Ty<-data.frame(Ty) #ggplot works with data frames
    colnames(Ty)<-c("Veterinary_action", "Var2", "Scenario","Freq")
    
    
    
    S1farm<-cbind(dex1$Source_ID, dex1$Recipient_ID, dex1$Scenario)
    colnames(S1farm)<-c("SourceID","RecipientID", "Scenario")
    S1farm<-as.data.frame(S1farm)
    
    S2farm<-cbind(dex2$Source_ID, dex2$Recipient_ID, dex2$Scenario)
    colnames(S2farm)<-c("SourceID","RecipientID", "Scenario")
    S2farm<-as.data.frame(S2farm)
    
    S3farm<-cbind(dex3$Source_ID, dex3$Recipient_ID, dex3$Scenario)
    colnames(S3farm)<-c("SourceID","RecipientID", "Scenario")
    S3farm<-as.data.frame(S3farm)
    
    S4farm<-cbind(dex4$Source_ID, dex4$Recipient_ID, dex4$Scenario)
    colnames(S4farm)<-c("SourceID","RecipientID", "Scenario")
    S4farm<-as.data.frame(S4farm)
    
    
       
    
       
      conCCMoore21Late <- dbConnect(RSQLite::SQLite(), "~/CEAH/ADSM_Missy/R code basics/ModSim18_CC_Moore_21Late/ModSim18_CC_Moore_21Late.sqlite3")
      conCCMooreEarly <- dbConnect(RSQLite::SQLite(), "~/CEAH/ADSM_Missy/R code basics/ModSim18_CC_Moore_Early/ModSim18_CC_Moore_Early.sqlite3")
      conCCMooreVX21Late <- dbConnect(RSQLite::SQLite(), "~/CEAH/ADSM_Missy/R code basics/ModSim18_CC_MooreVX_21Late/ModSim18_CC_MooreVX_21Late.sqlite3")
      conCCMooreVxEarly <- dbConnect(RSQLite::SQLite(), "~/CEAH/ADSM_Missy/R code basics/ModSim18_CC_MooreVx_Early/ModSim18_CC_MooreVX_Early.sqlite3")
      
      #Scenario 1

      
      EpiCurve<-dbSendQuery(conCCMoore21Late, statement = paste("SELECT day, Sum(infnU), Sum(detnU)",
                                                                "FROM  Results_dailybyproductiontype r",
                                                                "WHERE 1=1","AND production_type_id is null",
                                                                "and last_day < 1","group by day","order by 1, 2"))
      EpiCur<-dbFetch(EpiCurve)
      
    
      
      #Scenario 2
      
    
      
      EpiCurve<-dbSendQuery(conCCMooreEarly, statement = paste("SELECT day, Sum(infnU), Sum(detnU)",
                                                               "FROM  Results_dailybyproductiontype r",
                                                               "WHERE 1=1","AND production_type_id is null",
                                                               "and last_day < 1","group by day","order by 1, 2"))
      EpiCur2<-dbFetch(EpiCurve)
      
      #Scenario 3
      
      
      EpiCurve<-dbSendQuery(conCCMooreVX21Late, statement = paste("SELECT day, Sum(infnU), Sum(detnU)",
                                                                "FROM  Results_dailybyproductiontype r",
                                                                "WHERE 1=1","AND production_type_id is null",
                                                                "and last_day < 1","group by day","order by 1, 2"))
      EpiCur3<-dbFetch(EpiCurve)
      
      
      #Scenario 4
      
      
      
      EpiCurve<-dbSendQuery(conCCMooreVxEarly, statement = paste("SELECT day, Sum(infnU), Sum(detnU)",
                                                               "FROM  Results_dailybyproductiontype r",
                                                               "WHERE 1=1","AND production_type_id is null",
                                                               "and last_day < 1","group by day","order by 1, 2"))
      EpiCur4<-dbFetch(EpiCurve)
      
    
      #Creating variable scenario per table
      EpiCur$Scenario<-c(rep("Late",nrow(EpiCur)))
      EpiCur2$Scenario<-c(rep("Early",nrow(EpiCur2)))
      EpiCur3$Scenario<-c(rep("Vx Late",nrow(EpiCur3)))
      EpiCur4$Scenario<-c(rep("Vx Early",nrow(EpiCur4)))
      
      
      #Making Master Tables
      EPICURVE<-rbind(EpiCur, EpiCur2, EpiCur3, EpiCur4);colnames(EPICURVE)<-c("day", "sumInfnU", "sumdetnU","Scenario")
      
     
    
    #Scenario 1
      
      
      OutbreakDuration <- dbSendQuery(conCCMoore21Late, statement = paste("SELECT iteration, diseaseduration",
                                                                          "FROM  Results_dailycontrols r",
                                                                          "WHERE 1=1",
                                                                          "AND last_day >0","order by 1"))
      OutDur<-dbFetch(OutbreakDuration)
      
      
      #Scenario 2
      
      OutbreakDuration <- dbSendQuery(conCCMooreEarly, statement = paste("SELECT iteration, diseaseduration",
                                                                         "FROM  Results_dailycontrols r",
                                                                         "WHERE 1=1",
                                                                         "AND last_day >0","order by 1"))
      OutDur2<-dbFetch(OutbreakDuration)
 
      #Scenario 3
      
      
      OutbreakDuration <- dbSendQuery(conCCMooreVX21Late, statement = paste("SELECT iteration, diseaseduration",
                                                                          "FROM  Results_dailycontrols r",
                                                                          "WHERE 1=1",
                                                                          "AND last_day >0","order by 1"))
      OutDur3<-dbFetch(OutbreakDuration)
      
      
      #Scenario 4
      
      OutbreakDuration <- dbSendQuery(conCCMooreVxEarly, statement = paste("SELECT iteration, diseaseduration",
                                                                         "FROM  Results_dailycontrols r",
                                                                         "WHERE 1=1",
                                                                         "AND last_day >0","order by 1"))
      OutDur4<-dbFetch(OutbreakDuration)
      
      
      OUTBREAKDUR<-rbind(OutDur,OutDur2,OutDur3,OutDur4)
      OUTBREAKDUR$Scenario<-c(rep("Late",nrow(OutDur)),rep("Early",nrow(OutDur2)),rep("Vx Late",nrow(OutDur3)),rep("Vx Early",nrow(OutDur4)))
      
      
    
       #Scenario 1
      
      TotfarmsOutb<-dbSendQuery(conCCMoore21Late, statement = paste("SELECT iteration, day, last_day, infcU, infcA",
                                                                    "FROM  Results_dailybyproductiontype r",
                                                                    "WHERE 1=1", 
                                                                    "AND production_type_id is null",
                                                                    "and last_day = 1", "order by 1"))
      TFarmOut<-dbFetch(TotfarmsOutb)
      
      
      
      #Scenario 2
      
  
      
      TotfarmsOutb<-dbSendQuery(conCCMooreEarly, statement = paste("SELECT iteration, day, last_day, infcU, infcA",
                                                                   "FROM  Results_dailybyproductiontype r",
                                                                   "WHERE 1=1", 
                                                                   "AND production_type_id is null",
                                                                   "and last_day = 1", "order by 1"))
      TFarmOut2<-dbFetch(TotfarmsOutb)
      
      #Scenario 3
      
      TotfarmsOutb<-dbSendQuery(conCCMooreVX21Late, statement = paste("SELECT iteration, day, last_day, infcU, infcA",
                                                                    "FROM  Results_dailybyproductiontype r",
                                                                    "WHERE 1=1", 
                                                                    "AND production_type_id is null",
                                                                    "and last_day = 1", "order by 1"))
      TFarmOut3<-dbFetch(TotfarmsOutb)
      
      
      
      #Scenario 4
      
      
      
      TotfarmsOutb<-dbSendQuery(conCCMooreVxEarly, statement = paste("SELECT iteration, day, last_day, infcU, infcA",
                                                                   "FROM  Results_dailybyproductiontype r",
                                                                   "WHERE 1=1", 
                                                                   "AND production_type_id is null",
                                                                   "and last_day = 1", "order by 1"))
      TFarmOut4<-dbFetch(TotfarmsOutb)
      
      
      
      TFO<-rbind(TFarmOut,TFarmOut2,TFarmOut3,TFarmOut4)
      TFO$Scenario<-c(rep("Late",nrow(TFarmOut)),rep("Early",nrow(TFarmOut2)),rep("Vx Late",nrow(TFarmOut3)),rep("Vx Early",nrow(TFarmOut4)))
      
      
      
   
      #Scenario 1
      
      farmsDepo<-dbSendQuery(conCCMoore21Late, statement = paste("SELECT iteration, day, last_day, descU, descA",
                                                                 "FROM  Results_dailybyproductiontype r",
                                                                 "WHERE 1=1", 
                                                                 "AND production_type_id is null",
                                                                 "and last_day = 1",
                                                                 "order by 1"))
      
      FarmDepo<-dbFetch(farmsDepo)
    
      
      #Scenario 2
      
      farmsDepo<-dbSendQuery(conCCMooreEarly, statement = paste("SELECT iteration, day, last_day, descU, descA",
                                                                "FROM  Results_dailybyproductiontype r",
                                                                "WHERE 1=1", 
                                                                "AND production_type_id is null",
                                                                "and last_day = 1",
                                                                "order by 1"))
      
      FarmDepo2<-dbFetch(farmsDepo)
      
      #Scenario 3
      
      farmsDepo<-dbSendQuery(conCCMooreVX21Late, statement = paste("SELECT iteration, day, last_day, descU, descA",
                                                                 "FROM  Results_dailybyproductiontype r",
                                                                 "WHERE 1=1", 
                                                                 "AND production_type_id is null",
                                                                 "and last_day = 1",
                                                                 "order by 1"))
      
      FarmDepo3<-dbFetch(farmsDepo)
      
      
      #Scenario 4
      
      farmsDepo<-dbSendQuery(conCCMooreVxEarly, statement = paste("SELECT iteration, day, last_day, descU, descA",
                                                                "FROM  Results_dailybyproductiontype r",
                                                                "WHERE 1=1", 
                                                                "AND production_type_id is null",
                                                                "and last_day = 1",
                                                                "order by 1"))
      
      FarmDepo4<-dbFetch(farmsDepo)
      
   
      
      FD<-rbind(FarmDepo,FarmDepo2,FarmDepo3,FarmDepo4)
      FD$Scenario<-c(rep("Late",nrow(FarmDepo)),rep("Early",nrow(FarmDepo2)),rep("Vx Late",nrow(FarmDepo3)),rep("Vx Early",nrow(FarmDepo4)))
      
      
      #Scenario 3
      VaccUnit3<-dbSendQuery(conCCMooreVX21Late, 
                             statement = paste("SELECT iteration, day, last_day, vaccU",
                                               "FROM  Results_dailybyproductiontype r",
                                               "WHERE 1=1", 
                                               "AND production_type_id is null",
                                               "and last_day = 1", "order by 1"))
      
      #creates the data frame from where analyses will be done
      VaccUnit3<-dbFetch(VaccUnit3) 
      
      #Scenario 4
      VaccUnit4<-dbSendQuery(conCCMooreVxEarly, 
                             statement = paste("SELECT iteration, day, last_day, vaccU",
                                               "FROM  Results_dailybyproductiontype r",
                                               "WHERE 1=1", 
                                               "AND production_type_id is null",
                                               "and last_day = 1", "order by 1"))
      
      #creates the data frame from where analyses will be done
      VaccUnit4<-dbFetch(VaccUnit4) 
      
      VaccUnit<-rbind(VaccUnit3,VaccUnit4)
      VaccUnit$Scenario<-c(rep("Vx Late",nrow(VaccUnit3)),rep("Vx Early",nrow(VaccUnit4)))
      
     
          
      #using leaflet
      setwd("~/CEAH/ADSM_Missy/R code basics")
      MapMaster<-read.xlsx("MapMaster.xlsx",1)
      
      pal<-colorFactor(c("navy", "red", "green4", "orange", "purple", "pink","brown", "blue"), domain=c("Destruction","Detection","Exam","Exposure","Infection","TraceFound","TraceInitiated","Vaccination"))
      palInf<-colorFactor(c("navy"), domain=c("Infection"))
      
      
      content <- paste(sep = "<br/>",
                       "ID:", MapMaster$ID,
                       "Production Type:", MapMaster$ProductionType,
                       "Size:",MapMaster$UnitSize)
      
      mapWorld = map("world", fill = TRUE, plot = F)# data of world polygon
      
