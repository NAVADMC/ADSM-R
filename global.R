#========================================================================================#
#========================================================================================#
#### Load packages
#========================================================================================#
#========================================================================================#
library(RSQLite)   # to connect to SQLite from R (this is what I am using)
library(DBI)       # (this is what I am using)
library(ggplot2)   # to do graphs
library(knitr)     # to do rmarkdown documents
library(gridExtra) # to do grid.arrange (multiple graphs in one window)
library(igraph)    # to make network graphs (graph.data.frame)
library(reshape)   # melt function
library(maps)      # to create maps
library(plotly)    # to create dynamic plots in r
library(tidyverse) # to manipulate data
library(doBy)      # summarize by group
library(stringr)   # string manipulation
library(tcltk2)    # GUI functions
library(dplyr)     # data manipulation functions
library(readxl)     # to read in xlsx files

rm(list=ls())      # to clean the environment

#========================================================================================#
#========================================================================================#
#### Helpful functions
#========================================================================================#
#========================================================================================#

#========================================================================================#
#### Get directory paths
#========================================================================================#
# inCaption = "Select all ADSM output folders (containing the .sqlite3 files) you wish to use as input"
# inCaption = "Select all ADSM output folders ending in \"_files\" (containing the .csv files) you wish to use as input"
adsmr_getdirpath = function(inCaption){
  
  #### Helper function that browses for an output directory
  browseInput = function(){
    
    # Convert the tcl variable to an R variable
    nComparisons = as.numeric(tclvalue(nComparisons))
    nameComparisons = tclvalue(nameComparisons)
    
    # Intialize the list or directories
    dirpath = list()
    
    # Destroy the GUI window
    tbtkdestroy()
    
    for(i in 1:nComparisons){
      ## Name the directory
      dirpath[i] = (tcltk::tk_choose.dir(caption = inCaption))
      
      ## Error checking for a selected directory
      if (dirpath[i] == ""){
        tkmessageBox("No directory was selected")
        stop("No directory was selected")
      }
      
    }
    
    splitNames = str_trim(str_split(nameComparisons, ",")[[1]])
    if (length(splitNames) != nComparisons){
      tkmessageBox(message = "Number of scenario names does not equal the number of scenarios chosen, default names used instead")
      splitNames = str_trim(unlist(lapply(current_wd, function(x){
        xSplit = str_split(x, "/")
        xSplitLast = xSplit[[1]][length(xSplit[[1]])]
        return(xSplitLast)
      })))
    }
    names(dirpath) = splitNames
    
    assign("dirpath", dirpath, envir = .GlobalEnv)
    
  }
  
  tbtkdestroy = function(){
    tkdestroy(tb_tkwin)
  }
  
  #========================================================================================#
  #### TclTk interface 
  
  ## Open a generic window
  tb_tkwin = tktoplevel()
  tkraise(tb_tkwin)
  
  ## Set a default number of comparisons
  nComparisons = tclVar("2")
  nameComparisons = tclVar("Scenario1, Scenario2")
  
  ## Create a user input form for the number of scenarios for comparison
  tb_tkwin$env$in_nComparisons = tk2entry(tb_tkwin, width = "25", textvariable = nComparisons)
  
  ## Create text inputs for naming scenarios
  tb_tkwin$env$in_nameComparisons = tk2entry(tb_tkwin, width = 60, textvariable = nameComparisons)
  
  ## Create a button to run the model
  tb_tkwin$env$butRunModel = tk2button(tb_tkwin,
                                       text = "Enter",
                                       command = browseInput)
  
  ## Create a cancel button
  tb_tkwin$env$butCancel = tk2button(tb_tkwin,
                                     text = "Cancel",
                                     command = tbtkdestroy)
  
  ## Set the interactive window title and lay out components
  tktitle(tb_tkwin) = "TB model 2.0"
  tkgrid(tk2label(tb_tkwin, text = "Enter the number of scenarios \n you want to compare:"),
         tb_tkwin$env$in_nComparisons,
         columnspan = 2,
         padx = 5,
         pady = c(5, 15))
  tkgrid(tk2label(tb_tkwin, text = "Enter the name of each scenario, \n separated by commas (\",\"):"),
         tb_tkwin$env$in_nameComparisons,
         columnspan = 2,
         padx = 5,
         pady = c(5, 15))
  tkgrid(tk2label(tb_tkwin, text = "    "))
  tkgrid(tb_tkwin$env$butRunModel, 
         tb_tkwin$env$butCancel,
         columnspan = 2,
         padx = 5,
         pady = c(5, 15))
  
  tkbind(tb_tkwin$env$entVersion, "<Return>", browseInput)
  tkwait.window(tb_tkwin)
}

#========================================================================================#
#### Get file paths
#========================================================================================#
adsmr_getfilepath = function(dbpath = character(0),
                             caption_in = "Choose file"){
  
  if (length(dbpath) == 0){
    # COMEBACK: add error flagging for incorrect filetypes
    # NOTE: All files must be in the same directory (is this a problem?)
    dbpath = choose.files(caption = caption_in)
    if(length(dbpath) == 0) stop("No files chosen!")
  }
  
  return(dbpath)
  
}

#========================================================================================#
#### Get .sqlite3 file paths using chosen directory paths
#========================================================================================#
adsmr_getdbpath = function(dirpath){
  dbpath = lapply(dirpath, function(x){
    return(paste0(x, "/", list.files(x)))
  })
  
  return(dbpath)
  
}

#========================================================================================#
#### Connect to and disconnect from database(s)
#========================================================================================#
#### Connect
adsmr_connect = function(dbpath){
  db <- sapply(dbpath, function(x){dbConnect(RSQLite::SQLite(), x)})
  names(db) = names(dbpath)
  return(db)
}

#### Disconnect
adsmr_disconnect = function(db){
  lapply(db, function(x){dbDisconnect(x)})
}

#========================================================================================#
#### Run prespecified queries ("apply" this to a list of db's)
#========================================================================================#
#### Queries
adsmr_query = function(db){
  
  #### Run queries and fetch the result
  outbreakDuration <- dbSendQuery(db, statement = paste("SELECT iteration, diseaseduration",
                                                        "FROM  Results_dailycontrols r",
                                                        "WHERE 1=1",
                                                        "AND last_day >0","order by 1"))
  outDur <- dbFetch(outbreakDuration)
  dbClearResult(outbreakDuration)
  cat(paste0(rep("-", 60), collapse = ""), "\n")
  cat("outDur query completed... \n")
  
  
  epiCurve <- dbSendQuery(db, statement = paste("SELECT day, Sum(infnU), Sum(detnU)",
                                                "FROM  Results_dailybyproductiontype r",
                                                "WHERE 1=1","AND production_type_id is null",
                                                "and last_day < 1","group by day","order by 1, 2"))
  
  epiCur <- dbFetch(epiCurve)
  dbClearResult(epiCurve)
  cat("epiCurve query completed... \n")
  
  totfarmsOutb <- dbSendQuery(db, statement = paste("SELECT iteration, day, last_day, infcU, infcA",
                                                    "FROM  Results_dailybyproductiontype r",
                                                    "WHERE 1=1", 
                                                    "AND production_type_id is null",
                                                    "and last_day = 1", "order by 1"))
  tFarmOut <- dbFetch(totfarmsOutb)
  dbClearResult(totfarmsOutb)
  cat("totfarmsOutb query completed... \n")
  
  farmsDepo <- dbSendQuery(db, statement = paste("SELECT iteration, day, last_day, descU, descA",
                                                 "FROM  Results_dailybyproductiontype r",
                                                 "WHERE 1=1", 
                                                 "AND production_type_id is null",
                                                 "and last_day = 1",
                                                 "order by 1"))
  farmDepo <- dbFetch(farmsDepo)
  dbClearResult(farmsDepo)
  cat("farmsDepo query completed... \n")
  
  transitionStateDaily <- dbSendQuery(db, statement = paste("SELECT day, tsdUSusc, tsdULat, tsdUSubc, tsdUClin, tsdUNImm, tsdUVImm, tsdUDest", 
                                                            "FROM  Results_dailybyproductiontype r",
                                                            "WHERE production_type_id is null",
                                                            "and iteration = 204"))
  tranStateDaily <- dbFetch(transitionStateDaily)
  dbClearResult(transitionStateDaily)
  cat("transitionStateDaily query completed... \n")
  cat(paste0(rep("-", 60), collapse = ""), "\n")
  
  #### Aggregate into a named list
  fetchlist = list(outDur,
                   epiCur,
                   tFarmOut,
                   farmDepo,
                   tranStateDaily)
  names(fetchlist) = c("outDur",
                       "epiCur",
                       "tFarmOut",
                       "farmDepo",
                       "tranStateDaily")
  
  return(fetchlist)
  
}

#### Apply queries
adsmr_applyquery = function(db, queryfun = adsmr_query, ...){
  querylist = lapply(db, queryfun, ...)
  return(querylist)
}

#========================================================================================#
#### Make table
#========================================================================================#
adsmr_maketable = function(dbquery, whichquery = c("all")){
  ## Get all possible query names
  possquery = names(dbquery[[names(dbquery)[1]]])
  
  ## If whichquery is "all", set to possquery
  if (identical(whichquery, rep("all", length(whichquery)))){whichquery = possquery}
  
  ## Store number of queries requested
  n_whichquery = length(whichquery)
  
  ## Store the names of and number of scenarios
  scenario = names(dbquery)
  n_scenario = length(scenario)
  
  ## Error check: query name must be in list of possible queries
  ec_badqueryname = sapply(whichquery, function(x){
    if (!(x %in% possquery) & x != "all"){stop(paste0("\"", x, "\" is not a possible query. 
                                                      Either change the variable dbquery to include
                                                      a query named \"", x, "\" or delete \"", x, "\" from
                                                      the whichquery variable."))}})
  
  ## Store all tables in a list
  alltable = list()  
  
  ## Loop through requested queries
  for (i in 1:n_whichquery){
    htable = list()
    
    for (j in 1:n_scenario){
      htable_helper = dbquery[[scenario[j]]][[whichquery[i]]]
      htable[[j]] = data.frame("Scenario" = rep(scenario[j], nrow(htable_helper)), htable_helper)
      
    }
    
    names(htable) = scenario
    alltable[[i]] = do.call(rbind.data.frame, htable)
    row.names(alltable[[i]]) = NULL
    colnames(alltable[[i]]) = gsub("\\.", "", colnames(alltable[[i]]))
    
  }
  
  names(alltable) = whichquery
  
  return(alltable)
  
    }

#========================================================================================#
#### Summarize table
#========================================================================================#
adsmr_Q1fun = function(x){
  quantile(x, probs = 0.25)
}

adsmr_Q3fun = function(x){
  quantile(x, probs = 0.75)
}

adsmr_summarizetable = function(dbtable, whichtable = c("all")){
  ## Get all possible table names
  posstable = names(dbtable)
  
  ## If whichtable is "all", set to posstable
  if (identical(whichtable, rep("all", length(whichtable)))){whichtable = posstable}
  
  ## Store number of queries requested
  n_whichtable = length(whichtable)
  
  ## Error check: query name must be in list of possible tables
  ec_badqueryname = sapply(whichtable, function(x){
    if (!(x %in% posstable) & x != "all"){stop(paste0("\"", x, "\" is not a possible table. 
                                                      Either change the variable dbtable to include
                                                      a table named ", x, " or delete ", x, " from
                                                      the whichtable variable."))}})
  ## Store all summaries in a list
  allsummaries = list()  
  
  ## Loop through requested queries
  for (i in 1:n_whichtable){
    hsummaries = list()
    
    ## Get the variables in the table and count them
    # Always subtract the "Scenario" variable (could build in others to subtract out later)
    varintable = names(dbtable[[whichtable[i]]])[-1]  
    n_varintable = length(varintable)
    
    for (j in 1:n_varintable){
      hsummaries[[j]] = summaryBy(as.formula(paste(varintable[j], "~ Scenario")), 
                                  data = dbtable[[whichtable[i]]],
                                  FUN = list(mean, sd, min, adsmr_Q1fun, median, adsmr_Q3fun, max))
      names(hsummaries[[j]]) = c("Scenario", "Mean", "SD", "Min", "Q1", "Median", "Q3", "Max")
      
    }
    
    names(hsummaries) = varintable
    allsummaries[[i]] = hsummaries
    
    
  }
  
  
  names(allsummaries) = whichtable
  return(allsummaries)
  
    }


#========================================================================================#
#### Get .csv file paths using chosen directory paths
#========================================================================================#
adsmr_getcsvpath = function(dirpath){
  # Take advantage of ADSM file naming convention of stroing .csv files in "scenario_name_files" directory for scenario with name "scenario_name"
  # Also, take advantage of the fact that the only folder within that outer directory holds all of the .csv files
  csvpath = lapply(dirpath, function(x){
    csvpath_temp = paste0(x, "_files")
    csvpath_temp2 = list.files(csvpath_temp)
    csvpath = paste0(csvpath_temp, "/", csvpath_temp2)
    return(csvpath)
  })
  
  csvpath_exposures = lapply(csvpath, function(x){
    paste0(x, "/", list.files(x, pattern = "_exposures_\\d*\\d.csv"))
  })
  
  csvpath_events = lapply(csvpath, function(x){
    paste0(x, "/", list.files(x, pattern = "_events_\\d*\\d.csv"))
  })
  
  return(list(exposures = csvpath_exposures,
              events = csvpath_events))
  
}

#========================================================================================#
#### Choose daily iteration based on user input
#========================================================================================#
adsmr_chooseiter = function(csvpath = character(0), 
                            calctype = c("min", "median", "mean", "max")){
  
  if (length(csvpath) == 0){
    adsmr_getdirpath(inCaption = "Select all ADSM output folders (containing the .sqlite3 files) you wish to use as input")
    dbpath = adsmr_getdbpath(dirpath)
    csvlist = adsmr_getcsvpath(dirpath)
  }
  
  calctype = match.arg(calctype, c("min", "median", "mean", "max"))
  
  csvtype = names(csvpath)
  scenarios = names(csvpath[["events"]])
  
  starttime = Sys.time()
  csv_nfarms = lapply(csvpath[["events"]],
                      function(x){
                        sapply(x, function(y){
                          readfile = read.csv(y, header = TRUE)
                          nfarms = length(unique(readfile$ID))
                          return(nfarms)
                        })
                      })
  endtime = Sys.time()
  
  cat(paste0("Reading all events.csv files took ", round(endtime - starttime, 1), " seconds"))
  
  iter_chosen = lapply(csv_nfarms, function(x){
    sample(which(x == do.call(calctype, list(x))), 1)
  })
  
  csv_readin = mapply(function(z, y, x){
    read.csv(csvpath[[x]][[y]][iter_chosen[[y]]])
  },
  paste0(rep(scenarios, each = 2), "_", rep(csvtype, 2)),
  rep(scenarios, each = 2),
  rep(csvtype, 2))
  
  csv_labeled = mapply(function(x, y){
    cbind.data.frame("Scenario" = rep(y, nrow(x)), x)
  }, csv_readin, 
  rep(scenarios, each = 2),
  SIMPLIFY = FALSE)
  
  csv_stacked = list()
  for (i in 1:length(csvtype)){
    csv_stacked[[i]] = bind_rows(csv_labeled[c(grep(paste0("_", csvtype[i]), names(csv_labeled)))])
  }    
  
  names(csv_stacked) = csvtype
  
  return(invisible(csv_stacked))
  
}

#========================================================================================#
#========================================================================================#
#### Generalized analysis
#========================================================================================#
#========================================================================================#

#========================================================================================#
#### Connect to SQLite databases
#========================================================================================#
# #### Can get list of file paths through a file browser
adsmr_getdirpath(inCaption = "Select all ADSM output folders (containing the .sqlite3 files) you wish to use as input")

  #========================================================================================#
  #### Search for mapping data (COMEBACK: need to find out where these come from and internalize if possible)
  DState = read_excel(adsmr_getfilepath(caption_in = "Choose the MapDStatTest.xslx file"))
  MapMaster = read_excel(adsmr_getfilepath(caption_in = "Choose the MapMaster.xslx file"))

dbpath = adsmr_getdbpath(dirpath)

#### Then, you need to connect to each database 
## They will all be stored in a list with names Scenario1,...
dblist = adsmr_connect(dbpath)

#========================================================================================#
#### Pull queries
#========================================================================================#
dbquery = adsmr_applyquery(dblist, adsmr_query)

#========================================================================================#
#### Create one table per query, across scenarios for given named queries
#========================================================================================#
dbtable = adsmr_maketable(dbquery, whichquery = c("all"))

#========================================================================================#
#### Summarize tables
#========================================================================================#
dbsummaries = adsmr_summarizetable(dbtable, whichtable = c("all"))

#========================================================================================#
#### Disconnect from SQLite databases
#========================================================================================#
adsmr_disconnect(dblist)

#========================================================================================#
#### Connect to CSV files
#========================================================================================#
csvlist = adsmr_getcsvpath(dirpath)

daily_dat = adsmr_chooseiter(csvlist, 
                           calctype = "median")

#========================================================================================#
#### Combine daily events data
#========================================================================================#
daily_compare = data.frame(table(select(daily_dat$events, Type, Reason, Scenario)))
colnames(daily_compare) = c("Veterinary_action", "Var2", "Scenario","Freq")

#========================================================================================#
#### Daily igraph data
#========================================================================================#
daily_graphdatprep = select(daily_dat$exposures, Scenario, Source_ID, Recipient_ID)

daily_graphdat = sapply(names(dirpath), function(x){
  graph.data.frame(subset(daily_graphdatprep, Scenario = x))
},
simplify = FALSE)  

#========================================================================================#
#### Prepare mapping data
#========================================================================================#
all_states = map_data("state")
Subset_states = subset(all_states, region %in% c( "texas", "new mexico", "oklahoma", "arkansas", 
                                                   "louisiana", "colorado", "kansas") )


    # #Daily event of a specific iteration
    # de1<-read.csv("ModSim18_CC_Moore_21Late_files/ModSim18_CC_Moore_21Late/daily_events_25.csv")
    # de2<-read.csv("ModSim18_CC_Moore_Early_files/ModSim18_CC_Moore_Early/daily_events_14.csv")
    # de3<-read.csv("ModSim18_CC_MooreVX_21Late_files/ModSim18_CC_MooreVX_21Late/daily_events_4.csv")
    # de4<-read.csv("ModSim18_CC_MooreVx_Early_files/daily_events_10.csv")
    # 
    # 
    # de1$Scenario<-c(rep("Late",nrow(de1)))
    # de2$Scenario<-c(rep("Early",nrow(de2)))
    # de3$Scenario<-c(rep("Vx Late",nrow(de3)))
    # de4$Scenario<-c(rep("Vx Early",nrow(de4)))
    # 
    # Compared_dailyEvents<-rbind(de1,de2,de3,de4)
    # 
    # 
    # 
    # Ty<-table(Compared_dailyEvents$Type,Compared_dailyEvents$Reason,Compared_dailyEvents$Scenario)
    # Ty<-data.frame(Ty) #ggplot works with data frames
    # colnames(Ty)<-c("Veterinary_action", "Var2", "Scenario","Freq")
    # 
    # 
    # 
    # S1farm<-cbind(dex1$Source_ID, dex1$Recipient_ID, dex1$Scenario)
    # colnames(S1farm)<-c("SourceID","RecipientID", "Scenario")
    # S1farm<-as.data.frame(S1farm)
    # 
    # S2farm<-cbind(dex2$Source_ID, dex2$Recipient_ID, dex2$Scenario)
    # colnames(S2farm)<-c("SourceID","RecipientID", "Scenario")
    # S2farm<-as.data.frame(S2farm)
    # 
    # S3farm<-cbind(dex3$Source_ID, dex3$Recipient_ID, dex3$Scenario)
    # colnames(S3farm)<-c("SourceID","RecipientID", "Scenario")
    # S3farm<-as.data.frame(S3farm)
    # 
    # S4farm<-cbind(dex4$Source_ID, dex4$Recipient_ID, dex4$Scenario)
    # colnames(S4farm)<-c("SourceID","RecipientID", "Scenario")
    # S4farm<-as.data.frame(S4farm)
    # 
    # 
    # csvread_subset = select(csvread, Scenario, Source_ID, Recipient_ID)
    # 
    # 
    # AllSceFarm<-rbind(S1farm,S2farm,S3farm,S4farm)
    # 
    # 
    # graph.data.frame(subset(temp, Scenario = "Scenario1"))
    # 
    # net1<-graph.data.frame(S1farm,directed=F)#specified edges of a directed farm
    # net2<-graph.data.frame(S2farm,directed=F)#specified edges of a directed farm
    # net3<-graph.data.frame(S3farm,directed=F)#specified edges of a directed farm
    # net4<-graph.data.frame(S4farm,directed=F)#specified edges of a directed farm
    # 
    # 
    # all_states<-map_data("state")
    # Subset_states <- subset(all_states, region %in% c( "texas", "new mexico", "oklahoma", "arkansas", "louisiana", "colorado", "kansas") )
      # 
      # 
      # conCCMoore21Late <- dbConnect(RSQLite::SQLite(), "~/CEAH/ADSM_Missy/R code basics/ModSim18_CC_Moore_21Late/ModSim18_CC_Moore_21Late.sqlite3")
      # conCCMooreEarly <- dbConnect(RSQLite::SQLite(), "~/CEAH/ADSM_Missy/R code basics/ModSim18_CC_Moore_Early/ModSim18_CC_Moore_Early.sqlite3")
      # conCCMooreVX21Late <- dbConnect(RSQLite::SQLite(), "~/CEAH/ADSM_Missy/R code basics/ModSim18_CC_MooreVX_21Late/ModSim18_CC_MooreVX_21Late.sqlite3")
      # conCCMooreVxEarly <- dbConnect(RSQLite::SQLite(), "~/CEAH/ADSM_Missy/R code basics/ModSim18_CC_MooreVx_Early/ModSim18_CC_MooreVX_Early.sqlite3")
      # 
      # #Scenario 1
      # 
      # 
      # EpiCurve<-dbSendQuery(conCCMoore21Late, statement = paste("SELECT day, Sum(infnU), Sum(detnU)",
      #                                                           "FROM  Results_dailybyproductiontype r",
      #                                                           "WHERE 1=1","AND production_type_id is null",
      #                                                           "and last_day < 1","group by day","order by 1, 2"))
      # EpiCur<-dbFetch(EpiCurve)
      # 
      # 
      # 
      # #Scenario 2
      # 
      # 
      # 
      # EpiCurve<-dbSendQuery(conCCMooreEarly, statement = paste("SELECT day, Sum(infnU), Sum(detnU)",
      #                                                          "FROM  Results_dailybyproductiontype r",
      #                                                          "WHERE 1=1","AND production_type_id is null",
      #                                                          "and last_day < 1","group by day","order by 1, 2"))
      # EpiCur2<-dbFetch(EpiCurve)
      # 
      # #Scenario 3
      # 
      # 
      # EpiCurve<-dbSendQuery(conCCMooreVX21Late, statement = paste("SELECT day, Sum(infnU), Sum(detnU)",
      #                                                           "FROM  Results_dailybyproductiontype r",
      #                                                           "WHERE 1=1","AND production_type_id is null",
      #                                                           "and last_day < 1","group by day","order by 1, 2"))
      # EpiCur3<-dbFetch(EpiCurve)
      # 
      # 
      # #Scenario 4
      # 
      # 
      # 
      # EpiCurve<-dbSendQuery(conCCMooreVxEarly, statement = paste("SELECT day, Sum(infnU), Sum(detnU)",
      #                                                          "FROM  Results_dailybyproductiontype r",
      #                                                          "WHERE 1=1","AND production_type_id is null",
      #                                                          "and last_day < 1","group by day","order by 1, 2"))
      # EpiCur4<-dbFetch(EpiCurve)
      # 
      # 
      # #Creating variable scenario per table
      # EpiCur$Scenario<-c(rep("Late",nrow(EpiCur)))
      # EpiCur2$Scenario<-c(rep("Early",nrow(EpiCur2)))
      # EpiCur3$Scenario<-c(rep("Vx Late",nrow(EpiCur3)))
      # EpiCur4$Scenario<-c(rep("Vx Early",nrow(EpiCur4)))
      # 
      # 
      # #Making Master Tables
      # EPICURVE<-rbind(EpiCur, EpiCur2, EpiCur3, EpiCur4);colnames(EPICURVE)<-c("day", "sumInfnU", "sumdetnU","Scenario")
      # 
      # 
      # 
    # #Scenario 1
    #   
    #   
    #   OutbreakDuration <- dbSendQuery(conCCMoore21Late, statement = paste("SELECT iteration, diseaseduration",
    #                                                                       "FROM  Results_dailycontrols r",
    #                                                                       "WHERE 1=1",
    #                                                                       "AND last_day >0","order by 1"))
    #   OutDur<-dbFetch(OutbreakDuration)
    #   
    #   
    #   #Scenario 2
    #   
    #   OutbreakDuration <- dbSendQuery(conCCMooreEarly, statement = paste("SELECT iteration, diseaseduration",
    #                                                                      "FROM  Results_dailycontrols r",
    #                                                                      "WHERE 1=1",
    #                                                                      "AND last_day >0","order by 1"))
    #   OutDur2<-dbFetch(OutbreakDuration)
    # 
    #   #Scenario 3
    #   
    #   
    #   OutbreakDuration <- dbSendQuery(conCCMooreVX21Late, statement = paste("SELECT iteration, diseaseduration",
    #                                                                       "FROM  Results_dailycontrols r",
    #                                                                       "WHERE 1=1",
    #                                                                       "AND last_day >0","order by 1"))
    #   OutDur3<-dbFetch(OutbreakDuration)
    #   
    #   
    #   #Scenario 4
    #   
    #   OutbreakDuration <- dbSendQuery(conCCMooreVxEarly, statement = paste("SELECT iteration, diseaseduration",
    #                                                                      "FROM  Results_dailycontrols r",
    #                                                                      "WHERE 1=1",
    #                                                                      "AND last_day >0","order by 1"))
    #   OutDur4<-dbFetch(OutbreakDuration)
    #   
    #   
    #   OUTBREAKDUR<-rbind(OutDur,OutDur2,OutDur3,OutDur4)
    #   OUTBREAKDUR$Scenario<-c(rep("Late",nrow(OutDur)),rep("Early",nrow(OutDur2)),rep("Vx Late",nrow(OutDur3)),rep("Vx Early",nrow(OutDur4)))
    #   
    #   
    # 
    #    #Scenario 1
    #   
    #   TotfarmsOutb<-dbSendQuery(conCCMoore21Late, statement = paste("SELECT iteration, day, last_day, infcU, infcA",
    #                                                                 "FROM  Results_dailybyproductiontype r",
    #                                                                 "WHERE 1=1", 
    #                                                                 "AND production_type_id is null",
    #                                                                 "and last_day = 1", "order by 1"))
    #   TFarmOut<-dbFetch(TotfarmsOutb)
    #   
    #   
    #   
    #   #Scenario 2
    #   
    # 
    #   
    #   TotfarmsOutb<-dbSendQuery(conCCMooreEarly, statement = paste("SELECT iteration, day, last_day, infcU, infcA",
    #                                                                "FROM  Results_dailybyproductiontype r",
    #                                                                "WHERE 1=1", 
    #                                                                "AND production_type_id is null",
    #                                                                "and last_day = 1", "order by 1"))
    #   TFarmOut2<-dbFetch(TotfarmsOutb)
    #   
    #   #Scenario 3
    #   
    #   TotfarmsOutb<-dbSendQuery(conCCMooreVX21Late, statement = paste("SELECT iteration, day, last_day, infcU, infcA",
    #                                                                 "FROM  Results_dailybyproductiontype r",
    #                                                                 "WHERE 1=1", 
    #                                                                 "AND production_type_id is null",
    #                                                                 "and last_day = 1", "order by 1"))
    #   TFarmOut3<-dbFetch(TotfarmsOutb)
    #   
    #   
    #   
    #   #Scenario 4
    #   
    #   
    #   
    #   TotfarmsOutb<-dbSendQuery(conCCMooreVxEarly, statement = paste("SELECT iteration, day, last_day, infcU, infcA",
    #                                                                "FROM  Results_dailybyproductiontype r",
    #                                                                "WHERE 1=1", 
    #                                                                "AND production_type_id is null",
    #                                                                "and last_day = 1", "order by 1"))
    #   TFarmOut4<-dbFetch(TotfarmsOutb)
    #   
    #   
    #   
    #   TFO<-rbind(TFarmOut,TFarmOut2,TFarmOut3,TFarmOut4)
    #   TFO$Scenario<-c(rep("Late",nrow(TFarmOut)),rep("Early",nrow(TFarmOut2)),rep("Vx Late",nrow(TFarmOut3)),rep("Vx Early",nrow(TFarmOut4)))
    #   
    #   
    #   
    # 
    #   #Scenario 1
    #   
    #   farmsDepo<-dbSendQuery(conCCMoore21Late, statement = paste("SELECT iteration, day, last_day, descU, descA",
    #                                                              "FROM  Results_dailybyproductiontype r",
    #                                                              "WHERE 1=1", 
    #                                                              "AND production_type_id is null",
    #                                                              "and last_day = 1",
    #                                                              "order by 1"))
    #   
    #   FarmDepo<-dbFetch(farmsDepo)
    # 
    #   
    #   #Scenario 2
    #   
    #   farmsDepo<-dbSendQuery(conCCMooreEarly, statement = paste("SELECT iteration, day, last_day, descU, descA",
    #                                                             "FROM  Results_dailybyproductiontype r",
    #                                                             "WHERE 1=1", 
    #                                                             "AND production_type_id is null",
    #                                                             "and last_day = 1",
    #                                                             "order by 1"))
    #   
    #   FarmDepo2<-dbFetch(farmsDepo)
    #   
    #   #Scenario 3
    #   
    #   farmsDepo<-dbSendQuery(conCCMooreVX21Late, statement = paste("SELECT iteration, day, last_day, descU, descA",
    #                                                              "FROM  Results_dailybyproductiontype r",
    #                                                              "WHERE 1=1", 
    #                                                              "AND production_type_id is null",
    #                                                              "and last_day = 1",
    #                                                              "order by 1"))
    #   
    #   FarmDepo3<-dbFetch(farmsDepo)
    #   
    #   
    #   #Scenario 4
    #   
    #   farmsDepo<-dbSendQuery(conCCMooreVxEarly, statement = paste("SELECT iteration, day, last_day, descU, descA",
    #                                                             "FROM  Results_dailybyproductiontype r",
    #                                                             "WHERE 1=1", 
    #                                                             "AND production_type_id is null",
    #                                                             "and last_day = 1",
    #                                                             "order by 1"))
    #   
    #   FarmDepo4<-dbFetch(farmsDepo)
    #   
    # 
    #   
    #   FD<-rbind(FarmDepo,FarmDepo2,FarmDepo3,FarmDepo4)
    #   FD$Scenario<-c(rep("Late",nrow(FarmDepo)),rep("Early",nrow(FarmDepo2)),rep("Vx Late",nrow(FarmDepo3)),rep("Vx Early",nrow(FarmDepo4)))
    #   
    #   
    #   #Scenario 3
    #   VaccUnit3<-dbSendQuery(conCCMooreVX21Late, 
    #                          statement = paste("SELECT iteration, day, last_day, vaccU",
    #                                            "FROM  Results_dailybyproductiontype r",
    #                                            "WHERE 1=1", 
    #                                            "AND production_type_id is null",
    #                                            "and last_day = 1", "order by 1"))
    #   
    #   #creates the data frame from where analyses will be done
    #   VaccUnit3<-dbFetch(VaccUnit3) 
    #   
    #   #Scenario 4
    #   VaccUnit4<-dbSendQuery(conCCMooreVxEarly, 
    #                          statement = paste("SELECT iteration, day, last_day, vaccU",
    #                                            "FROM  Results_dailybyproductiontype r",
    #                                            "WHERE 1=1", 
    #                                            "AND production_type_id is null",
    #                                            "and last_day = 1", "order by 1"))
    #   
    #   #creates the data frame from where analyses will be done
    #   VaccUnit4<-dbFetch(VaccUnit4) 
    #   
    #   VaccUnit<-rbind(VaccUnit3,VaccUnit4)
    #   VaccUnit$Scenario<-c(rep("Vx Late",nrow(VaccUnit3)),rep("Vx Early",nrow(VaccUnit4)))
    # 
    # 
    #   setwd("~/CEAH/ADSM_Missy/R code basics")
# DState<-read.xlsx("MapDStatTest.xlsx",1)
# MapMaster<-read.xlsx("MapMaster.xlsx",1)
      