#ADSM-Shiny
#========================================================================================#
#========================================================================================#
#### Load packages
#========================================================================================#
#========================================================================================#
rm(list=ls())      # to clean the environment

list.of.packages <- c("ggplot2", "shiny","plotly","xlsx", "jpeg","leaflet","RSQLite","DBI","knitr","gridExtra",
                      "igraph","reshape","maps","rmarkdown","dplyr", "readxl", "tcltk2", "tidyverse", "doBy", "stringr", "tinytex")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)


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
  tktitle(tb_tkwin) = "ADSM R"
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
  cat("tfarmsOut query completed... \n")
  
  farmsDepo <- dbSendQuery(db, statement = paste("SELECT iteration, day, last_day, descU, descA",
                                                 "FROM  Results_dailybyproductiontype r",
                                                 "WHERE 1=1", 
                                                 "AND production_type_id is null",
                                                 "and last_day = 1",
                                                 "order by 1"))
  farmDepo <- dbFetch(farmsDepo)
  dbClearResult(farmsDepo)
  cat("farmDepo query completed... \n")
  
  transitionStateDaily <- dbSendQuery(db, statement = paste("SELECT day, tsdUSusc, tsdULat, tsdUSubc, tsdUClin, tsdUNImm, tsdUVImm, tsdUDest", 
                                                            "FROM  Results_dailybyproductiontype r",
                                                            "WHERE production_type_id is null",
                                                            "and iteration = 204"))
  
  tranStateDaily <- dbFetch(transitionStateDaily)
  dbClearResult(transitionStateDaily)
  cat("tranStateDaily query completed... \n")
  
  vaccinationUnit <- dbSendQuery(db, statement = paste("SELECT iteration, day, last_day, vaccU",
                                                "FROM  Results_dailybyproductiontype r",
                                                "WHERE 1=1",
                                                "AND production_type_id is null",
                                                "and last_day = 1", "order by 1"))
  vaccUnit <- dbFetch(vaccinationUnit)
  dbClearResult(vaccinationUnit)
  cat("vaccUnit query completed... \n")
  
  cat(paste0(rep("-", 60), collapse = ""), "\n")
  
  #### Aggregate into a named list
  fetchlist = list(outDur,
                   epiCur,
                   tFarmOut,
                   farmDepo,
                   tranStateDaily,
                   vaccUnit)
  names(fetchlist) = c("outDur",
                       "epiCur",
                       "tFarmOut",
                       "farmDepo",
                       "tranStateDaily",
                       "vaccUnit")
  
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
#adsmr_getcsvpath = function(dirpath){
#  # Take advantage of ADSM file naming convention of stroing .csv files in "scenario_name_files" directory for scenario with name "scenario_name"
#  # Also, take advantage of the fact that the only folder within that outer directory holds all of the .csv files
#  csvpath = lapply(dirpath, function(x){
#    csvpath_temp = paste0(x, "_files")
#    csvpath_temp2 = list.files(csvpath_temp)
#    csvpath = paste0(csvpath_temp, "/", csvpath_temp2)
#    return(csvpath)
#  })
#  
#  csvpath_exposures = lapply(csvpath, function(x){
#    paste0(x, "/", list.files(x, pattern = "_exposures_\\d*\\d.csv"))
#  })
#  
#  csvpath_events = lapply(csvpath, function(x){
#    paste0(x, "/", list.files(x, pattern = "_events_\\d*\\d.csv"))
#  })
#  
#  return(list(exposures = csvpath_exposures,
#              events = csvpath_events))
#  
#}

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
    sample(which(x >= do.call(calctype, list(x)) - 2.5 & x <= do.call(calctype, list(x)) + 2.5), 1)
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
    if (identical(c("Lat", "Lon") %in% names(csv_stacked[[i]]), c(TRUE, TRUE))) {
      dplyr::rename(csv_stacked[[i]], S_Lon = Lon, S_Lat = Lat)}
  }  
  
  
  names(csv_stacked) = csvtype
  
  return(invisible(csv_stacked))
  
}

#========================================================================================#
#### Rmd file
#========================================================================================#
rmdfilestring = "
---
title: 'Report.Rmd'
author: 'Karla Moreno'
date: 'February 7, 2018'
output:
html_document: default
word_document: default
pdf_document: default
params:
scenario_variable: NA
Week: NA
Day: NA
always_allow_html: yes
---
  
  # Dynamic Report
  
  # Creating a document that provides comparison of disease modeling scenarios ran in ADSM. 
  
  We created an R Markdown document that would guide the R user to explore the outputs from ADSM by modifying or expanding this basic code. 

# Background about the simulation modeling of infectious diseases

Describe the population, location, and disease studied, including the baseline mechanisms of transmission and control strategies that are included in each of the scenarios.

## List of scenarios 
In our example below, we will visualize outputs of two scenarios. Scenarios are simulations of disease spread where factors are modified from the baseline scenario in order to better understand its effect on the outbreak characteristics. 


# Outbreak Characteristics

Add information or description of corresponding outputs

```{r, echo=F}

#Outbreak Characteristics

EPI<-ggplot(dbtable$epiCur[(dbtable$epiCur$Scenario) %in% params$scenario_variable,],aes(x=day, y=SuminfnU, group=Scenario, colour=Scenario))+
  geom_line(size=1)+
  labs(title='Epidemic Curve',x='Time (days)', y = 'New Infected Units')

EPI



myplot4 <- qplot(Scenario, diseaseDuration, geom=c('boxplot'), data=dbtable$outDur[(dbtable$outDur$Scenario) %in% params$scenario_variable,],
                 fill=Scenario, main='Outbreak duration',
                 xlab='', ylab='Duration (days)')
myplot4 



myplot5 <- qplot(Scenario, infcU, geom=c('boxplot'), data=dbtable$tFarmOut[(dbtable$tFarmOut$Scenario) %in% params$scenario_variable,],
                 fill=Scenario, main='Farms in outbreak',
                 xlab='', ylab='Units')
myplot5


myplot6 <- qplot(Scenario, descU, geom=c('boxplot'), data=dbtable$farmDepo[(dbtable$farmDepo$Scenario) %in% params$scenario_variable,],
                 fill=Scenario, main='Farms depopulated',
                 xlab='', ylab='Units')
myplot6  


myplot6.5 <- qplot(Scenario, vaccU, geom=c('boxplot'), data=dbtable$vaccUnit[(dbtable$vaccUnit$Scenario) %in% params$scenario_variable,],
                   fill=Scenario, main='Units Vaccinated',
                   xlab='', ylab='Farms vaccinated')

myplot6.5
```



# Epidemic Spread

Add information or description of corresponding outputs

```{r, echo=F}
#Epidemic Spread
#Maps with leaflet wont be visible unless is a html file

#   # # Reactive expression for the data subsetted to what the user selected
filteredDataW <- 
  MapMaster[MapMaster$Type=='Infection'& MapMaster$Week ==(params$Week),]

dfW = data.frame(filteredDataW)
m=leaflet(data = mapWorld) %>% addTiles() %>%
  addPolygons(weight = 1, fillColor = 'white', color='grey') %>%
  fitBounds(~min(dfW$Lon), ~min(dfW$Lat), ~max(dfW$Lon), ~max(dfW$Lat))
m %>% addCircleMarkers(~Lon, ~Lat, popup=content, radius= ~ifelse(UnitSize,6,10), color = ~palInf(Type), stroke=F, fillOpacity = 0.1, data=dfW)


myplot1 <- qplot(Type, geom=c('bar'), data=MapMaster[(MapMaster$Week)==(params$Week),],
                 fill=Type, main='Veterinary Action')+
  theme_void()

myplot1

# # Reactive expression for the data subsetted to what the user selected
filteredDataD <- MapMaster[MapMaster$Type=='Infection' & MapMaster$Day ==(params$Day),]

df = data.frame(filteredDataD)
m=leaflet(data = mapWorld) %>% addTiles() %>%
  addPolygons(weight = 1, fillColor = 'white', color='grey') %>%
  fitBounds(~min(df$Lon), ~min(df$Lat), ~max(df$Lon), ~max(df$Lat))
m %>% addCircleMarkers(~Lon, ~Lat, popup=content, radius= ~ifelse(UnitSize,6,10), color = ~palInf(Type), stroke=F, fillOpacity = 0.1, data=df)

```



# Herd Level Analyses

Add information or description of corresponding outputs

```{r, echo=F}

#Herd Level Analyses

myplot7 <- ggplot(daily_compare[(daily_compare$Scenario)%in% params$scenario_variable,], aes(x='', y=Freq, fill=Veterinary_action))+
  geom_bar(width = 1, stat = 'identity')+
  facet_grid(facets=.~Scenario)+
  xlab('') 
myplot7
```

```{r, echo=F}

#Herd Level Analyses Transmission type

myplot7.5 <-ggplot(daily_comparE[(daily_comparE$Scenario)%in% params$scenario_variable,], aes(x='', y=Freq, fill=Transmission))+
  geom_bar(width = 1, stat = 'identity')+
  facet_grid(facets=.~Scenario)+
  xlab('') 

myplot7.5
```


## Network visualization

```{r, echo=F}


par(mfrow = c(1, length(params$scenario_variable)))

for (i in 1:length(params$scenario_variable)){
  
  net2<-graph.data.frame(daily_graphdatprep[daily_graphdatprep$Scenario == params$scenario_variable[i],],directed=F)#specified edges of a directed farm 'Early'
  
  
  
  plot(net2, layout=layout.fruchterman.reingold, vertex.size=8,edge.arrow.size=0.5, vertex.label=NA, main=params$scenario_variable[i])
  
}

par(mfrow = c(1, 1))

```" 

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
  # DState = read_excel(adsmr_getfilepath(caption_in = "Choose the MapDStatTest.xslx file"))
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

daily_dat = suppressWarnings(adsmr_chooseiter(csvlist, 
                                              calctype = "median"))

#========================================================================================#
#### Combine daily events data
#========================================================================================#
daily_compare = data.frame(table(select(daily_dat$events, Type, Reason, Scenario)))
colnames(daily_compare) = c("Veterinary_action", "Var2", "Scenario","Freq")

#========================================================================================#
#### Combine daily exposures data
#========================================================================================#

daily_comparE = data.frame(table(select(daily_dat$exposures, Reason, Scenario)))
colnames(daily_comparE) = c("Transmission","Scenario","Freq")
daily_comparE$Transmission[daily_comparE$Transmission==""] <- NA 
daily_comparE<-na.omit(daily_comparE)

#========================================================================================#
#### Daily igraph data
#========================================================================================#
daily_graphdatprep = select(daily_dat$exposures, Source_ID, Recipient_ID, Scenario)

#========================================================================================#
#### Map data
#========================================================================================#
pal<-colorFactor(c("navy", "red", "green4", "orange", "purple", "pink","brown", "blue"), 
                 domain=c("Destruction","Detection","Exam","Exposure","Infection","TraceFound","TraceInitiated","Vaccination"))
palInf<-colorFactor(c("navy"), domain=c("Infection"))
         
content <- paste(sep = "<br/>",
                 "ID:", MapMaster$ID,
                 "Production Type:", MapMaster$ProductionType,
                 "Size:",MapMaster$UnitSize)
      
mapWorld = maps::map("world", fill = TRUE, plot = F)# data of world polygon

         

      
