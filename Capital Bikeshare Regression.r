##--------------------------------------------------------------------------------------------------------
## Detecting Routes whose Average Time to Travel Changes Using Capital Bikeshare Data Set
## STAT 27850/30850
## Submitted on Thursday February 6th, 2020
## Jun Choo, H.I. Park, Young-Joo Yun
##--------------------------------------------------------------------------------------------------------
## Description of the code 
## Total time taken to run the code: 8.61366 mins
##--------------------------------------------------------------------------------------------------------
## This script is composed of five sections:
## Section (1) preprocesses the data and produces (i) a list of data tables for each route 
##                                                (ii) an aggregate data table containing all observations
## Section (2) runs diagnostics to validate our choice of linear model 
## Section (3) runs our model which is composed of a linear regression followed by a permutation test, 
##             for each route 
## Section (4) implements Benjamini Hochberg(BH) procedure as our multiple testing procedure 
## Section (5) analyzes the results 
## Note: please ensure all of the following packages are installed and that the files listed
##       are in the same directory as this script. 
## (Packages)
## - "data.table"
## - "ggplot2"
## - "geosphere"
## (Data files) 
## - "bikedata.RData"
## - "2010-capitalbikeshare-tripdata.csv"
## - "2011-capitalbikeshare-tripdata.csv"
## - "data.txt"
## We define unique route as a set {start_station, end_station}, i.e. the order does not matter, and this 
## choice is justified in the report.
##--------------------------------------------------------------------------------------------------------
## Section (1) preprocesses the data 
##--------------------------------------------------------------------------------------------------------
## Uses a data table named dtBike to store the data we have been provided by the professor
## Adds additional data that we have gathered
## This section produces an aggregated data table (dtAggRoutes) and a list of data tables for each route
## (dtListRoutes), which contain the following columns: 
## - Columns based on data provided to us
##       - Duration 
##       - Start_date 
##       - End_date 
##       - Start_station_number
##       - Start_station 
##       - End_station_number 
##       - End_station 
##       - Member type (categorical with 2 levels)
##       - Days_since_Jan1_2010 (ordinal categorical / continuous)
## - Columns based on data we have gatherd
##       - Weekday (categorical with 2 levels) 
##       - Rush_hour (categorical with 2 levels) 
##       - Distance (continuous) 
##       - Season (categorical with 6 levels)
## We ensured that all factor covariates are stored as such by using the command as.factor()
##--------------------------------------------------------------------------------------------------------
## (1)(i) Imports libraries and loads data 
##--------------------------------------------------------------------------------------------------------
startTime <- Sys.time()
library(data.table)
library(ggplot2)
library("geosphere") # for computing the distance between two bike stations 
storedData = load("bikedata.RData")
dataCoord = as.matrix(read.table("data.txt")) # manually found coordinates of bike stations
dt2010 = fread("2010-capitalbikeshare-tripdata.csv")
dt2011 = fread("2011-capitalbikeshare-tripdata.csv")
##--------------------------------------------------------------------------------------------------------
## (1)(ii) Creates a data table called dtBike, which contains the default data we are given  
##--------------------------------------------------------------------------------------------------------
dtBike = rbindlist(list(dt2010, dt2011))
# renames columns for convenience 
names(dtBike)[2] = "Start_date"; names(dtBike)[3] = "End_date"
names(dtBike)[4] = "Start_station_number"; names(dtBike)[5] = "Start_station"
names(dtBike)[6] = "End_station_number"; names(dtBike)[7] = "End_station"
names(dtBike)[8] = "Bike_number"; names(dtBike)[9] = "Member_type"
dtBike[, "Member_type" := as.factor(Member_type)]
##--------------------------------------------------------------------------------------------------------
## (1)(iii) Adds additional data to the dtBike
##--------------------------------------------------------------------------------------------------------
dtBike[, "Days_since_Jan1_2010" := days_since_Jan1_2010] # adds "Days_since_Jan1_2010"
fun_weekday = function(numDays) {
  # returns 1 = weekday, 0 = weekend
  if ((numDays %% 7 == 1) | (numDays %% 7 == 2)) { 
    # 1 and 2 come from the fact that Jan1 is a Friday
    to_return = 0
  } else {
    to_return = 1
  }
  return(to_return)
}
weekday = sapply(days_since_Jan1_2010, fun_weekday)
dtBike[, "Weekday" := as.factor(weekday)] # adds "Weekday"
fun_rush = function(date) {
  # input is each row of data 'starttime'
  # returns 1 = rush hour, 0 = not rush hours
  # rush hours in Washington D.C. are Mon-Fri 6 - 9:30am and 3:30-6:30pm 
  # weekend/weekday information is incorporated separately using the vector weekday created above
  if (((date[4] >= 6) & (date[4] < 9)) | 
      ((date[4] == 9) & (date[5] <= 30)) | 
      ((date[4] == 15) & (date[5] >= 30)) |
      ((date[4] >= 16) & (date[4] < 18)) |
      ((date[4] == 18) & (date[5] <= 30))) {
    to_return = 1
  } else {
    to_return = 0
    } 
  return(to_return)
}
rush_hour = apply(starttime, 1, fun_rush) * weekday # Boolean multiplication to filter out weekdays
dtBike[, "Rush_hour" := as.factor(rush_hour)] # adds "Rush_hour" 
# first, processes the coordinates information 
whichStation = as.integer(stations[, 1]) # station number 
coord = dataCoord[,2]; coord = sapply(coord, function(x){strsplit(x, ", ")}) # coordinates
fun_l = function(lString, longOrLat) {
  # converts "deg, mins, sec" form of longitude or latitude to "deg" form 
  # 1 degree = 1 hour
  # decimal Degrees = degrees + (mins/60) + (seconds/3600)
  # north and east are positive; south and west are negative 
  lListPre = strsplit(lString, " ") # needs to parse "lString", which is a string 
  if (longOrLat == "long") {
    lList = strsplit(lListPre[[1]], " ")
  } else {
    lList = strsplit(lListPre[[2]], " ")
  }
  d = as.integer(lList[[1]]); m = as.integer(lList[[2]])
  lS = strsplit(lList[[3]], "")
  s = as.integer(paste(lS[[1]][1], lS[[1]][2], sep = ""))
  sign = 1; 
  if ((lS[[1]][3] == "S") |  (lS[[1]][3] == "W")){ # sign is different based on direction
    sign = -1 * sign 
  }
  to_return = sign * (d + (m / 60) + (s / 3600))
  return(to_return)
}
long = sapply(coord, function(x){fun_l(x, "long")}) 
lat = sapply(coord, function(x){fun_l(x, "lat")})
coordDeg = cbind(long, lat)
# adds coordinates to the data table
dtBike[, Start_coord := lapply(Start_station_number, function(x){coordDeg[which(x == whichStation),]})]
dtBike[, End_coord := lapply(End_station_number, function(x){coordDeg[which(x == whichStation),]})]
dtBike[, Distance := unlist(Map(distHaversine, Start_coord, End_coord))] # adds distance
# the function "distHaversine" takes a long time
fun_season= function(x) {
  # assigns categorical value depending which sub-season the given date falls under
  weaList = strsplit(x, " ")
  date = (weaList[[1]])[1]; dateList = strsplit(date, "-")
  year = (dateList[[1]])[1]; month = as.integer((dateList[[1]])[2]); day = (dateList[[1]])[3]
  if ((month == 12) | (month == 1)) {
    to_return = 1
  } else if ((month == 2) | (month == 3)) {
    to_return = 2
  } else if ((month == 4) | (month == 5)) {
    to_return = 3
  } else if ((month == 6) | (month == 7)) {
    to_return = 4
  } else if ((month == 8) | (month == 9)) {
    to_return = 5
  } else {
    to_return = 6
  }
  return(to_return)
}
dtBike[, Season := as.factor(unlist(lapply(Start_date, fun_season)))] # adds season 
# deletes columns that are not used anymore 
dtBike[, Start_coord := NULL] 
dtBike[, End_coord := NULL]
dtBike[, Bike_number := NULL]
##--------------------------------------------------------------------------------------------------------
## (1)(iv) Creates a list of data tables, each for a unique route 
## The list of data tables corresponding to unique routes are constructed in the following way 
## Step 1. Creates a new data table with the start station and end station of dtBike (the original 
##         data table) swapped
## Step 2. Combines the orignal data table with this new data table 
## Step 3. Splits the data table by the start station, creating a list of data tables
## Step 4. Further splits each data table by the end station, so each data table contains all observations
##         for the route it corresponds to. However, because of Step 2, we have a duplicate for each data
##         table. Each route is named as A_B where and B are keys (integer) that are mapped to a station 
##         using a dictionary.
## Step 5. Deletes either A_B or B_A in our list of data tables. We also delete data tables corresponding
##         to A_A (start station = end station)
## Step 6. We have a list of data frames correponding to unique routes
## There are 6780 unique routes, inlucding those of the form A_A
## Excluding those of the form A_A, there are 6637 unique routes 
##--------------------------------------------------------------------------------------------------------
# creates two dictionaries that map station IDs with natural numbers 1-144
allVec = unique(dtBike[, list(Start_station_number)])
dictNtoS = list(); dictStoN = list()
for(i in 1:nrow(allVec)) {
  dictNtoS[[toString(i)]] = allVec[i, Start_station_number]
  dictStoN[[toString(allVec[i, Start_station_number])]] = i
}
# creates another data table with start and end stations swapped
dtBikeS = copy(dtBike)
dtBikeS = dtBikeS["Start_station_number" != "End_station_number"] 
setcolorder(dtBikeS, c("Duration", "Start_date", "End_date", "End_station_number", 
                       "End_station", "Start_station_number", "Start_station", 
                       "Member_type", "Days_since_Jan1_2010", "Weekday", "Rush_hour", "Distance", "Season"))
names(dtBikeS)[4] = "Start_station_number"; names(dtBikeS)[5] = "Start_station"
names(dtBikeS)[6] = "End_station_number"; names(dtBikeS)[7] = "End_station"
# combines both data tables
dtBikeM = rbindlist(list(dtBike, dtBikeS)) 
# returns a list of data tables split by the start station number
dtList = split(dtBikeM, by = "Start_station_number")
# returns a list of data tables that are further split by the end station number
# each object (data table) in the list is named as startkey_endkey 
# where each key is mapped to a unique station number by the dictionaries created above
dtList2 = list() 
for (i in 1:144) {
  tmpList = split(dtList[[i]], by = "End_station_number")
  for (el in tmpList) {
    s = el[1, Start_station_number]
    e = el[1, End_station_number]
    com = paste(toString(dictStoN[[toString(s)]]), "_", toString(dictStoN[[toString(e)]]),sep = "")
    dtList2[[com]] = el 
  } 
} 
# deletes duplicates 
# this is done by converting startkey_endkey to {startkey, endkey} (string to integer)
# and then by sorting each pair {startkey, endkey} so that given a list of such pairs, 
# the duplicate can be removed by using the function "unique()"
nR = names(dtList2)
nRSplit = sapply(nR, function(x){strsplit(x, "_")}) # parses the string
nRSplitNum = lapply(nRSplit, as.numeric) # converts string to integer
nRSplitNumSorted = lapply(nRSplitNum, sort) # sorts each pair 
nRMat = matrix(unlist(nRSplitNumSorted), ncol = 2, byrow = TRUE)
listNoDup = !duplicated(nRMat)
indUniq = which(listNoDup) # gets indices (in the list) of the data tables that are not remove
dtListFinal = dtList2[indUniq] # gets final list of data tables, each for a unique route (there are 6780)
# Removes routes of the form A_A, by a similar process as above
nR = names(dtListFinal)
nRSplit = sapply(nR, function(x){strsplit(x, "_")}) # parses the string
nRSplitNum = lapply(nRSplit, as.numeric) # converts string to integer
boolA_A = lapply(nRSplitNum, function(x){x[1] != x[2]})
dtListFinalFinal = dtListFinal[unlist(boolA_A)] # there are 6637 elements 
dtListRoutes = copy(dtListFinalFinal) # the name of the final list of data tables is "dtListRoutes"
##--------------------------------------------------------------------------------------------------------
## (1)(v) Creates an aggregate data table called "dtAggRoutes", which is used for running diagnostics later 
##--------------------------------------------------------------------------------------------------------
dtAggRoutes = rbindlist(dtListFinalFinal) 
rm(storedData) # removes data we don't need anymore
##--------------------------------------------------------------------------------------------------------
## (2) Diagnostics for our choice of model 
##--------------------------------------------------------------------------------------------------------
## Since the number of obseravtions is large, we sampled 10000 observations randomly from the entire data
## (which is done by sampling the rows of the aggregate data table dtAggRoutes)
## Runs linear regression with our linear model, and create residual plots
## Our linear model, which we specified before lookings at the data, is the following: 
##                 Duration ~ Distance + Weekday + Member_type + Rush_hour + Season + 
##                            Member_type:Rush_hour + Distance:Season + Distance:Member_type
## Running this section saves the diagnostics plots in the current directory 
##--------------------------------------------------------------------------------------------------------
## (2)(i) Runs the model
##--------------------------------------------------------------------------------------------------------
noInteractAgg = "Distance + Weekday + Member_type + Rush_hour + Season"
InteractAgg = "Member_type:Rush_hour + Distance:Season + Distance:Member_type"
finModelAgg = paste("Duration ~", noInteractAgg, "+", InteractAgg)
# runs linear regression
sampAgg = dtAggRoutes[sample(1:nrow(dtAggRoutes), 10000)]
modelAgg = lm(finModelAgg, data = sampAgg)
resVec = residuals(modelAgg)
fittedVec = fitted(modelAgg)
disVec = sampAgg[, Distance]
weekVec = sampAgg[, Weekday]
memVec = sampAgg[, Member_type]
seasVec = sampAgg[, Season]
horVec = rep(0, length(resVec))
##--------------------------------------------------------------------------------------------------------
## (2)(ii) Generates plots
##--------------------------------------------------------------------------------------------------------# generates plots
plotFitted = ggplot(NULL, aes(x = fittedVec)) + 
  geom_point(aes(y = resVec), color = "#70A1D7", fill = "#70A1D7", size = 0.2) + 
  geom_line(aes(y = horVec), color = "#F47C7C") + 
  labs(title = "Residual Plot (Fitted Duration)", x = "Fitted Duration", y = "Residuals") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 11, family = "serif")) + 
  theme(axis.line = element_line(colour = "black")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
plotDis = ggplot(NULL, aes(x = disVec)) + 
  geom_point(aes(y = resVec), color = "#70A1D7", fill = "#70A1D7", size = 0.2) + 
  geom_line(aes(y = horVec), color = "#F47C7C") + 
  labs(title = "Residual Plot (Distance)", x = "Distance", y = "Residuals") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 11, family = "serif")) + 
  theme(axis.line = element_line(colour = "black")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlim(0, 5000)
plotWeek = ggplot(NULL, aes(x = weekVec)) + 
  geom_point(aes(y = resVec), color = "#70A1D7", fill = "#70A1D7", size = 0.2) + 
  geom_line(aes(y = horVec), color = "#F47C7C") + 
  labs(title = "Residual Plot (Weekday)", x = "Weekday", y = "Residuals") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 11, family = "serif")) + 
  theme(axis.line = element_line(colour = "black")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
plotMem = ggplot(NULL, aes(x = memVec)) + 
  geom_point(aes(y = resVec), color = "#70A1D7", fill = "#70A1D7", size = 0.2) + 
  geom_line(aes(y = horVec), color = "#F47C7C") + 
  labs(title = "Residual Plot (Member_type)", x = "Member_type", y = "Residuals") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 11, family = "serif")) + 
  theme(axis.line = element_line(colour = "black")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
plotSeas = ggplot(NULL, aes(x = seasVec)) + 
  geom_point(aes(y = resVec), color = "#70A1D7", fill = "#70A1D7", size = 0.2) + 
  geom_line(aes(y = horVec), color = "#F47C7C") + 
  labs(title = "Residual Plot (Season)", x = "Season", y = "Residuals") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 11, family = "serif")) + 
  theme(axis.line = element_line(colour = "black")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
png(filename = "plotFitted.png")
plotFitted
dev.off()
png(filename = "plotDis.png")
plotDis
dev.off()
png(filename = "plotWeek.png")
plotWeek
dev.off()
png(filename = "plotMem.png")
plotMem
dev.off()
png(filename = "plotSeas.png")
plotSeas
dev.off()
##--------------------------------------------------------------------------------------------------------
## (3) Runs our model 
##--------------------------------------------------------------------------------------------------------
## The pipeline of our model is the following: 
## 1. Runs linear regression on each data table corresponding to a unique route 
##    Note that the model we are using is the same as the model we just used in the previous section
##    However, since the distance is the same for each route, it is absorbsed in the intercept term
##    The interaction terms that contain distance is therefore not used either 
##    The model is the following: 
##             Duration ~ Weekday + Member_type + Rush_hour + Season + Member_type:Rush_hour
##    This model has 9 degrees of freedom. 
##    After running the model, the residuals are added as a column to each data table
## 2. Runs grouped permutation test where we permute the residuals with Days_since_Jan1_2010
##    By grouped, we mean that the residuals corresponding to the same day stay as a single unit
##    throughout the permutation. This is similar to Scheme B of Assignment 1 where we keep X 
##    the same and permute Y. This preserves the correlation structure, and we do this in order to 
##    account for the confounding effects of the observations that fall under the same day. 
## The model returns a list of p-values 
## However, before, implementing our model, we first further narrow down our list of data tables 
## by eliminating those that do not satisfy the necesary conditions, which are 
## - The number of data points should be at least 9, which is the degrees of freedom
## - There should be at least one observation for each level of every covariate 
## The number of routes that contain observations satisfying these two conditions is 2638
##--------------------------------------------------------------------------------------------------------
## (3)(i) Removes data tables that do not satisfy the two conditions
##--------------------------------------------------------------------------------------------------------
df = 9 # this is the degree of freedom of our model 
dtListRoutes = dtListRoutes[unlist(lapply(dtListRoutes, function(x) {nrow(x) >= df}))] 
dtListRoutes = lapply(dtListRoutes, droplevels)
fun_season_fac = function(x) {
  numL = nlevels(x[, Season])
  if (numL < 6) {to_return = 1} else {to_return = 0}
  return(to_return)
}
fun_memb_fac = function(x) {
  numL = nlevels(x[, Member_type])
  if (numL != 2) {to_return = 1} else {to_return = 0}
  return(to_return)
}
fun_weekday_fac = function(x) {
  numL = nlevels(x[, Weekday])
  if (numL != 2) {to_return = 1} else {to_return = 0}
  return(to_return)
}
fun_rush_fac = function(x) {
  numL = nlevels(x[, Rush_hour])
  if (numL != 2) {to_return = 1} else {to_return = 0}
  return(to_return)
}
whichLess = lapply(dtListRoutes, fun_season_fac); whichLess = unlist(whichLess)
dtListRoutes = dtListRoutes[which(whichLess == 0)] 
whichLess = lapply(dtListRoutes, fun_memb_fac); whichLess = unlist(whichLess)
dtListRoutes = dtListRoutes[which(whichLess == 0)] 
whichLess = lapply(dtListRoutes, fun_weekday_fac); whichLess = unlist(whichLess)
dtListRoutes = dtListRoutes[which(whichLess == 0)] 
whichLess = lapply(dtListRoutes, fun_rush_fac); whichLess = unlist(whichLess)
dtListRoutes = dtListRoutes[which(whichLess == 0)] 
##--------------------------------------------------------------------------------------------------------
## (3)(ii) Runs linear regression on each data table, and adds the residuals to each data table 
##--------------------------------------------------------------------------------------------------------
fun_lr = function(finModel, listofDt) {
  # "finModel" is the string that specifies the model 
  # "listofDt" is the list of data tables 
  for(i in 1:length(listofDt)) {
    dtCur = listofDt[[i]]
    model = lm(finModel, data = dtCur)
    toAddResid = residuals(model)
    (dtCur)[, Resid := toAddResid]
  }
}
noInteract = "Weekday + Member_type + Rush_hour + Season"
Interact = "Member_type:Rush_hour"
finModelIn = paste("Duration ~", noInteract, "+", Interact)
dtListIn = dtListRoutes
# runs the function 
fun_lr(finModelIn, dtListIn)
##--------------------------------------------------------------------------------------------------------
## (3)(ii) Runs permutation test for each data table 
##--------------------------------------------------------------------------------------------------------
fun_perm = function(x, numPerm) {
  dtCur = x # data table corresponding to route i
  X = (dtCur[, list(Resid)])[[1]] # this does not change
  Y = (dtCur[, list(Days_since_Jan1_2010)])[[1]] # this is permuted
  uniqueY = unique(Y) 
  numUniqueDaysX = length(uniqueY) # number of groups
  testStats = sum(cor(X, Y) ** 2) # our test statistic 
  testSampVec = rep(0, numPerm) # for storing test statistics 
  for(j in 1:numPerm) {
    samp = sample(1:numUniqueDaysX, replace = FALSE) # permutes the indices of Y 
    YSamp = uniqueY[samp] # permutes Y
    dictOtoN = list()
    for(k in 1:numUniqueDaysX) {
      dictOtoN[[uniqueY[k]]] = YSamp[k] 
    }
    YSampEx = sapply(Y, function(z){dictOtoN[[z]]})
    # maintains the group structure by replacing each value of the Y 
    # with the corresponding value of sampled Y 
    # in the same position of the original vector Y
    #fun_same_pos = Vectorize(function(z) {YSampEx[which(Y == unique(Y)[z])] = YSamp[z]})
    #fun_same_pos(1:numUniqueDaysX)
    testSampVec[j] = sum(cor(X, YSampEx) ** 2)
  }
  # computes the p-value 
  numer = 1 + length(testSampVec[testSampVec >= testStats])
  denom = 1 + numPerm 
  pVal= numer / denom # p-value
  return(pVal)
}
fun_perm_h = function(x){fun_perm(x, 100)}
pValVec = lapply(dtListRoutes, fun_perm_h)
pValVec = unlist(pValVec)
##--------------------------------------------------------------------------------------------------------
## (4) Runs BH to correct for multiple testing procedure 
##--------------------------------------------------------------------------------------------------------
## Around 90 routes are rejected
## Saves a text file of the addresses of the start point and end point of each route 
##--------------------------------------------------------------------------------------------------------
fun_bh = function(p, alpha) {
  # returns indices of p-values to reject
  n = length(p); kMax = 0
  for (i in 1:n) {
    thres = alpha * (i / n); tmp = sum(p < thres)
    if (tmp >= i) {kMax = tmp}
  }
  thresFin = alpha * kMax / n
  to_return = which(p <= thresFin)
  return(to_return)
}
pValVecIn = pValVec
alphaIn = 0.3 # the rejection threshold alpha is set at level 0.3
toReject = fun_bh(pValVecIn, alphaIn) # indices of dtListRoutes corresponding to the routes to be rejected
getRoutes = function(x){
  startName = ((x[, "Start_station"])[[1]])[1]
  endName = ((x[, "End_station"])[[1]])[1]
  return(paste(startName, endName, sep = ", "))
}
routeNames = unlist(lapply(dtListRoutes[toReject], getRoutes))
write.table(routeNames, "rejected_routes.txt", row.names = FALSE)
##--------------------------------------------------------------------------------------------------------
## (5) Analysis of our results 
##--------------------------------------------------------------------------------------------------------
## Samples two residuals plots corresponding to the hyotheses we rejected
## Samples two residuals plots corresponding to the hypotehses we did not reject
##--------------------------------------------------------------------------------------------------------
## (5)(i) Residual plots of the routes corresponding to the hypotheses we rejected 
##--------------------------------------------------------------------------------------------------------
noInteract = "Weekday + Member_type + Rush_hour + Season"
Interact = "Member_type:Rush_hour"
finModel = paste("Duration ~", noInteract, "+", Interact)
# samples 
rRej = dtListRoutes[toReject]
sampRej = sample(1:length(rRej), 2)
# runs linear regression 
modelRej = lm(finModel, data = rRej[[sampRej[1]]])
resVec = residuals(modelRej)
fitVec = fitted(modelRej)
horVec = rep(0, length(resVec))
plotFitted = ggplot(NULL, aes(x = fitVec)) + 
  geom_point(aes(y = resVec), color = "#F47C7C", fill = "#70A1D7", size = 0.2) + 
  geom_line(aes(y = horVec), color = "#F47C7C") + 
  labs(title = "Residual Plot of Rejected Route (Example 1)", x = "Fitted Duration", y = "Residuals") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 11, family = "serif")) + 
  theme(axis.line = element_line(colour = "black")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
plotFitted
png(filename = "plotFitted_rej1.png")
plotFitted
dev.off()
modelRej = lm(finModel, data = rRej[[sampRej[2]]])
resVec = residuals(modelRej)
fitVec = fitted(modelRej)
horVec = rep(0, length(resVec))
plotFitted = ggplot(NULL, aes(x = fitVec)) + 
  geom_point(aes(y = resVec), color = "#F47C7C", fill = "#70A1D7", size = 0.2) + 
  geom_line(aes(y = horVec), color = "#F47C7C") + 
  labs(title = "Residual Plot of Rejected Route (Example 2)", x = "Fitted Duration", y = "Residuals") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 11, family = "serif")) + 
  theme(axis.line = element_line(colour = "black")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
plotFitted
png(filename = "plotFitted_rej2.png")
plotFitted
dev.off()
##--------------------------------------------------------------------------------------------------------
## (5)(ii) Residual plots of the routes corresponding to the hypotheses we did not reject
##--------------------------------------------------------------------------------------------------------
# samples 
rNotRej = dtListRoutes[is.na(pmatch(rRej, 1:length(dtListRoutes)))]
sampNotRej = sample(1:length(rNotRej), 2)
# runs linear regression 
modelNotRej = lm(finModel, data = rNotRej[[sampNotRej[1]]])
resVec = residuals(modelNotRej)
fitVec = fitted(modelNotRej)
horVec = rep(0, length(resVec))
plotFitted = ggplot(NULL, aes(x = fitVec)) + 
  geom_point(aes(y = resVec), color = "#A1DE93", fill = "#70A1D7", size = 0.2) + 
  geom_line(aes(y = horVec), color = "#F47C7C") + 
  labs(title = "Residual Plot of Route Not Rejected (Example 1)", x = "Fitted Duration", y = "Residuals") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 11, family = "serif")) + 
  theme(axis.line = element_line(colour = "black")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
plotFitted
png(filename = "plotFitted_NotRej1.png")
plotFitted
dev.off()
modelNotRej = lm(finModel, data = rNotRej[[sampNotRej[2]]])
resVec = residuals(modelNotRej)
fitVec = fitted(modelNotRej)
horVec = rep(0, length(resVec))
plotFitted = ggplot(NULL, aes(x = fitVec)) + 
  geom_point(aes(y = resVec), color = "#A1DE93", fill = "#70A1D7", size = 0.2) + 
  geom_line(aes(y = horVec), color = "#F47C7C") + 
  labs(title = "Residual Plot of Route Not Rejected (Example 2)", x = "Fitted Duration", y = "Residuals") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 11, family = "serif")) + 
  theme(axis.line = element_line(colour = "black")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
plotFitted
png(filename = "plotFitted_NotRej2.png")
plotFitted
dev.off()
endTime <- Sys.time()
durTime = endTime - startTime
##--------------------------------------------------------------------------------------------------------