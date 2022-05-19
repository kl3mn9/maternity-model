###################################################################
# BECKY BAGGALEY, SEPTEMBER 2020, LENGTH OF STAY MODEL - ZANZIBAR
# CODE TO DETERMINE DELIVERY ROOM AND MATERNITY WARD OCCUPANCY
# IN ZANZIBAR (USING ZANZIBAR-SPECIFIC MATERNITY WARD DATA) 
# AND IN SUB-SAHARAN AFRICA IN GENERAL USING WHO DATA
# EDITS BY CAROLIN VEGVARI 19/10/2020
# EDITS AND REFACTORING BY CAROLIN VEGVARI 16/05/2022
# github: https://github.com/kl3mn9/maternity-model
###################################################################


rm(list=ls())
closeAllConnections()

library(lubridate)	# for handling times and dates

# set working directory
setwd("C:\\Users\\Carolin\\Documents\\Write-up\\BeckyZanzibarMaternity\\")

set.seed(42)		#for reproducibility

######################
###Model parameters###		# CV: consider moving to parameter file to make script file general to any country
######################
timeZone 		<- "Africa/Dar_es_Salaam"
prob_comp		<- 0.15	# % deliveries with complications
min_labour		<- 0.25	# minimum duration in labour ward (hours)
min_postp		<- 2		# minimum duration in postpartum ward (hours)
factor_dur_comp	<- 1.5	# factor increase in duration in labour ward if delivery is complicated
prob_CEmOC_comp	<- 0.9	# probability delivery in CEmOC, complicated delivery (NB// assumes all other deliveries in BEmOC)
prob_CEmOC_uncomp <- 0.33	# probability delivery in CEmOC, uncomplicated delivery (NB// assumes all other deliveries in BEmOC)

births		<- 3600	#35316	# annual number of births
no_CEmoC		<- 1		# number of CEmOC facilities - SCENARIO 1
no_BEmOC		<- 2		# number of BEmOC facilities - SCENARIO 1
					# CHANGE TO no_BEmOC <- 5 FOR SCENARIO 2

###LENGTHS OF STAY PARAMETERS###
###POST PARTUM###
###1 for WHO - sub-Saharan Africa###
shapePPUWHO <- 1.058021		#shape parameter, uncomplicated
scalePPUWHO <- 50.15097		#scale parameter, uncomplicated
shapePPCWHO <- 1.230724		#shape parameter, complicated
scalePPCWHO <- 136.0294		#scale parameter, complicated
###2 for Zanzibar - Kenya###
shapePPUZ <- 1.053336		#shape parameter, uncomplicated
scalePPUZ <- 43.65649		#scale parameter, uncomplicated
shapePPCZ <- 1.08632		#shape parameter, complicated
scalePPCZ <- 143.8400		#scale parameter, complicated
###DELIVERY ROOM###
###Lack of data, so same for WHO and Zanzibar###
shapeDelU <- 3.37 		#shape parameter, uncomplicated
scaleDelU <- 1.23 		#scale parameter, uncomplicated

###Read in Zanzibar data###
# contains data on births in 37 facilities in Zanzibar
data<-read.table("Zanzibar facility dataset.csv", header=T,sep=",", fill=TRUE)
numFacilities <- nrow(data)

test<-data[!(data$no_delivs==0),] #deleting facilities #30 and #32 because there are zero deliveries in the dataset

sum(data$no_delivs)
births <- 12*sum(data$no_delivs)	#estimated no. births per year in facilities in Zanzibar

#generating distributions - duration in delivery and postpartum rooms, Zanziar and general SSA, for births=annual births in Zanzibar
durDelUncomp <- rgamma(n=births,shape=shapeDelU,scale=scaleDelU)				#duration in delivery room, uncomplicated delivery
durDelComp <- factor_dur_comp*rgamma(n=births,shape=shapeDelU,scale=scaleDelU)	#duration in delivery room, complicated delivery

durPPUWHO <- rgamma(n=births,shape=shapePPUWHO,scale=scalePPUZ)	#duration postpartum, uncomplicated delivery, WHO analysis
durPPCWHO <- rgamma(n=births,shape=shapePPCWHO,scale=scalePPCZ)	#duration postpartum, complicated delivery, WHO analysis
durPPUZ <- rgamma(n=births,shape=shapePPUZ,scale=scalePPUZ)		#duration postpartum, uncomplicated delivery, Zanzibar analysis
durPPCZ <- rgamma(n=births,shape=shapePPCZ,scale=scalePPCZ)		#duration postpartum, complicated delivery, Zanzibar analysis


#########################################
###START OF SIMULATION###################
#########################################

#Random date and time function taken from stackoverflow
#http://stackoverflow.com/questions/14720983/efficiently-generate-a-random-sample-of-times-and-dates-between-two-dates

latemail <- function(N, st="2014/01/01", et="2014/12/31") {
	st <- as.POSIXct(as.Date(st))
      et <- as.POSIXct(as.Date(et))
      difft <- as.numeric(difftime(et,st,unit="sec"))
      ev <- sort(runif(N, 0, difft))
     rt <- st + ev
 }


temp3 <- latemail(births)			#each woman set a random date and time of presentation in labour at a health facility
							#the vector orders them by date (for one year - 2014)
tz(temp3) <- timeZone				# set time zone, e.g. to East African Time (for Zanzibar)
labour_start <- temp3[sample(births)]	#labour_start dates and times in random order (means date/time of admission to labour ward)


#########################################################################
###Assigning a facility for delivery and if delivery is complicated######
#########################################################################

facility <- rep(0,births)

facility[1:(12*data$no_delivs[1])] <- 1	# the first 5*12 women are assigned facility 1
							# 5 because as per data 5 women have given birth in facility 1 in one month
							# factor of 12 because we calculate number of births per year from number of births per month


temp5 <- rep(0,length(data$no_delivs))
temp5[1] <- 1
complicatedZ <- rep(0,births)			# complicated births for Zanzibar - determined by how many c sections performed in each facility

# below we assume that complicated births only occur in facilities that perform c-sections in dataset
# if x out of n births in a month at a facility are c-sections 12*x complicated births are assigned to the facility over a year
# (and 12*n births in total)
for(i in 2:nrow(data)) 
{
	temp5[i] <- temp5[i-1] + 12 * data$no_delivs[i-1]
	facility[temp5[i]:(temp5[i] - 1 + (12*data$no_delivs[i]))] <- i

	# assigning c-sections
	if(data$csects[i]>0)  
	{
		for(j in temp5[i]:(temp5[i] - 1 + (12*data$csects[i])))	
			complicatedZ[j] <- 1
	}
}
 				

complicated <- rbinom(births, 1, prob_comp)	# assign whether each woman has a complicated delivery: 1= complicated 0= uncomplicated
                                              	# WHO general


####################################
###Duration in the labour ward######
####################################

dur_labour <- rep(0,births)		
						

dur_labour[complicated==1] <- durDelComp[complicated==1]		# duration in labour ward dependant on whether delivery is complicated
dur_labour[complicated==0] <- durDelUncomp[complicated==0]		# or uncomplicated
dur_labour[dur_labour<min_labour] <- min_labour				# and must be minimum of min_labour

labour_end <- labour_start + dur_labour*60*60				#date and time that delivery ends (dur_labour in hours converted into seconds)


####################################
###Duration in the maternity ward###
####################################

dur_maternityWHO <- rep(0,births)	# duration in maternity ward according to WHO data for SSA
dur_maternityZ <- rep(0,births)	# duration in maternity ward according to Zanzibar data	

# duration in maternity ward dependant on whether delivery is complicated
dur_maternityWHO[complicated==1] <- durPPCWHO[complicated==1]
dur_maternityWHO[complicated==0] <- durPPUWHO[complicated==0]
dur_maternityZ[complicated==1] <- durPPCZ[complicated==1]
dur_maternityZ[complicated==0] <- durPPUZ[complicated==0]


discharge1WHO <- labour_end + dur_maternityWHO*60*60	#literal discharge time i.e., could be in the middle of the night
discharge1Z <- labour_end + dur_maternityZ*60*60


###########################################################################
# convenience functions
###########################################################################

getDischargeTime <- function(dischargeTime)
{
	# correcting discharge time and date so discharge can only occur between 8am and 8pm. If discharge is 
	# due to occur during the night it is pushed back until the following morning, 8am
	time <- format(dischargeTime,"%H:%M:%S")
	date <- as.Date(dischargeTime)
	date[time>"20:00:00"] <- date[time>"20:00:00"] + 1
	time[time>"20:00:00"] <- "08:00:00"
	time[time<"08:00:00"] <- "08:00:00"

	#converting dischargeTime to finish ensures women aren't discharged during the night
	finish <- as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S")

	return(finish)	
}

# function to generate matrix of events
event.array <- function(x, interval)
{
  	len <- length(interval)
	matrix(rep(unclass(x), len), ncol=len)
}

# function to generate matrix of time intervals
intervals.array <- function(x, intervals)
{
  	len <- length(x)
  	matrix(rep(unclass(intervals), len), nrow=len, byrow=TRUE)
}

# function to determine occupancy in each facility for each snapshot
getOccupancy <- function(a.start, a.finish, a.intervals, facility.number)
{
	count <- colSums(a.start[facility==facility.number,] <= a.intervals[facility==facility.number,] & a.finish[facility==facility.number,] >= a.intervals[facility==facility.number,])
}

# function to determine occupancy of different wards/rooms depending on starTime, finish entered
getOccupancyByCentreSnapshot <- function(start, finish, breaks, timeZone, numFacilities)
{ 
	a.start <- event.array(start, breaks)		 
	a.finish <- event.array(finish, breaks)
	a.intervals <- intervals.array(start, breaks)

	freq <- breaks
	facility.numbers <- seq(1, numFacilities)
	# omit facilities 30 and 32 because zero deliveries were recorded
	# this is specific to Zanzibar
	facility.numbers <- facility.numbers[-c(30, 32)]
	for(facility.number in facility.numbers)
	{
		count <- getOccupancy(a.start, a.finish, a.intervals, facility.number)
		freq <- cbind(freq, count)	
	} 

	freq <- as.data.frame(freq)
	names(freq) <- c("Snapshot", paste0("count", facility.numbers))
	freq$Snapshot <- breaks

	return(freq)
}

#################################################################################################################################
# OCCUPANCY AT SNAPSHOTS IN TIME
#################################################################################################################################

# times to generate 'snapshots', 
# i.e. times at which to establish delivery room and maternity ward occupancy
breaks <- seq(as.POSIXct('2014-01-01 00:00', tz = timeZone), by = '1 hours', length = 365*24+1)
temp <- format(breaks,"%H:%M:%S")
# 3 snapshots per day over a year
breaks3 <- breaks[temp=="04:00:00" | temp=="12:00:00" |temp=="20:00:00"]

# ZANZIBAR ANALYSIS #############################################################################################################

dischargeTime <- getDischargeTime(discharge1Z)

# Occupancy maternity centre (including time spent in delivery room and time in post-partum room
freqZ <- getOccupancyByCentreSnapshot(labour_start, dischargeTime, breaks3, timeZone, numFacilities)

# Occupancy delivery room 
freqLabourZ <- getOccupancyByCentreSnapshot(labour_start, labour_end, breaks3, timeZone, numFacilities)

# Occupancy post-partum room
freqMatZ <- getOccupancyByCentreSnapshot(labour_end, dischargeTime, breaks3, timeZone, numFacilities)


# WHO SSA ANALYSIS #############################################################################################################

dischargeTime <- getDischargeTime(discharge1WHO)

# Occupancy maternity centre (including time spent in delivery room and time in post-partum room
freqWHO <- getOccupancyByCentreSnapshot(labour_start, dischargeTime, breaks3, timeZone, numFacilities)

# Occupancy delivery room - same as Zanzibar because labour start and end times same
freqLabourWHO <- getOccupancyByCentreSnapshot(labour_start, labour_end, breaks3, timeZone, numFacilities)

# Occupancy post-partum room
freqMatWHO <- getOccupancyByCentreSnapshot(labour_end, dischargeTime, breaks3, timeZone, numFacilities)


#################################################################################################################################
# PROPORTION OF TIME EACH FACILITY IS OVER CAPACITY OR EMPTY --> EVALUATE CAPACITY EVERY HOUR RATHER THAN EVERY 8 HOURS
#################################################################################################################################

breaks <- seq(as.POSIXct('2014-01-01 00:00', tz = "GMT"),by = '1 hours', length = 365*24+1)

freqLabourZHour <- getOccupancyByCentreSnapshot(labour_start, labour_end, breaks, timeZone, numFacilities)

# How can there be centres with labour ward capacity 0? Do women get transferred there from other centres after labour?
capacityLW <- data$beds_deliv[-c(30, 32)]		
capacityMW <- data$beds_matern[-c(30, 32)]
capacity <- capacityLW + capacityMW

# which of the above is the right capacity to evaluate? i.e. do births also happen in maternity wards 
# in centres where there are zero delivery beds?
hoursOverCapacity <- colSums(sweep(freqLabourZHour[, 2:ncol(freqLabourZHour)], 2, capacityLW, ">"))
percentTimeOverCapacity <- hoursOverCapacity / nrow(freqLabourZHour) * 100


hoursEmpty <- colSums(freqLabourZHour[, 2:ncol(freqLabourZHour)]==0)
percentTimeEmpty <- hoursEmpty / nrow(freqLabourZHour) * 100


