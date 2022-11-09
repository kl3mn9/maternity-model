###################################################################
# BECKY BAGGALEY, SEPTEMBER 2020, LENGTH OF STAY MODEL - ZANZIBAR
# CODE TO DETERMINE DELIVERY ROOM AND MATERNITY WARD OCCUPANCY
# IN ZANZIBAR (USING ZANZIBAR-SPECIFIC MATERNITY WARD DATA) 
# AND IN SUB-SAHARAN AFRICA IN GENERAL USING WHO DATA
# EDITS AND REFACTORING BY CAROLIN VEGVARI 16/05/2022
# EDITS BY CAROLIN VEGVARI 07/11/2020
# USE LOS DISTRIBUTIONS FITTED TO LATEST TANZANIA DHS DATA
# USE DATA ON SEASONALITY OF BIRTHS IN ZANZIBAR FROM TANZANIA DHS
# ADD FUNCTION TO SIMULATE SEASONAL BIRTH DATA 09/11/2022
# github: https://github.com/kl3mn9/maternity-model
###################################################################


rm(list=ls())
closeAllConnections()

library(lubridate)	# for handling times and dates
library(ggplot2)		# for plotting
library(tidyr)		# for data formating
library(cowplot)		# for composite plots


# set working directory
setwd("C:\\Users\\Carolin\\Oriole Global Health\\OGH Team - Documents\\02. Business Development\\03. Business Outreach\\MaternityModel\\ZanzibarMaternity\\")

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
###2 for Zanzibar - Tanzania DHS###
shapePPUZ <- 0.9801		#shape parameter, uncomplicated
scalePPUZ <- 19.10		#scale parameter, uncomplicated
shapePPCZ <- 1.794		#shape parameter, complicated
scalePPCZ <- 82.64		#scale parameter, complicated
###DELIVERY ROOM###
###Lack of data, so same for WHO and Zanzibar###
shapeDelU <- 3.37 		#shape parameter, uncomplicated
scaleDelU <- 1.23 		#scale parameter, uncomplicated

###Read in Zanzibar data###
# contains data on births in 37 facilities in Zanzibar
data<-read.table("Zanzibar facility dataset.csv", header=T,sep=",", fill=TRUE)
numFacilities <- nrow(data)

df.fac.snaps<-data[!(data$no_delivs==0),] #deleting facilities 30 and 32 because there are zero deliveries in the dataset

sum(data$no_delivs)
births <- 12*sum(data$no_delivs)	#estimated no. births per year in facilities in Zanzibar

#generating distributions - duration in delivery and postpartum rooms, Zanzibar and general SSA, for births=annual births in Zanzibar
durDelUncomp <- rgamma(n=births,shape=shapeDelU,scale=scaleDelU)				#duration in delivery room, uncomplicated delivery
durDelComp <- factor_dur_comp*rgamma(n=births,shape=shapeDelU,scale=scaleDelU)	#duration in delivery room, complicated delivery

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


# weight months by number of births per month from TANZANIA DHS data for ZANZIBAR
weights <- read.table("ZNZ_seasonality_weights.txt", header=FALSE)[,1]

getDT <- function(N, weights, start="2014/01/01")
{
	w <- c(rep(weights[1], 31*24*60*60),
		rep(weights[2], 28*24*60*60),
		rep(weights[3], 31*24*60*60),
		rep(weights[4], 30*24*60*60),
		rep(weights[5], 31*24*60*60),
		rep(weights[6], 30*24*60*60),
		rep(weights[7], 31*24*60*60),
		rep(weights[8], 31*24*60*60),
		rep(weights[9], 30*24*60*60),
		rep(weights[10], 31*24*60*60),
		rep(weights[11], 30*24*60*60),
		rep(weights[12], 31*24*60*60))

	times <- sample(1:length(w), N, replace=TRUE, prob=w)	
	dateTime <- as.POSIXct(as.Date(start)) + as.numeric(times)

	return(dateTime)
}


labour_start <- getDT(N=births, weights=weights)	#each woman set a random date and time of presentation in labour at a health facility
tz(labour_start) <- timeZone					# set time zone, e.g. to East African Time (for Zanzibar)	


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
						

dur_labour[complicatedZ==1] <- durDelComp[complicatedZ==1]		# duration in labour ward dependant on whether delivery is complicated
dur_labour[complicatedZ==0] <- durDelUncomp[complicatedZ==0]	# or uncomplicated
dur_labour[dur_labour<min_labour] <- min_labour				# and must be minimum of min_labour

labour_end <- labour_start + dur_labour*60*60				#date and time that delivery ends (dur_labour in hours converted into seconds)


####################################
###Duration in the maternity ward###
####################################

dur_maternityWHO <- rep(0,births)	# duration in maternity ward according to WHO data for SSA
dur_maternityZ <- rep(0,births)	# duration in maternity ward according to Zanzibar data	

# duration in maternity ward dependant on whether delivery is complicated
dur_maternityZ[complicatedZ==1] <- durPPCZ[complicatedZ==1]
dur_maternityZ[complicatedZ==0] <- durPPUZ[complicatedZ==0]

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
getOccupancy <- function(a.start, a.finish, a.intervals, facility, facility.number)
{
	count <- colSums(a.start[facility==facility.number,] <= a.intervals[facility==facility.number,] & a.finish[facility==facility.number,] >= a.intervals[facility==facility.number,])
}

# function to determine occupancy of different wards/rooms depending on start Time, finish entered
getOccupancyByCentreSnapshot <- function(start, finish, breaks, facility, facility.numbers)
{ 
	a.start <- event.array(start, breaks)		 
	a.finish <- event.array(finish, breaks)
	a.intervals <- intervals.array(start, breaks)

	freq <- breaks
	for(facility.number in facility.numbers)
	{
		count <- getOccupancy(a.start, a.finish, a.intervals, facility, facility.number)
		freq <- cbind(freq, count)	
	} 

	freq <- as.data.frame(freq)
	names(freq) <- c("Snapshot", paste0("count", facility.numbers))
	freq$Snapshot <- breaks

	return(freq)
}

#################################################################################################################################
# OCCUPANCY AT SNAPSHOTS IN TIME (4:00, 12:00, 20:00)
#################################################################################################################################

# times to generate 'snapshots', 
# i.e. times at which to establish delivery room and maternity ward occupancy
# 3 snapshots per day over a year
breaks <- seq(as.POSIXct('2014-01-01 00:00', tz = timeZone), by = '1 hours', length = 365*24+1)
temp <- format(breaks,"%H:%M:%S")
breaks3 <- breaks[temp=="04:00:00" | temp=="12:00:00" |temp=="18:00:00"]


# all births 
dischargeTime <- getDischargeTime(discharge1Z)
tz(dischargeTime) <- timeZone

facility.numbers <- seq(1, numFacilities)
# omit facilities 30 and 32 because zero deliveries were recorded
# this is specific to Zanzibar
facility.numbers <- facility.numbers[-c(30, 32)]

# Occupancy maternity centre (including time spent in delivery room and time in post-partum room
freqZ <- getOccupancyByCentreSnapshot(labour_start, dischargeTime, breaks3, facility, facility.numbers)

# Occupancy delivery room 
freqLabourZ <- getOccupancyByCentreSnapshot(labour_start, labour_end, breaks3, facility, facility.numbers)

# Occupancy post-partum room
freqMatZ <- getOccupancyByCentreSnapshot(labour_end, dischargeTime, breaks3, facility, facility.numbers)


# complicated births only
labour_start_comp <- labour_start[complicatedZ==1]
labour_end_comp <- labour_end[complicatedZ==1]
dischargeTime_comp <- dischargeTime[complicatedZ==1]

facility.numbers.c <- seq(1, numFacilities)
# only consider facilities carrying out c-sections
facility.numbers.c <- facility.numbers.c[data$csects > 0]
facility.c <- facility[complicatedZ==1]

# Occupancy maternity centre (including time spent in delivery room and time in post-partum room
freqZ_comp <- getOccupancyByCentreSnapshot(labour_start_comp, dischargeTime_comp, breaks3, facility.c, facility.numbers.c)

# Occupancy delivery room 
freqLabourZ_comp <- getOccupancyByCentreSnapshot(labour_start_comp, labour_end_comp, breaks3, facility.c, facility.numbers.c)

# Occupancy post-partum room
freqMatZ_comp <- getOccupancyByCentreSnapshot(labour_end_comp, dischargeTime_comp, breaks3, facility.c, facility.numbers.c)


#################################################################################################################################
# PLOT SNAPSHOTS OF WOMEN IN FACILITIES, DELIVERY ROOM AND POST-PARTUM ROOM
#################################################################################################################################

# WOMEN IN FACILITIES
# Plot total number of births and complicated births over a representative month (snapshots, facility occupancy)
df.total.snaps <- data.frame(Snapshot=breaks3, TotalBirths=rowSums(freqZ[, 2:ncol(freqZ)]), CompBirths=rowSums(freqZ_comp[,2:ncol(freqZ_comp)]))
df.total.snaps$UncompBirths <- df.total.snaps$TotalBirths - df.total.snaps$CompBirths

# 178:270 are rownumbers of snapshots in March
df.total.snaps.long.Mar <- gather(df.total.snaps[178:270, -2], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)
df.total.snaps.long <- gather(df.total.snaps[, -2], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

p1 <- ggplot(df.total.snaps.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a year") + ylab("Number of women in EmOCs") + 
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=16))
print(p1)
#ggsave("TotalBirthsYear_TNZ.png", plot=p1, dpi=300, width=12, height=8)


p1a <- ggplot(df.total.snaps.long.Mar, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a month") + ylab("Number of women in EmOCs") + 
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=16))
print(p1a)
#ggsave("TotalBirthsMarch_TNZ.png", plot=p1a, dpi=300, width=12, height=8)


# Plot number of births and complicated births over a year by facility (snapshots, facility occupancy)
freqZ.long <- gather(freqZ, Facility, NumBirths, count1:count37)
freqZ_comp.long <- gather(freqZ_comp, Facility, CompBirths, count5:count36)
df.fac.snaps <- merge(freqZ.long, freqZ_comp.long, by=c("Snapshot", "Facility"), all=TRUE)
df.fac.snaps <- df.fac.snaps[order(df.fac.snaps$Facility, df.fac.snaps$Snapshot), ]
df.fac.snaps$CompBirths[is.na(df.fac.snaps$CompBirths)] <- 0
df.fac.snaps$UncompBirths <- df.fac.snaps$NumBirths - df.fac.snaps$CompBirths
df.fac.snaps.long <- gather(df.fac.snaps[, -3], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

fac.levels <- paste0("count", 1:37)
fac.levels <- fac.levels[-c(30, 32)]
df.fac.snaps.long$Facility <- factor(df.fac.snaps.long$Facility, levels=fac.levels) 
df.fac.snaps.long <- df.fac.snaps.long[order(df.fac.snaps.long$Facility, df.fac.snaps.long$Snapshot), ]
levels(df.fac.snaps.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p2 <- ggplot(df.fac.snaps.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a year") + ylab("Number of women in EmOCs") + facet_wrap(~ Facility, ncol=5) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
print(p2)
#ggsave("BirthsFacilityYear_TNZ.png", plot=p2, dpi=300, width=18, height=18)


# Plot number of births and complicated births over a representative month (March) by facility (snapshots, facility occupancy)
freqZ.long.Mar <- gather(freqZ[178:270,], Facility, NumBirths, count1:count37)
freqZ_comp.long.Mar <- gather(freqZ_comp[178:270,], Facility, CompBirths, count5:count36)
df.fac.snaps <- merge(freqZ.long.Mar, freqZ_comp.long.Mar, by=c("Snapshot", "Facility"), all=TRUE)
df.fac.snaps <- df.fac.snaps[order(df.fac.snaps$Facility, df.fac.snaps$Snapshot), ]
df.fac.snaps$CompBirths[is.na(df.fac.snaps$CompBirths)] <- 0
df.fac.snaps$UncompBirths <- df.fac.snaps$NumBirths - df.fac.snaps$CompBirths
df.fac.snaps.long <- gather(df.fac.snaps[, -3], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

fac.levels <- paste0("count", 1:37)
fac.levels <- fac.levels[-c(30, 32)]
df.fac.snaps.long$Facility <- factor(df.fac.snaps.long$Facility, levels=fac.levels) 
df.fac.snaps.long <- df.fac.snaps.long[order(df.fac.snaps.long$Facility, df.fac.snaps.long$Snapshot), ]
levels(df.fac.snaps.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p2a <- ggplot(df.fac.snaps.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a year") + ylab("Number of women in EmOCs") + facet_wrap(~ Facility, ncol=5) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
print(p2a)
#ggsave("BirthsFacilityMarch_TNZ.png", plot=p2a, dpi=300, width=18, height=18)



# WOMEN IN DELIVERY ROOMS
# Plot total number of births and complicated births over a representative month (snapshots, facility occupancy)
df.del.snaps <- data.frame(Snapshot=breaks3, TotalBirths=rowSums(freqLabourZ[, 2:ncol(freqLabourZ)]), CompBirths=rowSums(freqLabourZ_comp[,2:ncol(freqLabourZ_comp)]))
df.del.snaps$UncompBirths <- df.del.snaps$TotalBirths - df.del.snaps$CompBirths

# 178:270 are rownumbers of snapshots in March
df.del.snaps.long.Mar <- gather(df.del.snaps[178:270, -2], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)
df.del.snaps.long <- gather(df.del.snaps[, -2], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

p3 <- ggplot(df.del.snaps.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a year") + ylab("Number of women in delivery rooms") + 
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=16))
print(p3)
#ggsave("TotalDelYear_TNZ.png", plot=p3, dpi=300, width=12, height=8)


p3a <- ggplot(df.del.snaps.long.Mar, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a year") + ylab("Number of women in delivery rooms") + 
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=16))
print(p3a)
#ggsave("TotalDelMarch_TNZ.png", plot=p3a, dpi=300, width=12, height=8)



# Plot number of births and complicated births over a year by facility (snapshots, facility occupancy)
freqLabourZ.long <- gather(freqLabourZ, Facility, NumBirths, count1:count37)
freqLabourZ_comp.long <- gather(freqLabourZ_comp, Facility, CompBirths, count5:count36)
df.del.fac.snaps <- merge(freqLabourZ.long, freqLabourZ_comp.long, by=c("Snapshot", "Facility"), all=TRUE)
df.del.fac.snaps <- df.del.fac.snaps[order(df.del.fac.snaps$Facility, df.del.fac.snaps$Snapshot), ]
df.del.fac.snaps$CompBirths[is.na(df.del.fac.snaps$CompBirths)] <- 0
df.del.fac.snaps$UncompBirths <- df.del.fac.snaps$NumBirths - df.del.fac.snaps$CompBirths
df.del.fac.snaps.long <- gather(df.del.fac.snaps[, -3], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

fac.levels <- paste0("count", 1:37)
fac.levels <- fac.levels[-c(30, 32)]
df.del.fac.snaps.long$Facility <- factor(df.del.fac.snaps.long$Facility, levels=fac.levels) 
df.del.fac.snaps.long <- df.del.fac.snaps.long[order(df.del.fac.snaps.long$Facility, df.del.fac.snaps.long$Snapshot), ]
levels(df.del.fac.snaps.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p4 <- ggplot(df.del.fac.snaps.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a year") + ylab("Number of women in delivery rooms") + facet_wrap(~ Facility, ncol=5) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
print(p4)
#ggsave("DelFacilityYear_TNZ.png", plot=p4, dpi=300, width=18, height=18)


# Plot number of births and complicated births over a representative month (March) by facility (snapshots, facility occupancy)
freqLabourZ.long <- gather(freqLabourZ[178:270,], Facility, NumBirths, count1:count37)
freqLabourZ_comp.long <- gather(freqLabourZ_comp[178:270,], Facility, CompBirths, count5:count36)
df.del.fac.snaps <- merge(freqLabourZ.long, freqLabourZ_comp.long, by=c("Snapshot", "Facility"), all=TRUE)
df.del.fac.snaps <- df.del.fac.snaps[order(df.del.fac.snaps$Facility, df.del.fac.snaps$Snapshot), ]
df.del.fac.snaps$CompBirths[is.na(df.del.fac.snaps$CompBirths)] <- 0
df.del.fac.snaps$UncompBirths <- df.del.fac.snaps$NumBirths - df.del.fac.snaps$CompBirths
df.del.fac.snaps.long <- gather(df.del.fac.snaps[, -3], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

fac.levels <- paste0("count", 1:37)
fac.levels <- fac.levels[-c(30, 32)]
df.del.fac.snaps.long$Facility <- factor(df.del.fac.snaps.long$Facility, levels=fac.levels) 
df.del.fac.snaps.long <- df.del.fac.snaps.long[order(df.del.fac.snaps.long$Facility, df.del.fac.snaps.long$Snapshot), ]
levels(df.del.fac.snaps.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p4a <- ggplot(df.del.fac.snaps.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a year") + ylab("Number of women in delivery rooms") + facet_wrap(~ Facility, ncol=5) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
print(p4a)
#ggsave("DelFacilityMarch_TNZ.png", plot=p4a, dpi=300, width=18, height=18)



# WOMEN IN MATERNITY BEDS (POST-PARTUM)
# Plot total number of births and complicated births over a representative month (snapshots, facility occupancy)
df.mat.snaps <- data.frame(Snapshot=breaks3, TotalBirths=rowSums(freqMatZ[, 2:ncol(freqMatZ)]), CompBirths=rowSums(freqMatZ_comp[,2:ncol(freqMatZ_comp)]))
df.mat.snaps$UncompBirths <- df.mat.snaps$TotalBirths - df.mat.snaps$CompBirths

# 178:270 are rownumbers of snapshots in March
#df.mat.snaps.long <- gather(df.mat.snaps[178:270, -2], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)
df.mat.snaps.long <- gather(df.mat.snaps[, -2], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

p5 <- ggplot(df.mat.snaps.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a year") + ylab("Number of women in post-partum beds") + 
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=16))
print(p5)
#ggsave("TotalMatYear_TNZ.png", plot=p5, dpi=300, width=12, height=8)


# Plot number of births and complicated births over a representative month by facility (snapshots, facility occupancy)
#freqMatZ.long <- gather(freqMatZ[178:270,], Facility, NumBirths, count1:count37)
#freqMatZ_comp.long <- gather(freqMatZ_comp[178:270,], Facility, CompBirths, count5:count36)
freqMatZ.long <- gather(freqMatZ, Facility, NumBirths, count1:count37)
freqMatZ_comp.long <- gather(freqMatZ_comp, Facility, CompBirths, count5:count36)
df.mat.fac.snaps <- merge(freqMatZ.long, freqMatZ_comp.long, by=c("Snapshot", "Facility"), all=TRUE)
df.mat.fac.snaps <- df.mat.fac.snaps[order(df.mat.fac.snaps$Facility, df.mat.fac.snaps$Snapshot), ]
df.mat.fac.snaps$CompBirths[is.na(df.mat.fac.snaps$CompBirths)] <- 0
df.mat.fac.snaps$UncompBirths <- df.mat.fac.snaps$NumBirths - df.mat.fac.snaps$CompBirths
df.mat.fac.snaps.long <- gather(df.mat.fac.snaps[, -3], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

fac.levels <- paste0("count", 1:37)
fac.levels <- fac.levels[-c(30, 32)]
df.mat.fac.snaps.long$Facility <- factor(df.mat.fac.snaps.long$Facility, levels=fac.levels) 
df.mat.fac.snaps.long <- df.mat.fac.snaps.long[order(df.mat.fac.snaps.long$Facility, df.mat.fac.snaps.long$Snapshot), ]
levels(df.mat.fac.snaps.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p6 <- ggplot(df.mat.fac.snaps.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a year") + ylab("Number of women in post-partum beds") + facet_wrap(~ Facility, ncol=5) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
print(p6)
#ggsave("MatFacilityYear_TNZ.png", plot=p6, dpi=300, width=18, height=18)



#################################################################################################################################
# PROPORTION OF TIME EACH FACILITY IS OVER CAPACITY OR EMPTY --> EVALUATE CAPACITY EVERY HOUR RATHER THAN EVERY 8 HOURS
#################################################################################################################################

# repeat n times to get mean and 95% credible interval
n <- 100
overLW.ll <- list()
emptyLW.ll <- list()
birthsPerSBA.ll <- list()
overSBA.ll <- list()
overSBA4.ll <- list()

#overMW.ll <- list()
#emptyMW.ll <- list()

# How can there be centres with labour ward capacity 0? Do women get transferred there from other centres after labour?
capacityLW <- data$beds_deliv[-c(30, 32)]		
capacityMW <- data$beds_matern[-c(30, 32)]
capacity <- capacityLW + capacityMW

capacitySBA_am <- data$SBA_am[-c(30,32)]
capacitySBA_pm <- data$SBA_pm[-c(30,32)]
capacitySBA_am4 <- data$SBA_am[-c(30,32)] * 4
capacitySBA_pm4 <- data$SBA_pm[-c(30,32)] * 4

start <- proc.time()

for(i in 1:n)
{
	# randomly draw distributions of duration in delivery and postpartum rooms 
	# for births=annual births in Zanzibar
	durDelUncomp <- rgamma(n=births,shape=shapeDelU,scale=scaleDelU)				#duration in delivery room, uncomplicated delivery
	durDelComp <- factor_dur_comp*rgamma(n=births,shape=shapeDelU,scale=scaleDelU)	#duration in delivery room, complicated delivery
	durPPUZ <- rgamma(n=births,shape=shapePPUZ,scale=scalePPUZ)					#duration postpartum, uncomplicated delivery, Zanzibar analysis
	durPPCZ <- rgamma(n=births,shape=shapePPCZ,scale=scalePPCZ)					#duration postpartum, complicated delivery, Zanzibar analysis

	labour_start <- getDT(N=births, weights=weights)	#each woman set a random date and time of presentation in labour at a health facility
	tz(labour_start) <- timeZone					# set time zone, e.g. to East African Time (for Zanzibar)
	
	# assign a facility for delivery and if delivery is complicated
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
	for(j in 2:nrow(data)) 
	{
		temp5[j] <- temp5[j-1] + 12 * data$no_delivs[j-1]
		facility[temp5[j]:(temp5[j] - 1 + (12*data$no_delivs[j]))] <- j

		# assigning c-sections
		if(data$csects[j]>0)  
		{
			for(k in temp5[j]:(temp5[j] - 1 + (12*data$csects[j])))	
				complicatedZ[k] <- 1
		}
	} 				


	# Duration in the labour ward
	dur_labour <- rep(0,births)			
	dur_labour[complicatedZ==1] <- durDelComp[complicatedZ==1]		# duration in labour ward dependant on whether delivery is complicated
	dur_labour[complicatedZ==0] <- durDelUncomp[complicatedZ==0]	# or uncomplicated
	dur_labour[dur_labour<min_labour] <- min_labour				# and must be minimum of min_labour
	labour_end <- labour_start + dur_labour*60*60				#date and time that delivery ends (dur_labour in hours converted into seconds)

	# Duration in the maternity ward
	dur_maternityZ <- rep(0,births)		

	# duration in maternity ward dependant on whether delivery is complicated
	dur_maternityZ[complicatedZ==1] <- durPPCZ[complicatedZ==1]
	dur_maternityZ[complicatedZ==0] <- durPPUZ[complicatedZ==0]

	discharge1Z <- labour_end + dur_maternityZ*60*60	# literal discharge time, could be in the middle of the night
	dischargeTime <- getDischargeTime(discharge1Z)		# change discharge time at night to early morning


	# get occupancy of maternity facilities and delivery rooms every hour
	breaks <- seq(as.POSIXct('2014-01-01 00:00', tz = "GMT"),by = '1 hours', length = 365*24+1)

	# get labour room occupancy
	freqLabourZHour <- getOccupancyByCentreSnapshot(labour_start, labour_end, breaks, facility, facility.numbers)
	# get maternity ward (post-partum) occupancy
	#freqMatZHour <- getOccupancyByCentreSnapshot(labour_end, dischargeTime, breaks, facility, facility.numbers)	

	
	# DETERMINE OVERCROWDING IN TERMS OF BEDS
	# which of the above is the right capacity to evaluate? i.e. do births also happen in maternity wards 
	# in centres where there are zero delivery beds?
	hoursLWOverCapacity <- colSums(sweep(freqLabourZHour[, 2:ncol(freqLabourZHour)], 2, capacityLW, ">"))
	percentTimeLWOverCapacity <- hoursLWOverCapacity / nrow(freqLabourZHour) * 100
	
	# DETERMINE HOW MUCH OF THE TIME LABOUR ROOMS ARE EMPTY
	hoursLWEmpty <- colSums(freqLabourZHour[, 2:ncol(freqLabourZHour)]==0)
	percentTimeLWEmpty <- hoursLWEmpty / nrow(freqLabourZHour) * 100

	#hoursMWOverCapacity <- colSums(sweep(freqMatZHour[, 2:ncol(freqMatZHour)], 2, capacityMW, ">"))
	#percentTimeMWOverCapacity <- hoursMWOverCapacity / nrow(freqMatZHour) * 100
	#hoursMWEmpty <- colSums(freqMatZHour[, 2:ncol(freqMatZHour)]==0)
	#percentTimeMWEmpty <- hoursMWEmpty / nrow(freqMatZHour) * 100

	overLW.ll[[i]] <- percentTimeLWOverCapacity
	emptyLW.ll[[i]] <- percentTimeLWEmpty

	#overMW.ll[[i]] <- percentTimeMWOverCapacity
	#emptyMW.ll[[i]] <- percentTimeMWEmpty


	# DETERMINE OVERCROWDING IN TERMS OF SBAs BY am AND pm SHIFTS
	hoursLWOverSBA_am <- colSums(sweep(freqLabourZHour[format(freqLabourZHour$Snapshot, "%p")=="am", 2:ncol(freqLabourZHour)], 2, capacitySBA_am, ">"))
	hoursLWOverSBA_pm <- colSums(sweep(freqLabourZHour[format(freqLabourZHour$Snapshot, "%p")=="pm", 2:ncol(freqLabourZHour)], 2, capacitySBA_pm, ">"))

	hoursLWOverSBA <- hoursLWOverSBA_am + hoursLWOverSBA_pm 
	overSBA.ll[[i]] <- hoursLWOverSBA / nrow(freqLabourZHour) * 100

	# DETERMINE OVERCROWDING IN TERMS OF SBAs BY am AND pm SHIFTS (ASSUME 4 WOMEN PER SBA ARE OK)
	hoursLWOverSBA_am4 <- colSums(sweep(freqLabourZHour[format(freqLabourZHour$Snapshot, "%p")=="am", 2:ncol(freqLabourZHour)], 2, capacitySBA_am4, ">"))
	hoursLWOverSBA_pm4 <- colSums(sweep(freqLabourZHour[format(freqLabourZHour$Snapshot, "%p")=="pm", 2:ncol(freqLabourZHour)], 2, capacitySBA_pm4, ">"))

	hoursLWOverSBA4 <- hoursLWOverSBA_am4 + hoursLWOverSBA_pm4 
	overSBA4.ll[[i]] <- hoursLWOverSBA4 / nrow(freqLabourZHour) * 100

}

end <- proc.time() - start
print(end)

df.overLW <- as.data.frame(do.call(rbind, overLW.ll))
df.emptyLW <- as.data.frame(do.call(rbind, emptyLW.ll))
df.overSBA <- as.data.frame(do.call(rbind, overSBA.ll))
df.overSBA4 <- as.data.frame(do.call(rbind, overSBA4.ll))

# save output data
write.csv(x=df.overLW, file="pctTimeExceedLWCapacity_TNZseason.csv", row.names=FALSE)
write.csv(x=df.emptyLW, file="pctTimeEmptyLW_TNZseason.csv", row.names=FALSE)
write.csv(x=df.overSBA, file="pctTimeExceedSBACapacity_TNZseason.csv", row.names=FALSE)
write.csv(x=df.overSBA, file="pctTimeExceedSBA4Capacity_TNZseason.csv", row.names=FALSE)


#################################################################################################################################
# PLOT PERCENT OF TIME DURING WHICH FACILITIES EXCEED CAPACITY OR ARE EMPTY
#################################################################################################################################

df.overLW.long <- gather(df.overLW, Facility, PctTimeOverLWCapacity, count1:count37, factor_key=TRUE)
levels(df.overLW.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p7 <- ggplot(df.overLW.long, aes(x=Facility, y=PctTimeOverLWCapacity)) + theme_classic() + geom_boxplot() + 
	ylab("% Time bed capacity in delivery room exceeded") + ylim(0, 17) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_blank())
print(p7)
#ggsave("bedCapacityExceeded100_TNZseason.png", plot=p7, dpi=300, width=18, height=9)

df.emptyLW.long <- gather(df.emptyLW, Facility, PctTimeLWEmpty, count1:count37, factor_key=TRUE)
levels(df.emptyLW.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p8 <- ggplot(df.emptyLW.long, aes(x=Facility, y=PctTimeLWEmpty)) + theme_classic() + geom_boxplot() + 
	ylab("% Time delivery room is empty") + ylim(0, 100) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_blank())
print(p8)
#ggsave("emptyLW100_TNZseason.png", plot=p8, dpi=300, width=18, height=9)


df.overSBA.long <- gather(df.overSBA, Facility, PctTimeOverSBACapacity, count1:count37, factor_key=TRUE)
levels(df.overSBA.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p9 <- ggplot(df.overSBA.long, aes(x=Facility, y=PctTimeOverSBACapacity)) + theme_classic() + geom_boxplot() + 
	ylab("% Time when more women than SBAs in delivery room") + ylim(0, 50) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1))
print(p9)
#ggsave("SBACapacityExceeded100_TNZ.png", plot=p9, dpi=300, width=18, height=9)

pic <- plot_grid(p7, p8, p9, labels=c('a', 'b', 'c'), nrow=3)
#save_plot(filename="capacityLW100_TNZseason.png", plot=pic, base_height=18, base_width=15, dpi=300)


df.overSBA4.long <- gather(df.overSBA4, Facility, PctTimeOverSBACapacity, count1:count37, factor_key=TRUE)
levels(df.overSBA4.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p9a <- ggplot(df.overSBA4.long, aes(x=Facility, y=PctTimeOverSBACapacity)) + theme_classic() + geom_boxplot() + 
	ylab("% Time when >4 women per SBA in delivery room") + ylim(0, 50) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1))
print(p9a)
ggsave("SBACapacityExceeded4_TNZseason.png", plot=p9a, dpi=300, width=18, height=9)


pica <- plot_grid(p7, p8, p9a, labels=c('a', 'b', 'c'), nrow=3)
save_plot(filename="capacityLW100.4_TNZseason.png", plot=pica, base_height=18, base_width=15, dpi=300)




#################################################################################################################################
# PLOT BIRTHS PER SBA BY FACILITY RELATIVE TO BIRTHS REQUIRED TO STAY IN TRAINING
#################################################################################################################################

# DETERMINE BIRTHS PER SBA PER YEAR
# assuming SBAs are not shared among facilities
birthsPerSBA <- as.vector(table(facility)[-c(30,32)] / data$SBA_total[-c(30,32)])
df.birthsPerSBA <- data.frame(Facility=paste("Facility", seq(1, 37)[-c(30,32)]), BirthsPerSBA=birthsPerSBA)
df.birthsPerSBA$Facility <- factor(df.birthsPerSBA$Facility, levels=paste("Facility", seq(1, 37)[-c(30,32)]))
birthsRequired <- 175

p10 <- ggplot(df.birthsPerSBA, aes(x=Facility, y=BirthsPerSBA)) + theme_classic() + geom_col(fill="mediumpurple") +
	geom_hline(yintercept=birthsRequired, colour="red", size=1.5) + ylab("Births per SBA per year") +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1))
print(p10)
ggsave("birthsPerSBA_TNZseason.png", plot=p10, dpi=300, width=18, height=9)



#########################################################################################################
# % empty delivery room for most and least busy months (June vs January)
#########################################################################################################

# repeat n times to get mean and 95% credible interval
n <- 100

emptyLW.Jun.ll <- list()
emptyLW.Jan.ll <- list()
overLW.Jun.ll <- list()
overLW.Jan.ll <- list()
overSBA.Jun.ll <- list()
overSBA.Jan.ll <- list()
overSBA4.Jun.ll <- list()
overSBA4.Jan.ll <- list()

# How can there be centres with labour ward capacity 0? Do women get transferred there from other centres after labour?
capacityLW <- data$beds_deliv[-c(30, 32)]		

capacitySBA_am <- data$SBA_am[-c(30,32)]
capacitySBA_pm <- data$SBA_pm[-c(30,32)]
capacitySBA_am4 <- data$SBA_am[-c(30,32)] * 4
capacitySBA_pm4 <- data$SBA_pm[-c(30,32)] * 4


start <- proc.time()

for(i in 1:n)
{
	# randomly draw distributions of duration in delivery and postpartum rooms 
	# for births=annual births in Zanzibar
	durDelUncomp <- rgamma(n=births,shape=shapeDelU,scale=scaleDelU)				#duration in delivery room, uncomplicated delivery
	durDelComp <- factor_dur_comp*rgamma(n=births,shape=shapeDelU,scale=scaleDelU)	#duration in delivery room, complicated delivery

	labour_start <- getDT(N=births, weights=weights)	#each woman set a random date and time of presentation in labour at a health facility
	tz(labour_start) <- timeZone					# set time zone, e.g. to East African Time (for Zanzibar)
	
	# assign a facility for delivery and if delivery is complicated
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
	for(j in 2:nrow(data)) 
	{
		temp5[j] <- temp5[j-1] + 12 * data$no_delivs[j-1]
		facility[temp5[j]:(temp5[j] - 1 + (12*data$no_delivs[j]))] <- j

		# assigning c-sections
		if(data$csects[j]>0)  
		{
			for(k in temp5[j]:(temp5[j] - 1 + (12*data$csects[j])))	
				complicatedZ[k] <- 1
		}
	} 				


	# Duration in the labour ward
	dur_labour <- rep(0,births)			
	dur_labour[complicatedZ==1] <- durDelComp[complicatedZ==1]		# duration in labour ward dependant on whether delivery is complicated
	dur_labour[complicatedZ==0] <- durDelUncomp[complicatedZ==0]	# or uncomplicated
	dur_labour[dur_labour<min_labour] <- min_labour				# and must be minimum of min_labour
	labour_end <- labour_start + dur_labour*60*60				#date and time that delivery ends (dur_labour in hours converted into seconds)

		
	# get occupancy of maternity facilities and delivery rooms every hour
	breaks <- seq(as.POSIXct('2014-01-01 00:00', tz = "GMT"),by = '1 hours', length = 365*24+1)


	# get labour room occupancy
	freqLabourHour <- getOccupancyByCentreSnapshot(labour_start, labour_end, breaks, facility, facility.numbers)
		
	# DETERMINE HOW MUCH OF THE TIME LABOUR ROOMS ARE EMPTY
	hoursLWEmpty.Jan <- colSums(freqLabourHour[1:744, 2:ncol(freqLabourHour)]==0)
	percentTimeLWEmpty.Jan <- hoursLWEmpty.Jan / nrow(freqLabourHour[1:744,]) * 100

	hoursLWEmpty.Jun <- colSums(freqLabourHour[3624:4344, 2:ncol(freqLabourHour)]==0)
	percentTimeLWEmpty.Jun <- hoursLWEmpty.Jun / nrow(freqLabourHour[3624:4344,]) * 100


	# DETERMINE OVERCROWDING IN TERMS OF BEDS
	# which of the above is the right capacity to evaluate? i.e. do births also happen in maternity wards 
	# in centres where there are zero delivery beds?
	hoursLWOverCapacityJan <- colSums(sweep(freqLabourHour[1:744, 2:ncol(freqLabourHour)], 2, capacityLW, ">"))
	percentTimeLWOverCapacityJan <- hoursLWOverCapacityJan / nrow(freqLabourHour[1:744,]) * 100

	hoursLWOverCapacityJun <- colSums(sweep(freqLabourHour[3624:4344, 2:ncol(freqLabourHour)], 2, capacityLW, ">"))
	percentTimeLWOverCapacityJun <- hoursLWOverCapacityJun / nrow(freqLabourHour[3624:4344,]) * 100	


	
	# DETERMINE OVERCROWDING IN TERMS OF SBAs BY am AND pm SHIFTS
	temp.Jan <- freqLabourHour[1:744,]
	temp.Jun <- freqLabourHour[3624:4344,]
	hoursLWOverSBA_am_Jan <- colSums(sweep(temp.Jan[format(temp.Jan$Snapshot, "%p")=="am", 2:ncol(temp.Jan)], 2, capacitySBA_am, ">"))
	hoursLWOverSBA_pm_Jan <- colSums(sweep(temp.Jan[format(temp.Jan$Snapshot, "%p")=="pm", 2:ncol(temp.Jan)], 2, capacitySBA_pm, ">"))
	hoursLWOverSBA_am_Jun <- colSums(sweep(temp.Jun[format(temp.Jun$Snapshot, "%p")=="am", 2:ncol(temp.Jun)], 2, capacitySBA_am, ">"))
	hoursLWOverSBA_pm_Jun <- colSums(sweep(temp.Jun[format(temp.Jun$Snapshot, "%p")=="pm", 2:ncol(temp.Jun)], 2, capacitySBA_pm, ">"))

	hoursLWOverSBAJan <- hoursLWOverSBA_am_Jan + hoursLWOverSBA_pm_Jan 	
	hoursLWOverSBAJun <- hoursLWOverSBA_am_Jun + hoursLWOverSBA_pm_Jun
	

	# DETERMINE OVERCROWDING IN TERMS OF SBAs BY am AND pm SHIFTS (ASSUME 4 WOMEN PER SBA ARE OK)
	hoursLWOverSBA_am4_Jan <- colSums(sweep(temp.Jan[format(temp.Jan$Snapshot, "%p")=="am", 2:ncol(temp.Jan)], 2, capacitySBA_am4, ">"))
	hoursLWOverSBA_pm4_Jan <- colSums(sweep(temp.Jan[format(temp.Jan$Snapshot, "%p")=="pm", 2:ncol(temp.Jan)], 2, capacitySBA_pm4, ">"))
	hoursLWOverSBA_am4_Jun <- colSums(sweep(temp.Jun[format(temp.Jun$Snapshot, "%p")=="am", 2:ncol(temp.Jun)], 2, capacitySBA_am4, ">"))
	hoursLWOverSBA_pm4_Jun <- colSums(sweep(temp.Jun[format(temp.Jun$Snapshot, "%p")=="pm", 2:ncol(temp.Jun)], 2, capacitySBA_pm4, ">"))


	hoursLWOverSBA4Jan <- hoursLWOverSBA_am4_Jan + hoursLWOverSBA_pm4_Jan 	
	hoursLWOverSBA4Jun <- hoursLWOverSBA_am4_Jun + hoursLWOverSBA_pm4_Jun


	# append results to lists
	emptyLW.Jun.ll[[i]] <- percentTimeLWEmpty.Jun
	emptyLW.Jan.ll[[i]] <- percentTimeLWEmpty.Jan

	overLW.Jun.ll[[i]] <- percentTimeLWOverCapacityJun
	overLW.Jan.ll[[i]] <- percentTimeLWOverCapacityJan

	overSBA.Jun.ll[[i]] <- hoursLWOverSBAJun / nrow(freqLabourHour[3624:4344,]) * 100
	overSBA4.Jun.ll[[i]] <- hoursLWOverSBA4Jun / nrow(freqLabourHour[3624:4344,]) * 100

	overSBA.Jan.ll[[i]] <- hoursLWOverSBAJan / nrow(freqLabourHour[1:744,]) * 100
	overSBA4.Jan.ll[[i]] <- hoursLWOverSBA4Jan / nrow(freqLabourHour[1:744,]) * 100
	
}

end <- proc.time() - start
print(end)


df.empty.Jun <- as.data.frame(do.call(rbind, emptyLW.Jun.ll))
df.empty.Jan <- as.data.frame(do.call(rbind, emptyLW.Jan.ll))

df.overLW.Jun <- as.data.frame(do.call(rbind, overLW.Jun.ll))
df.overLW.Jan <- as.data.frame(do.call(rbind, overLW.Jan.ll))

df.overSBA.Jun <- as.data.frame(do.call(rbind, overSBA.Jun.ll))
df.overSBA.Jan <- as.data.frame(do.call(rbind, overSBA.Jan.ll))

df.overSBA4.Jun <- as.data.frame(do.call(rbind, overSBA4.Jun.ll))
df.overSBA4.Jan <- as.data.frame(do.call(rbind, overSBA4.Jan.ll))


df.empty.Jun.long <- gather(df.empty.Jun, Facility, PctTimeLWEmpty, count1:count37, factor_key=TRUE)
levels(df.empty.Jun.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p11max <- ggplot(df.empty.Jun.long, aes(x=Facility, y=PctTimeLWEmpty)) + theme_classic() + geom_boxplot() + 
	ylab("% Time delivery room is empty") + ylim(0, 100) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=11), axis.text.x=element_text(angle=45, hjust=1, vjust=1))
print(p11max)


df.empty.Jan.long <- gather(df.empty.Jan, Facility, PctTimeLWEmpty, count1:count37, factor_key=TRUE)
levels(df.empty.Jan.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p11min <- ggplot(df.empty.Jan.long, aes(x=Facility, y=PctTimeLWEmpty)) + theme_classic() + geom_boxplot() + 
	ylab("% Time delivery room is empty") + ylim(0, 100) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=11), axis.text.x=element_text(angle=45, hjust=1, vjust=1))
print(p11min)


df.overSBA.Jun.long <- gather(df.overSBA.Jun, Facility, PctTimeOverSBACapacity, count1:count37, factor_key=TRUE)
levels(df.overSBA.Jun.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p12max <- ggplot(df.overSBA.Jun.long, aes(x=Facility, y=PctTimeOverSBACapacity)) + theme_classic() + geom_boxplot() + 
	ylab("% Time when more women than SBAs in delivery room") + ylim(0, 50) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1))
print(p12max)


df.overSBA.Jan.long <- gather(df.overSBA.Jan, Facility, PctTimeOverSBACapacity, count1:count37, factor_key=TRUE)
levels(df.overSBA.Jan.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p12min <- ggplot(df.overSBA.Jan.long, aes(x=Facility, y=PctTimeOverSBACapacity)) + theme_classic() + geom_boxplot() + 
	ylab("% Time when more women than SBAs in delivery room") + ylim(0, 50) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1))
print(p12min)


df.overLW.Jun.long <- gather(df.overLW.Jun, Facility, PctTimeOverLWCapacity, count1:count37, factor_key=TRUE)
levels(df.overLW.Jun.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p13max <- ggplot(df.overLW.Jun.long, aes(x=Facility, y=PctTimeOverLWCapacity)) + theme_classic() + geom_boxplot() + 
	ylab("% Time bed capacity in delivery room exceeded") + #ylim(0, 17) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_blank())
print(p13max)


df.overLW.Jan.long <- gather(df.overLW.Jan, Facility, PctTimeOverLWCapacity, count1:count37, factor_key=TRUE)
levels(df.overLW.Jan.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p13min <- ggplot(df.overLW.Jan.long, aes(x=Facility, y=PctTimeOverLWCapacity)) + theme_classic() + geom_boxplot() + 
	ylab("% Time bed capacity in delivery room exceeded") + #ylim(0, 17) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_blank())
print(p13min)



pic <- plot_grid(p13max, p11max, p12max, labels=c('a', 'b', 'c'), nrow=3)
save_plot(filename="capacityLW100_TNZseason_Jun.png", plot=pic, base_height=18, base_width=15, dpi=300)


pic <- plot_grid(p13min, p11min, p12min, labels=c('a', 'b', 'c'), nrow=3)
save_plot(filename="capacityLW100_TNZseason_Jan.png", plot=pic, base_height=18, base_width=15, dpi=300)



df.overSBA4.Jun.long <- gather(df.overSBA4.Jun, Facility, PctTimeOverSBACapacity, count1:count37, factor_key=TRUE)
levels(df.overSBA4.Jun.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p14max <- ggplot(df.overSBA4.Jun.long, aes(x=Facility, y=PctTimeOverSBACapacity)) + theme_classic() + geom_boxplot() + 
	ylab("% Time when > 4 women per SBA in delivery room") + ylim(0, 50) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1))
print(p14max)


df.overSBA4.Jan.long <- gather(df.overSBA4.Jan, Facility, PctTimeOverSBACapacity, count1:count37, factor_key=TRUE)
levels(df.overSBA4.Jan.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p14min <- ggplot(df.overSBA4.Jan.long, aes(x=Facility, y=PctTimeOverSBACapacity)) + theme_classic() + geom_boxplot() + 
	ylab("% Time when > 4 women per SBA in delivery room") + ylim(0, 50) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1))
print(p14min)


pic <- plot_grid(p13max, p11max, p14max, labels=c('a', 'b', 'c'), nrow=3)
save_plot(filename="capacityLW100.4_TNZseason_Jun.png", plot=pic, base_height=18, base_width=15, dpi=300)

pic <- plot_grid(p13min, p11min, p14min, labels=c('a', 'b', 'c'), nrow=3)
save_plot(filename="capacityLW100.4_TNZseason_Jan.png", plot=pic, base_height=18, base_width=15, dpi=300)




