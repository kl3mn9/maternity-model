###################################################################
# BECKY BAGGALEY, SEPTEMBER 2020, LENGTH OF STAY MODEL - ZANZIBAR
# CODE TO DETERMINE DELIVERY ROOM AND MATERNITY WARD OCCUPANCY
# IN ZANZIBAR (USING ZANZIBAR-SPECIFIC MATERNITY WARD DATA) 
# AND IN SUB-SAHARAN AFRICA IN GENERAL USING WHO DATA
# EDITS AND REFACTORING BY CAROLIN VEGVARI 16/05/2022
# EDITS BY CAROLIN VEGVARI 07/11/2020
# USE LOS DISTRIBUTIONS FITTED TO LATEST TANZANIA DHS DATA - ZANZIBAR ONLY
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
library(foreach)		# for parallel for loops
library(doParallel)	# for parallel for loops
library(doRNG)		# for parallel for loops with random number seed

# set working directory
setwd("C:\\Users\\Carolin\\Oriole Global Health\\OGH Team - Documents\\02. Business Development\\03. Business Outreach\\MaternityModel\\ZanzibarMaternity\\")

seed <- 42
set.seed(seed)		#for reproducibility

######################
###Model parameters###		# CV: consider moving to parameter file to make script file general to any country
######################
timeZone 		<- "Africa/Dar_es_Salaam"
prob_comp		<- 0.15	# % deliveries with complications
min_labour		<- 0.25	# minimum duration in labour ward (hours)
min_postp		<- 2		# minimum duration in postpartum ward (hours)
factor_dur_comp	<- 1.5	# factor increase in duration in labour ward if delivery is complicated

###LENGTHS OF STAY PARAMETERS###
###POST PARTUM###
### For Zanzibar - Tanzania DHS###
shapePPUZ <- 1.114		#shape parameter, uncomplicated
scalePPUZ <- 12.08		#scale parameter, uncomplicated
shapePPCZ <- 2.023		#shape parameter, complicated
scalePPCZ <- 57.8			#scale parameter, complicated

###DELIVERY ROOM###
###Lack of data, so same for WHO and Zanzibar###
shapeDelU <- 3.37 		#shape parameter, uncomplicated
scaleDelU <- 1.23 		#scale parameter, uncomplicated

###Read in Zanzibar facilities data###
# contains data on births in 37 facilities in Zanzibar
data <- read.table("Zanzibar facility dataset.csv", header=T, sep=",", fill=TRUE)

df.fac <- data[!(data$no_delivs==0),] 						# deleting facilities 30 and 32 because there are zero deliveries in the dataset
df.fac <- df.fac[order(df.fac$csects, decreasing=TRUE), ]	# order facilities according to number of c-sections
row.names(df.fac) <- 1:nrow(df.fac)

#births <- 12*sum(data$no_delivs)			# estimated no. births per year in facilities in Zanzibar
births <- 48763						# births in Zanzibar in 2013, source: knoema

# Calculate probability of complicated births based on data
#prob_comp <- sum(df.fac$csects) / sum(df.fac$no_delivs)		# % c-sections in Zanzibar data
prob_comp <- 0.15 										# % deliveries with complications, assumed in World Health Report

# CALCULATE PROBABILITIES FOR EACH FACILITY
probs_fac_comp <- df.fac$no_delivs[df.fac$csects > 0] / sum(df.fac$no_delivs[df.fac$csects > 0])
probs_fac_uncomp <- (df.fac$no_delivs - df.fac$csects) / sum(df.fac$no_delivs - df.fac$csects)


# weight months by number of births per month from TANZANIA DHS data for ZANZIBAR
weights <- read.table("ZNZ_seasonality_weights.txt", header=FALSE)[,1]


# create parameter list to be passed to parallel for loops
params <- list(timeZone=timeZone, prob_comp=prob_comp, min_labour=min_labour, min_postp=min_postp,
		factor_dur_comp=factor_dur_comp, births=births, prob_comp=prob_comp,
		probs_fac_comp=probs_fac_comp, probs_fac_uncomp=probs_fac_uncomp,
		shapePPUZ=shapePPUZ, scalePPUZ=scalePPUZ, shapePPCZ=shapePPCZ, scalePPCZ=scalePPCZ,
		shapeDelU=shapeDelU, scaleDelU=scaleDelU)


###########################################################################
# convenience functions
###########################################################################

#Random date and time function taken from stackoverflow
#http://stackoverflow.com/questions/14720983/efficiently-generate-a-random-sample-of-times-and-dates-between-two-dates

latemail <- function(N, st="2013/01/01", et="2013/12/31") {
	st <- as.POSIXct(as.Date(st))
      et <- as.POSIXct(as.Date(et))
      difft <- as.numeric(difftime(et,st,unit="sec"))
      ev <- sort(runif(N, 0, difft))
     rt <- st + ev
 }


getDT <- function(N, weights, start="2013/01/01")
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

# function to determine occupancy of different wards/rooms depending on start time, finish entered
# combine functions event.array, intervals.array and getOccupancy so that function can be passed 
# as part of transporter to parallel for loops
getOccupancyByCentreSnapshot2 <- function(start, finish, breaks, facility, facility.numbers)
{ 	
	# generate matrices of events
	len1 <- length(breaks)
	a.start <- matrix(rep(unclass(start), len1), ncol=len1)
	a.finish <- matrix(rep(unclass(finish), len1), ncol=len1)

	# generate matrix of time intervals
	len2 <- length(start)
 	a.intervals <- matrix(rep(unclass(breaks), len2), nrow=len2, byrow=TRUE)

	freq <- breaks
	for(facility.number in facility.numbers)
	{
		count <- colSums(a.start[facility==facility.number,] <= a.intervals[facility==facility.number,] & a.finish[facility==facility.number,] >= a.intervals[facility==facility.number,])
		freq <- cbind(freq, count)	
	} 

	freq <- as.data.frame(freq)
	names(freq) <- c("Snapshot", paste0("count", facility.numbers))
	freq$Snapshot <- breaks

	return(freq)
}



#########################################
###START OF SIMULATION###################
#########################################

#generating distributions - duration in delivery and postpartum rooms, Zanzibar 
durDelUncomp <- rgamma(n=births,shape=shapeDelU,scale=scaleDelU)				# duration in delivery room, uncomplicated delivery
durDelComp <- factor_dur_comp*rgamma(n=births,shape=shapeDelU,scale=scaleDelU)	# duration in delivery room, complicated delivery

durPPUZ <- rgamma(n=births,shape=shapePPUZ,scale=scalePPUZ)					# duration postpartum, uncomplicated delivery, Zanzibar analysis
durPPCZ <- rgamma(n=births,shape=shapePPCZ,scale=scalePPCZ)					# duration postpartum, complicated delivery, Zanzibar analysis


labour_start <- getDT(N=births, weights=weights)	#each woman set a random date and time of presentation in labour at a health facility
tz(labour_start) <- timeZone					# set time zone, e.g. to East African Time (for Zanzibar)	

complicatedZ <- rbinom(births, 1, prob_comp)		# assign whether each woman has a complicated delivery: 1= complicated 0= uncomplicated


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

dur_maternityZ <- rep(0,births)	# duration in maternity ward according to Zanzibar data	

# duration in maternity ward dependant on whether delivery is complicated
dur_maternityZ[complicatedZ==1] <- durPPCZ[complicatedZ==1]
dur_maternityZ[complicatedZ==0] <- durPPUZ[complicatedZ==0]

discharge1Z <- labour_end + dur_maternityZ*60*60


################################################################################
# ZANZIBAR SCENARIO - ASSIGN FACILITIES
# All complicated births happen in CEmOC - only 7 facilities in Zanzibar data
# report complicated births (C-sections), remaining facilities are assumed to
# be BEmOCs - no complicated births reported in data
################################################################################

facility <- rep(0, births)
# assign complicated births to facilities
facility[complicatedZ==1] <- sample(1:nrow(df.fac[df.fac$csects>0, ]), size=length(facility[complicatedZ==1]), prob=probs_fac_comp, replace=TRUE)
# assign uncomplicated births to facilities
facility[complicatedZ==0] <- sample(1:nrow(df.fac), size=length(facility[complicatedZ==0]), prob=probs_fac_uncomp, replace=TRUE)


#################################################################################################################################
# OCCUPANCY AT SNAPSHOTS IN TIME (4:00, 12:00, 20:00)
#################################################################################################################################

# times to generate 'snapshots', 
# i.e. times at which to establish delivery room and maternity ward occupancy
# 3 snapshots per day over a year
breaks <- seq(as.POSIXct('2013-01-01 00:00', tz = timeZone), by = '1 hours', length = 365*24+1)
temp <- format(breaks,"%H:%M:%S")
breaks3 <- breaks[temp=="04:00:00" | temp=="12:00:00" |temp=="18:00:00"]


# all births 
dischargeTime <- getDischargeTime(discharge1Z)
tz(dischargeTime) <- timeZone

facility.numbers <- 1:nrow(df.fac)

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

# only consider facilities carrying out c-sections
facility.numbers.c <- facility.numbers[df.fac$csects > 0]
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
#ggsave("TotalBirthsYear_ZNZ.png", plot=p1, dpi=300, width=12, height=8)


p1a <- ggplot(df.total.snaps.long.Mar, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a month") + ylab("Number of women in EmOCs") + 
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=16))
print(p1a)
#ggsave("TotalBirthsMarch_ZNZ.png", plot=p1a, dpi=300, width=12, height=8)


# Plot number of births and complicated births over a year by facility (snapshots, facility occupancy)
freqZ.long <- gather(freqZ, Facility, NumBirths, count1:count35)
freqZ_comp.long <- gather(freqZ_comp, Facility, CompBirths, count1:count7)
df.fac.snaps <- merge(freqZ.long, freqZ_comp.long, by=c("Snapshot", "Facility"), all=TRUE)
df.fac.snaps <- df.fac.snaps[order(df.fac.snaps$Facility, df.fac.snaps$Snapshot), ]
df.fac.snaps$CompBirths[is.na(df.fac.snaps$CompBirths)] <- 0
df.fac.snaps$UncompBirths <- df.fac.snaps$NumBirths - df.fac.snaps$CompBirths
df.fac.snaps.long <- gather(df.fac.snaps[, -3], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

fac.levels <- paste0("count", 1:35)
df.fac.snaps.long$Facility <- factor(df.fac.snaps.long$Facility, levels=fac.levels) 
df.fac.snaps.long <- df.fac.snaps.long[order(df.fac.snaps.long$Facility, df.fac.snaps.long$Snapshot), ]
levels(df.fac.snaps.long$Facility) <- paste("Facility", seq(1, 35))

p2 <- ggplot(df.fac.snaps.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a year") + ylab("Number of women in EmOCs") + facet_wrap(~ Facility, ncol=5) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
print(p2)
#ggsave("BirthsFacilityYear_ZNZ.png", plot=p2, dpi=300, width=18, height=18)


# Plot number of births and complicated births over a representative month (March) by facility (snapshots, facility occupancy)
freqZ.long.Mar <- gather(freqZ[178:270,], Facility, NumBirths, count1:count35)
freqZ_comp.long.Mar <- gather(freqZ_comp[178:270,], Facility, CompBirths, count1:count7)
df.fac.snaps <- merge(freqZ.long.Mar, freqZ_comp.long.Mar, by=c("Snapshot", "Facility"), all=TRUE)
df.fac.snaps <- df.fac.snaps[order(df.fac.snaps$Facility, df.fac.snaps$Snapshot), ]
df.fac.snaps$CompBirths[is.na(df.fac.snaps$CompBirths)] <- 0
df.fac.snaps$UncompBirths <- df.fac.snaps$NumBirths - df.fac.snaps$CompBirths
df.fac.snaps.long <- gather(df.fac.snaps[, -3], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

fac.levels <- paste0("count", 1:35)
df.fac.snaps.long$Facility <- factor(df.fac.snaps.long$Facility, levels=fac.levels) 
df.fac.snaps.long <- df.fac.snaps.long[order(df.fac.snaps.long$Facility, df.fac.snaps.long$Snapshot), ]
levels(df.fac.snaps.long$Facility) <- paste("Facility", seq(1, 35))

p2a <- ggplot(df.fac.snaps.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a year") + ylab("Number of women in EmOCs") + facet_wrap(~ Facility, ncol=5) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
print(p2a)
#ggsave("BirthsFacilityMarch_ZNZ.png", plot=p2a, dpi=300, width=18, height=18)



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
ggsave("TotalDelYear_ZNZ.png", plot=p3, dpi=300, width=12, height=8)


p3a <- ggplot(df.del.snaps.long.Mar, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a year") + ylab("Number of women in delivery rooms") + 
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=16))
print(p3a)
ggsave("TotalDelMarch_ZNZ.png", plot=p3a, dpi=300, width=12, height=8)


# Plot number of births and complicated births over a year by facility (snapshots, facility occupancy)
freqLabourZ.long <- gather(freqLabourZ, Facility, NumBirths, count1:count35)
freqLabourZ_comp.long <- gather(freqLabourZ_comp, Facility, CompBirths, count1:count7)
df.del.fac.snaps <- merge(freqLabourZ.long, freqLabourZ_comp.long, by=c("Snapshot", "Facility"), all=TRUE)
df.del.fac.snaps <- df.del.fac.snaps[order(df.del.fac.snaps$Facility, df.del.fac.snaps$Snapshot), ]
df.del.fac.snaps$CompBirths[is.na(df.del.fac.snaps$CompBirths)] <- 0
df.del.fac.snaps$UncompBirths <- df.del.fac.snaps$NumBirths - df.del.fac.snaps$CompBirths
df.del.fac.snaps.long <- gather(df.del.fac.snaps[, -3], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

fac.levels <- paste0("count", 1:35)
df.del.fac.snaps.long$Facility <- factor(df.del.fac.snaps.long$Facility, levels=fac.levels) 
df.del.fac.snaps.long <- df.del.fac.snaps.long[order(df.del.fac.snaps.long$Facility, df.del.fac.snaps.long$Snapshot), ]
levels(df.del.fac.snaps.long$Facility) <- paste("Facility", seq(1, 35))

p4 <- ggplot(df.del.fac.snaps.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a year") + ylab("Number of women in delivery rooms") + facet_wrap(~ Facility, ncol=5) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
print(p4)
#ggsave("DelFacilityYear_ZNZ.png", plot=p4, dpi=300, width=18, height=18)


# Plot number of births and complicated births over a representative month (March) by facility (snapshots, facility occupancy)
freqLabourZ.long <- gather(freqLabourZ[178:270,], Facility, NumBirths, count1:count35)
freqLabourZ_comp.long <- gather(freqLabourZ_comp[178:270,], Facility, CompBirths, count1:count7)
df.del.fac.snaps <- merge(freqLabourZ.long, freqLabourZ_comp.long, by=c("Snapshot", "Facility"), all=TRUE)
df.del.fac.snaps <- df.del.fac.snaps[order(df.del.fac.snaps$Facility, df.del.fac.snaps$Snapshot), ]
df.del.fac.snaps$CompBirths[is.na(df.del.fac.snaps$CompBirths)] <- 0
df.del.fac.snaps$UncompBirths <- df.del.fac.snaps$NumBirths - df.del.fac.snaps$CompBirths
df.del.fac.snaps.long <- gather(df.del.fac.snaps[, -3], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

fac.levels <- paste0("count", 1:35)
df.del.fac.snaps.long$Facility <- factor(df.del.fac.snaps.long$Facility, levels=fac.levels) 
df.del.fac.snaps.long <- df.del.fac.snaps.long[order(df.del.fac.snaps.long$Facility, df.del.fac.snaps.long$Snapshot), ]
levels(df.del.fac.snaps.long$Facility) <- paste("Facility", seq(1, 35))

p4a <- ggplot(df.del.fac.snaps.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a year") + ylab("Number of women in delivery rooms") + facet_wrap(~ Facility, ncol=5) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
print(p4a)
ggsave("DelFacilityMarch_ZNZ.png", plot=p4a, dpi=300, width=18, height=18)



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
ggsave("TotalMatYear_ZNZ.png", plot=p5, dpi=300, width=12, height=8)


# Plot number of births and complicated births over a representative month by facility (snapshots, facility occupancy)
#freqMatZ.long <- gather(freqMatZ[178:270,], Facility, NumBirths, count1:count35)
#freqMatZ_comp.long <- gather(freqMatZ_comp[178:270,], Facility, CompBirths, count1:count7)
freqMatZ.long <- gather(freqMatZ, Facility, NumBirths, count1:count35)
freqMatZ_comp.long <- gather(freqMatZ_comp, Facility, CompBirths, count1:count7)
df.mat.fac.snaps <- merge(freqMatZ.long, freqMatZ_comp.long, by=c("Snapshot", "Facility"), all=TRUE)
df.mat.fac.snaps <- df.mat.fac.snaps[order(df.mat.fac.snaps$Facility, df.mat.fac.snaps$Snapshot), ]
df.mat.fac.snaps$CompBirths[is.na(df.mat.fac.snaps$CompBirths)] <- 0
df.mat.fac.snaps$UncompBirths <- df.mat.fac.snaps$NumBirths - df.mat.fac.snaps$CompBirths
df.mat.fac.snaps.long <- gather(df.mat.fac.snaps[, -3], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

fac.levels <- paste0("count", 1:35)
df.mat.fac.snaps.long$Facility <- factor(df.mat.fac.snaps.long$Facility, levels=fac.levels) 
df.mat.fac.snaps.long <- df.mat.fac.snaps.long[order(df.mat.fac.snaps.long$Facility, df.mat.fac.snaps.long$Snapshot), ]
levels(df.mat.fac.snaps.long$Facility) <- paste("Facility", seq(1, 35))

p6 <- ggplot(df.mat.fac.snaps.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a year") + ylab("Number of women in post-partum beds") + facet_wrap(~ Facility, ncol=5) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
print(p6)
ggsave("MatFacilityYear_ZNZ.png", plot=p6, dpi=300, width=18, height=18)



#################################################################################################################################
# PROPORTION OF TIME EACH FACILITY IS OVER CAPACITY OR EMPTY --> EVALUATE CAPACITY EVERY HOUR RATHER THAN EVERY 8 HOURS
#################################################################################################################################

# repeat n times to get mean and 95% credible interval
n <- 1000

# Bed capacity limits
capacityLW <- df.fac$beds_deliv
# Where there are 0 delivery beds reported assume that each theatre has at least one bed		
capacityLW[capacityLW==0] <- df.fac$theatre[df.fac$beds_deliv==0]

# SBA capacity limits
capacitySBA_am <- df.fac$SBA_am
capacitySBA_pm <- df.fac$SBA_pm
capacitySBA_am4 <- df.fac$SBA_am * 4
capacitySBA_pm4 <- df.fac$SBA_pm * 4

params$capacityLW <- capacityLW
params$capacitySBA_am <- capacitySBA_am
params$capacitySBA_pm <- capacitySBA_pm
params$capacitySBA_am4 <- capacitySBA_am4
params$capacitySBA_pm4 <- capacitySBA_pm4

params$df.fac <- df.fac


# FUNCTION AND DATA TRANSPORTER FOR PARALLEL CODE
FT <- list(getDT=match.fun(getDT), getDischargeTime=match.fun(getDischargeTime),  
	getOccupancyByCentreSnapshot2=match.fun(getOccupancyByCentreSnapshot2))

breaks <- seq(as.POSIXct('2013-03-01 00:00', tz = "GMT"),by = '1 hours', length = 31*24+1)


# function to be called in parallel for loop
calcFacStats <- function(params, FT, breaks)
{
	with(params, {

	# randomly draw distributions of duration in delivery and postpartum rooms 
	# for births=annual births in Zanzibar
	durDelUncomp <- rgamma(n=births,shape=shapeDelU,scale=scaleDelU)				#duration in delivery room, uncomplicated delivery
	durDelComp <- factor_dur_comp*rgamma(n=births,shape=shapeDelU,scale=scaleDelU)	#duration in delivery room, complicated delivery
	durPPUZ <- rgamma(n=births,shape=shapePPUZ,scale=scalePPUZ)					#duration postpartum, uncomplicated delivery, Zanzibar analysis
	durPPCZ <- rgamma(n=births,shape=shapePPCZ,scale=scalePPCZ)					#duration postpartum, complicated delivery, Zanzibar analysis

	labour_start <- getDT(N=births, weights=weights)	#each woman set a random date and time of presentation in labour at a health facility
	tz(labour_start) <- timeZone					# set time zone, e.g. to East African Time (for Zanzibar)
	
	complicatedZ <- rbinom(births, 1, prob_comp)	# assign whether each woman has a complicated delivery: 1= complicated 0= uncomplicated


	# assign a facility for delivery for complicated and uncomplicated births
	facility <- rep(0, births)
	# assign complicated births to facilities
	facility[complicatedZ==1] <- sample(1:nrow(df.fac[df.fac$csects>0, ]), size=length(facility[complicatedZ==1]), prob=probs_fac_comp, replace=TRUE)
	# assign uncomplicated births to facilities
	facility[complicatedZ==0] <- sample(1:nrow(df.fac), size=length(facility[complicatedZ==0]), prob=probs_fac_uncomp, replace=TRUE)


	# Duration in the labour ward
	dur_labour <- rep(0,births)			
	dur_labour[complicatedZ==1] <- durDelComp[complicatedZ==1]		# duration in labour ward dependant on whether delivery is complicated
	dur_labour[complicatedZ==0] <- durDelUncomp[complicatedZ==0]	# or uncomplicated
	dur_labour[dur_labour<min_labour] <- min_labour				# and must be minimum of min_labour
	labour_end <- labour_start + dur_labour*60*60				#date and time that delivery ends (dur_labour in hours converted into seconds)

	
	facility.numbers <- 1:nrow(df.fac)
	# get labour room occupancy
	freqLabourZHour <- getOccupancyByCentreSnapshot(labour_start, labour_end, breaks, facility, facility.numbers)
	
	
	# DETERMINE OVERCROWDING IN TERMS OF BEDS
	hoursLWOverCapacity <- colSums(sweep(freqLabourZHour[, 2:ncol(freqLabourZHour)], 2, capacityLW, ">"))
	percentTimeLWOverCapacity <- hoursLWOverCapacity / nrow(freqLabourZHour) * 100
	
	
	# DETERMINE HOW MUCH OF THE TIME LABOUR ROOMS ARE EMPTY
	hoursLWEmpty <- colSums(freqLabourZHour[, 2:ncol(freqLabourZHour)]==0)
	percentTimeLWEmpty <- hoursLWEmpty / nrow(freqLabourZHour) * 100


	# DETERMINE OVERCROWDING IN TERMS OF SBAs BY am AND pm SHIFTS
	hoursLWOverSBA_am <- colSums(sweep(freqLabourZHour[format(freqLabourZHour$Snapshot, "%p")=="am", 2:ncol(freqLabourZHour)], 2, capacitySBA_am, ">"))
	hoursLWOverSBA_pm <- colSums(sweep(freqLabourZHour[format(freqLabourZHour$Snapshot, "%p")=="pm", 2:ncol(freqLabourZHour)], 2, capacitySBA_pm, ">"))

	hoursLWOverSBA <- hoursLWOverSBA_am + hoursLWOverSBA_pm 
	percentLWOverSBA <- hoursLWOverSBA / nrow(freqLabourZHour) * 100

	# DETERMINE OVERCROWDING IN TERMS OF SBAs BY am AND pm SHIFTS (ASSUME 4 WOMEN PER SBA ARE OK)
	hoursLWOverSBA_am4 <- colSums(sweep(freqLabourZHour[format(freqLabourZHour$Snapshot, "%p")=="am", 2:ncol(freqLabourZHour)], 2, capacitySBA_am4, ">"))
	hoursLWOverSBA_pm4 <- colSums(sweep(freqLabourZHour[format(freqLabourZHour$Snapshot, "%p")=="pm", 2:ncol(freqLabourZHour)], 2, capacitySBA_pm4, ">"))

	hoursLWOverSBA4 <- hoursLWOverSBA_am4 + hoursLWOverSBA_pm4 
	percentLWOverSBA4 <- hoursLWOverSBA4 / nrow(freqLabourZHour) * 100

	res <- list(percentTimeLWOverCapacity=percentTimeLWOverCapacity, percentTimeLWEmpty=percentTimeLWEmpty,
			percentLWOverSBA=percentLWOverSBA, percentLWOverSBA4=percentLWOverSBA4)

	return(res)

	})

}


start <- proc.time()

# set up cluster
cl <- makeCluster(8)
registerDoParallel(cl)
registerDoRNG(seed=seed)

# call parallel for loop
foreachResults <- foreach(i=1:n, .packages=c("lubridate")) %dorng% calcFacStats(params, FT, breaks)
#res <- calcFacStats(params,FT, breaks)

# end cluster
stopImplicitCluster()

end <- proc.time() - start
print(end)


overLW.ll <- list()
emptyLW.ll <- list()
overSBA.ll <- list()
overSBA4.ll <- list()

for(i in 1:length(foreachResults))
{
	# append results to lists
	overLW.ll[[i]] <- foreachResults[[i]]$percentTimeLWOverCapacity
	emptyLW.ll[[i]] <- foreachResults[[i]]$percentTimeLWEmpty
	overSBA.ll[[i]] <- foreachResults[[i]]$percentLWOverSBA
	overSBA4.ll[[i]] <- foreachResults[[i]]$percentLWOverSBA4
}

df.overLW <- as.data.frame(do.call(rbind, overLW.ll))
df.emptyLW <- as.data.frame(do.call(rbind, emptyLW.ll))
df.overSBA <- as.data.frame(do.call(rbind, overSBA.ll))
df.overSBA4 <- as.data.frame(do.call(rbind, overSBA4.ll))

# save output data
#write.csv(x=df.overLW, file="pctTimeExceedLWCapacity_ZNZseason.csv", row.names=FALSE)
#write.csv(x=df.emptyLW, file="pctTimeEmptyLW_ZNZseason.csv", row.names=FALSE)
#write.csv(x=df.overSBA, file="pctTimeExceedSBACapacity_ZNZseason.csv", row.names=FALSE)
#write.csv(x=df.overSBA4, file="pctTimeExceedSBA4Capacity_ZNZseason.csv", row.names=FALSE)


#################################################################################################################################
# PLOT PERCENT OF TIME DURING WHICH FACILITIES EXCEED CAPACITY OR ARE EMPTY
#################################################################################################################################

df.overLW <- read.csv("ZanzibarScenario_ZNZ_DHS_Seasonal\\1000iter\\pctTimeExceedLWCapacity_ZNZseason.csv")
df.emptyLW <- read.csv("ZanzibarScenario_ZNZ_DHS_Seasonal\\1000iter\\pctTimeEmptyLW_ZNZseason.csv")
df.overSBA <- read.csv("ZanzibarScenario_ZNZ_DHS_Seasonal\\1000iter\\pctTimeExceedSBACapacity_ZNZseason.csv")
df.overSBA4 <- read.csv("ZanzibarScenario_ZNZ_DHS_Seasonal\\1000iter\\pctTimeExceedSBA4Capacity_ZNZseason.csv")


df.overLW.long <- gather(df.overLW, Facility, PctTimeOverLWCapacity, count1:count35, factor_key=TRUE)
levels(df.overLW.long$Facility) <- paste("Facility", seq(1, 35))
df.overLW.long$Type <- rep("BEmOC", nrow(df.overLW.long))
df.overLW.long$Type[df.overLW.long$Facility%in%paste("Facility", 1:7)] <- "CEmOC"

p7 <- ggplot(df.overLW.long, aes(x=Facility, y=PctTimeOverLWCapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time bed capacity in delivery room exceeded") + ylim(0, 17) +
	scale_fill_manual(values=c("lightgrey", "grey27")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_blank(), legend.position=c(0.8, 0.8), legend.title=element_blank(), legend.text=element_text(size=14), plot.margin=margin(0, 0, 0, 1, "cm"))
print(p7)
#ggsave("bedCapacityExceeded1_ZNZseason.png", plot=p7, dpi=300, width=18, height=9)

df.emptyLW.long <- gather(df.emptyLW, Facility, PctTimeLWEmpty, count1:count35, factor_key=TRUE)
levels(df.emptyLW.long$Facility) <- paste("Facility", seq(1, 35))
df.emptyLW.long$Type <- rep("BEmOC", nrow(df.emptyLW.long))
df.emptyLW.long$Type[df.emptyLW.long$Facility%in%paste("Facility", 1:7)] <- "CEmOC"

p8 <- ggplot(df.emptyLW.long, aes(x=Facility, y=PctTimeLWEmpty, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time delivery room is empty") + ylim(0, 100) +
	scale_fill_manual(values=c("lightgrey", "grey27")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_blank(), legend.position="none", plot.margin=margin(0, 0, 0, 1, "cm"))
print(p8)
#ggsave("emptyLW100_ZNZseason.png", plot=p8, dpi=300, width=18, height=9)


df.overSBA.long <- gather(df.overSBA, Facility, PctTimeOverSBACapacity, count1:count35, factor_key=TRUE)
levels(df.overSBA.long$Facility) <- paste("Facility", seq(1, 35))
df.overSBA.long$Type <- rep("BEmOC", nrow(df.emptyLW.long))
df.overSBA.long$Type[df.overSBA.long$Facility%in%paste("Facility", 1:7)] <- "CEmOC"

p9 <- ggplot(df.overSBA.long, aes(x=Facility, y=PctTimeOverSBACapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time when more women than SBAs in delivery room") + ylim(0, 70) +
	scale_fill_manual(values=c("lightgrey", "grey27")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1), legend.position="none", plot.margin=margin(0, 0, 0, 1, "cm"))
print(p9)
#ggsave("SBACapacityExceeded100_ZNZseason.png", plot=p9, dpi=300, width=18, height=9)

pic <- plot_grid(p7, p8, p9, labels=c('a', 'b', 'c'), label_size=16, nrow=3)
#save_plot(filename="capacityLW_ZNZseason.png", plot=pic, base_height=16, base_width=12, dpi=300)


df.overSBA4.long <- gather(df.overSBA4, Facility, PctTimeOverSBACapacity, count1:count35, factor_key=TRUE)
levels(df.overSBA4.long$Facility) <- paste("Facility", seq(1, 35))
df.overSBA4.long$Type <- rep("BEmOC", nrow(df.emptyLW.long))
df.overSBA4.long$Type[df.overSBA4.long$Facility%in%paste("Facility", 1:7)] <- "CEmOC"

p9alt <- ggplot(df.overSBA4.long, aes(x=Facility, y=PctTimeOverSBACapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time when >4 women per SBA in delivery room") + ylim(0, 70) +
	scale_fill_manual(values=c("lightgrey", "grey27")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1), legend.position="none", plot.margin=margin(0, 0, 0, 1, "cm"))
print(p9alt)

pic <- plot_grid(p7, p8, p9alt, labels=c('a', 'b', 'c'), label_size=16, nrow=3)
#save_plot(filename="capacityLWSBA4_ZNZseason.png", plot=pic, base_height=16, base_width=12, dpi=300)


#################################################################################################################################
# AGGREGATE PLOTS BY CEmOCs and BEmOCs
#################################################################################################################################


p7sum <- ggplot(df.overLW.long, aes(x=Type, y=PctTimeOverLWCapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time bed capacity in delivery room exceeded") + ylim(0, 100) +
	scale_fill_manual(values=c("lightgrey", "darkgrey")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=14), legend.position="none")
print(p7sum)


p8sum <- ggplot(df.emptyLW.long, aes(x=Type, y=PctTimeLWEmpty, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time delivery room is empty") + ylim(0, 100) +
	scale_fill_manual(values=c("lightgrey", "darkgrey")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=14), legend.position="none")
print(p8sum)


p9sum <- ggplot(df.overSBA.long, aes(x=Type, y=PctTimeOverSBACapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time when more women than SBAs in delivery room") + ylim(0, 100) +
	scale_fill_manual(values=c("lightgrey", "darkgrey")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=14), legend.position="none")
print(p9sum)

p9sumalt <- ggplot(df.overSBA4.long, aes(x=Type, y=PctTimeOverSBACapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time when >4 women per SBA in delivery room") + ylim(0, 100) +
	scale_fill_manual(values=c("lightgrey", "darkgrey")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=14), legend.position="none")
print(p9sumalt)



#################################################################################################################################
# PLOT BIRTHS PER SBA BY FACILITY 
#################################################################################################################################

# DETERMINE BIRTHS PER SBA PER YEAR
# assuming SBAs are not shared among facilities
birthsPerSBA <- as.vector(table(facility) / df.fac$SBA_total)
df.birthsPerSBA <- data.frame(Facility=paste("Facility", seq(1, 35)), BirthsPerSBA=birthsPerSBA)
df.birthsPerSBA$Facility <- factor(df.birthsPerSBA$Facility, levels=paste("Facility", seq(1, 35)))
df.birthsPerSBA$Type <- c(rep("CEmOC", 7), rep("BEmOC", 28))
birthsRequired <- 175

p10 <- ggplot(df.birthsPerSBA, aes(x=Facility, y=BirthsPerSBA, fill=Type)) + theme_classic() + geom_col(colour="black") +
	geom_hline(yintercept=birthsRequired, colour="red", size=1.5) + ylab("Births per SBA per year") +
	scale_fill_manual(values=c("lightgrey", "darkgrey")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1), legend.title=element_blank(), legend.text=element_text(size=12))
print(p10)
#ggsave("birthsPerSBA_ZNZseason.png", plot=p10, dpi=300, width=18, height=9)


p11 <- ggplot(df.birthsPerSBA, aes(x=Type, y=BirthsPerSBA, fill=Type)) + theme_classic() + geom_boxplot(colour="black") +
	scale_fill_manual(values=c("lightgrey", "darkgrey")) + ylab("Births per SBA per year") +
	geom_hline(yintercept=birthsRequired, colour="red", size=1.5) + 
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=14), legend.position="none")
print(p11)
#ggsave("birthsPerSBASummary_ZNZseason.png", plot=p11, dpi=300, width=9, height=9)


pic <- plot_grid(p7sum, p8sum, p9sum, p11, labels=c('a', 'b', 'c', 'd'), label_size=16, nrow=2)
#save_plot(filename="capacityLWsum_ZNZseason.png", plot=pic, base_height=12, base_width=12, dpi=300)

pic <- plot_grid(p7sum, p8sum, p9sumalt, p11, labels=c('a', 'b', 'c', 'd'), label_size=16, nrow=2)
#save_plot(filename="capacityLWSBA4sum_ZNZseason.png", plot=pic, base_height=12, base_width=12, dpi=300)


#########################################################################################################
# % empty delivery room for most and least busy months (June vs January)
#########################################################################################################

breaksJan <- seq(as.POSIXct('2013-01-01 00:00', tz = "GMT"),by = '1 hours', length = 31*24+1)
breaksJun <- seq(as.POSIXct('2013-06-01 00:00', tz = "GMT"),by = '1 hours', length = 30*24+1)

start <- proc.time()

# set up cluster
cl <- makeCluster(8)
registerDoParallel(cl)
registerDoRNG(seed=seed)

# call parallel for loop
foreachResultsJan <- foreach(i=1:n, .packages=c("lubridate")) %dorng% calcFacStats(params, FT, breaksJan)
#resJan <- calcFacStats(params,FT, breaksJan)

# end cluster
stopImplicitCluster()

end <- proc.time() - start
print(end)


start <- proc.time()

# set up cluster
cl <- makeCluster(8)
registerDoParallel(cl)
registerDoRNG(seed=seed)

# call parallel for loop
foreachResultsJun <- foreach(i=1:n, .packages=c("lubridate")) %dorng% calcFacStats(params, FT, breaksJun)
#resJun <- calcFacStats(params,FT, breaksJun)

# end cluster
stopImplicitCluster()

end <- proc.time() - start
print(end)



emptyLW.Jun.ll <- list()
emptyLW.Jan.ll <- list()
overLW.Jun.ll <- list()
overLW.Jan.ll <- list()
overSBA.Jun.ll <- list()
overSBA.Jan.ll <- list()
overSBA4.Jun.ll <- list()
overSBA4.Jan.ll <- list()


for(i in 1:length(foreachResultsJan))
{
	# append results to lists
	emptyLW.Jun.ll[[i]] <- foreachResultsJun[[i]]$percentTimeLWEmpty
	emptyLW.Jan.ll[[i]] <- foreachResultsJan[[i]]$percentTimeLWEmpty

	overLW.Jun.ll[[i]] <- foreachResultsJun[[i]]$percentTimeLWOverCapacity
	overLW.Jan.ll[[i]] <- foreachResultsJan[[i]]$percentTimeLWOverCapacity

	overSBA.Jun.ll[[i]] <- foreachResultsJun[[i]]$percentLWOverSBA
	overSBA4.Jun.ll[[i]] <- foreachResultsJun[[i]]$percentLWOverSBA4

	overSBA.Jan.ll[[i]] <- foreachResultsJan[[i]]$percentLWOverSBA
	overSBA4.Jan.ll[[i]] <- foreachResultsJan[[i]]$percentLWOverSBA4

}


df.empty.Jun <- as.data.frame(do.call(rbind, emptyLW.Jun.ll))
df.empty.Jan <- as.data.frame(do.call(rbind, emptyLW.Jan.ll))

df.overLW.Jun <- as.data.frame(do.call(rbind, overLW.Jun.ll))
df.overLW.Jan <- as.data.frame(do.call(rbind, overLW.Jan.ll))

df.overSBA.Jun <- as.data.frame(do.call(rbind, overSBA.Jun.ll))
df.overSBA.Jan <- as.data.frame(do.call(rbind, overSBA.Jan.ll))

df.overSBA4.Jun <- as.data.frame(do.call(rbind, overSBA4.Jun.ll))
df.overSBA4.Jan <- as.data.frame(do.call(rbind, overSBA4.Jan.ll))

# save output data
#write.csv(x=df.overLW.Jan, file="pctTimeExceedLWCapacityJan_ZNZ.csv", row.names=FALSE)
#write.csv(x=df.empty.Jan, file="pctTimeEmptyLWJan_ZNZ.csv", row.names=FALSE)
#write.csv(x=df.overSBA.Jan, file="pctTimeExceedSBACapacityJan_ZNZ.csv", row.names=FALSE)
#write.csv(x=df.overSBA4.Jan, file="pctTimeExceedSBA4CapacityJan_ZNZ.csv", row.names=FALSE)

#write.csv(x=df.overLW.Jun, file="pctTimeExceedLWCapacityJun_ZNZ.csv", row.names=FALSE)
#write.csv(x=df.empty.Jun, file="pctTimeEmptyLWJun_ZNZ.csv", row.names=FALSE)
#write.csv(x=df.overSBA.Jun, file="pctTimeExceedSBACapacityJun_ZNZ.csv", row.names=FALSE)
#write.csv(x=df.overSBA4.Jun, file="pctTimeExceedSBA4CapacityJun_ZNZ.csv", row.names=FALSE)


#################################################################################################################################
# PLOT PERCENT OF TIME DURING WHICH FACILITIES EXCEED CAPACITY OR ARE EMPTY IN JANUARY AND JUNE
#################################################################################################################################

df.overLW.Jan <- read.csv("ZanzibarScenario_ZNZ_DHS_Seasonal\\1000iter\\pctTimeExceedLWCapacityJan_ZNZ.csv")
df.empty.Jan <- read.csv("ZanzibarScenario_ZNZ_DHS_Seasonal\\1000iter\\pctTimeEmptyLWJan_ZNZ.csv")
df.overSBA.Jan <- read.csv("ZanzibarScenario_ZNZ_DHS_Seasonal\\1000iter\\pctTimeExceedSBACapacityJan_ZNZ.csv")
df.overSBA4.Jan <- read.csv("ZanzibarScenario_ZNZ_DHS_Seasonal\\1000iter\\pctTimeExceedSBA4CapacityJan_ZNZ.csv")

df.overLW.Jun <- read.csv("ZanzibarScenario_ZNZ_DHS_Seasonal\\1000iter\\pctTimeExceedLWCapacityJun_ZNZ.csv")
df.empty.Jun <- read.csv("ZanzibarScenario_ZNZ_DHS_Seasonal\\1000iter\\pctTimeEmptyLWJun_ZNZ.csv")
df.overSBA.Jun <- read.csv("ZanzibarScenario_ZNZ_DHS_Seasonal\\1000iter\\pctTimeExceedSBACapacityJun_ZNZ.csv")
df.overSBA4.Jun <- read.csv("ZanzibarScenario_ZNZ_DHS_Seasonal\\1000iter\\pctTimeExceedSBA4CapacityJun_ZNZ.csv")


df.empty.Jun.long <- gather(df.empty.Jun, Facility, PctTimeLWEmpty, count1:count35, factor_key=TRUE)
levels(df.empty.Jun.long$Facility) <- paste("Facility", seq(1, 35))
df.empty.Jun.long$Type <- rep("BEmOC", nrow(df.empty.Jun.long))
df.empty.Jun.long$Type[df.empty.Jun.long$Facility%in%paste("Facility", 1:7)] <- "CEmOC"

p11max <- ggplot(df.empty.Jun.long, aes(x=Facility, y=PctTimeLWEmpty, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time delivery room is empty") + ylim(0, 100) +
	scale_fill_manual(values=c("lightgrey", "grey27")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=11), axis.text.x=element_blank(), legend.position="none", plot.margin=margin(0, 0, 0, 1, "cm"))
print(p11max)


df.empty.Jan.long <- gather(df.empty.Jan, Facility, PctTimeLWEmpty, count1:count35, factor_key=TRUE)
levels(df.empty.Jan.long$Facility) <- paste("Facility", seq(1, 35))
df.empty.Jan.long$Type <- rep("BEmOC", nrow(df.empty.Jan.long))
df.empty.Jan.long$Type[df.empty.Jan.long$Facility%in%paste("Facility", 1:7)] <- "CEmOC"

p11min <- ggplot(df.empty.Jan.long, aes(x=Facility, y=PctTimeLWEmpty, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time delivery room is empty") + ylim(0, 100) +
	scale_fill_manual(values=c("lightgrey", "grey27")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=11), axis.text.x=element_blank(), legend.position="none", plot.margin=margin(0, 0, 0, 1, "cm"))
print(p11min)


df.overSBA.Jun.long <- gather(df.overSBA.Jun, Facility, PctTimeOverSBACapacity, count1:count35, factor_key=TRUE)
levels(df.overSBA.Jun.long$Facility) <- paste("Facility", seq(1, 35))
df.overSBA.Jun.long$Type <- rep("BEmOC", nrow(df.overSBA.Jun.long))
df.overSBA.Jun.long$Type[df.overSBA.Jun.long$Facility%in%paste("Facility", 1:7)] <- "CEmOC"

p12max <- ggplot(df.overSBA.Jun.long, aes(x=Facility, y=PctTimeOverSBACapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time when more women than SBAs in delivery room") + ylim(0, 100) +
	scale_fill_manual(values=c("lightgrey", "grey27")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1), legend.position="none", plot.margin=margin(0, 0, 0, 1, "cm"))
print(p12max)


df.overSBA.Jan.long <- gather(df.overSBA.Jan, Facility, PctTimeOverSBACapacity, count1:count35, factor_key=TRUE)
levels(df.overSBA.Jan.long$Facility) <- paste("Facility", seq(1, 35))
df.overSBA.Jan.long$Type <- rep("BEmOC", nrow(df.overSBA.Jan.long))
df.overSBA.Jan.long$Type[df.overSBA.Jan.long$Facility%in%paste("Facility", 1:7)] <- "CEmOC"

p12min <- ggplot(df.overSBA.Jan.long, aes(x=Facility, y=PctTimeOverSBACapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time when more women than SBAs in delivery room") + ylim(0, 100) +
	scale_fill_manual(values=c("lightgrey", "grey27")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1), legend.position="none", plot.margin=margin(0, 0, 0, 1, "cm"))
print(p12min)


df.overLW.Jun.long <- gather(df.overLW.Jun, Facility, PctTimeOverLWCapacity, count1:count35, factor_key=TRUE)
levels(df.overLW.Jun.long$Facility) <- paste("Facility", seq(1, 35))
df.overLW.Jun.long$Type <- rep("BEmOC", nrow(df.overLW.Jun.long))
df.overLW.Jun.long$Type[df.overLW.Jun.long$Facility%in%paste("Facility", 1:7)] <- "CEmOC"

p13max <- ggplot(df.overLW.Jun.long, aes(x=Facility, y=PctTimeOverLWCapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time bed capacity in delivery room exceeded") + ylim(0, 20) +
	scale_fill_manual(values=c("lightgrey", "grey27")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_blank(), legend.position=c(0.8, 0.8), legend.title=element_blank(), legend.text=element_text(size=14), plot.margin=margin(0, 0, 0, 1, "cm"))
print(p13max)


df.overLW.Jan.long <- gather(df.overLW.Jan, Facility, PctTimeOverLWCapacity, count1:count35, factor_key=TRUE)
levels(df.overLW.Jan.long$Facility) <- paste("Facility", seq(1, 35))
df.overLW.Jan.long$Type <- rep("BEmOC", nrow(df.overLW.Jan.long))
df.overLW.Jan.long$Type[df.overLW.Jan.long$Facility%in%paste("Facility", 1:7)] <- "CEmOC"

p13min <- ggplot(df.overLW.Jan.long, aes(x=Facility, y=PctTimeOverLWCapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time bed capacity in delivery room exceeded") + ylim(0, 20) +
	scale_fill_manual(values=c("lightgrey", "grey27")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_blank(), legend.position=c(0.8, 0.8), legend.title=element_blank(), legend.text=element_text(size=14), plot.margin=margin(0, 0, 0, 1, "cm"))
print(p13min)


pic <- plot_grid(p13max, p11max, p12max, labels=c('a', 'b', 'c'), label_size=16, nrow=3)
#save_plot(filename="capacityLW_ZNZseason_Jun.png", plot=pic, base_height=16, base_width=12, dpi=300)

pic <- plot_grid(p13min, p11min, p12min, labels=c('a', 'b', 'c'), label_size=16, nrow=3)
#save_plot(filename="capacityLW_ZNZseason_Jan.png", plot=pic, base_height=16, base_width=12, dpi=300)



df.overSBA4.Jun.long <- gather(df.overSBA4.Jun, Facility, PctTimeOverSBACapacity, count1:count35, factor_key=TRUE)
levels(df.overSBA4.Jun.long$Facility) <- paste("Facility", seq(1, 35))
df.overSBA4.Jun.long$Type <- rep("BEmOC", nrow(df.overSBA4.Jun.long))
df.overSBA4.Jun.long$Type[df.overSBA4.Jun.long$Facility%in%paste("Facility", 1:7)] <- "CEmOC"

p14max <- ggplot(df.overSBA4.Jun.long, aes(x=Facility, y=PctTimeOverSBACapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time when > 4 women per SBA in delivery room") + ylim(0, 50) +
	scale_fill_manual(values=c("lightgrey", "grey27")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1), legend.position="none", plot.margin=margin(0, 0, 0, 1, "cm"))
print(p14max)


df.overSBA4.Jan.long <- gather(df.overSBA4.Jan, Facility, PctTimeOverSBACapacity, count1:count35, factor_key=TRUE)
levels(df.overSBA4.Jan.long$Facility) <- paste("Facility", seq(1, 35))
df.overSBA4.Jan.long$Type <- rep("BEmOC", nrow(df.overSBA4.Jan.long))
df.overSBA4.Jan.long$Type[df.overSBA4.Jan.long$Facility%in%paste("Facility", 1:7)] <- "CEmOC"

p14min <- ggplot(df.overSBA4.Jan.long, aes(x=Facility, y=PctTimeOverSBACapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time when > 4 women per SBA in delivery room") + ylim(0, 50) +
	scale_fill_manual(values=c("lightgrey", "grey27")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1), legend.position="none", plot.margin=margin(0, 0, 0, 1, "cm"))
print(p14min)


pic <- plot_grid(p13max, p11max, p14max, labels=c('a', 'b', 'c'), nrow=3)
#save_plot(filename="capacityLW4_ZNZseason_Jun.png", plot=pic, base_height=18, base_width=15, dpi=300)

pic <- plot_grid(p13min, p11min, p14min, labels=c('a', 'b', 'c'), nrow=3)
#save_plot(filename="capacityLW4_ZNZseason_Jan.png", plot=pic, base_height=18, base_width=15, dpi=300)


#################################################################################################################################
# AGGREGATE PLOTS BY CEmOCs and BEmOCs
#################################################################################################################################

df.overLW.Jun.long$Type <- factor(df.overLW.Jun.long$Type, levels=c("CEmOC", "BEmOC"))

p7maxsum <- ggplot(df.overLW.Jun.long, aes(x=Type, y=PctTimeOverLWCapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time bed capacity in delivery room exceeded") + ylim(0, 100) +
	scale_fill_manual(values=c("lightgrey", "darkgrey")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=14), legend.position="none", plot.margin=margin(0,0,0,1, "cm"))
print(p7maxsum)


df.overLW.Jan.long$Type <- factor(df.overLW.Jan.long$Type, levels=c("CEmOC", "BEmOC"))

p7minsum <- ggplot(df.overLW.Jan.long, aes(x=Type, y=PctTimeOverLWCapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time bed capacity in delivery room exceeded") + ylim(0, 100) +
	scale_fill_manual(values=c("lightgrey", "darkgrey")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=14), legend.position="none", plot.margin=margin(0,0,0,1, "cm"))
print(p7minsum)


df.empty.Jun.long$Type <- factor(df.empty.Jun.long$Type, levels=c("CEmOC", "BEmOC"))

p8maxsum <- ggplot(df.empty.Jun.long, aes(x=Type, y=PctTimeLWEmpty, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time delivery room is empty") + ylim(0, 100) +
	scale_fill_manual(values=c("lightgrey", "darkgrey")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=14), legend.position="none", plot.margin=margin(0,0,0,1, "cm"))
print(p8maxsum)


df.empty.Jan.long$Type <- factor(df.empty.Jan.long$Type, levels=c("CEmOC", "BEmOC"))

p8minsum <- ggplot(df.empty.Jan.long, aes(x=Type, y=PctTimeLWEmpty, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time delivery room is empty") + ylim(0, 100) +
	scale_fill_manual(values=c("lightgrey", "darkgrey")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=14), legend.position="none", plot.margin=margin(0,0,0,1, "cm"))
print(p8minsum)


df.overSBA.Jun.long$Type <- factor(df.overSBA.Jun.long$Type, levels=c("CEmOC", "BEmOC"))

p9maxsum <- ggplot(df.overSBA.Jun.long, aes(x=Type, y=PctTimeOverSBACapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time when more women than SBAs in delivery room") + ylim(0, 100) +
	scale_fill_manual(values=c("lightgrey", "darkgrey")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=14), legend.position="none", plot.margin=margin(0,0,0,1, "cm"))
print(p9maxsum)


df.overSBA.Jan.long$Type <- factor(df.overSBA.Jan.long$Type, levels=c("CEmOC", "BEmOC"))

p9minsum <- ggplot(df.overSBA.Jan.long, aes(x=Type, y=PctTimeOverSBACapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time when more women than SBAs in delivery room") + ylim(0, 100) +
	scale_fill_manual(values=c("lightgrey", "darkgrey")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=14), legend.position="none", plot.margin=margin(0,0,0,1, "cm"))
print(p9minsum)


df.overSBA4.Jun.long$Type <- factor(df.overSBA4.Jun.long$Type, levels=c("CEmOC", "BEmOC"))

p9maxsumalt <- ggplot(df.overSBA4.Jun.long, aes(x=Type, y=PctTimeOverSBACapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time when >4 women per SBA in delivery room") + ylim(0, 100) +
	scale_fill_manual(values=c("lightgrey", "darkgrey")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=14), legend.position="none", plot.margin=margin(0,0,0,1, "cm"))
print(p9maxsumalt)


df.overSBA4.Jan.long$Type <- factor(df.overSBA4.Jan.long$Type, levels=c("CEmOC", "BEmOC"))

p9minsumalt <- ggplot(df.overSBA4.Jan.long, aes(x=Type, y=PctTimeOverSBACapacity, fill=Type)) + theme_classic() + geom_boxplot(colour="black") + 
	ylab("% Time when >4 women per SBA in delivery room") + ylim(0, 100) +
	scale_fill_manual(values=c("lightgrey", "darkgrey")) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=14), legend.position="none", plot.margin=margin(0,0,0,1, "cm"))
print(p9minsumalt)


pic <- plot_grid(p7maxsum, p8maxsum, p9maxsum, labels=c('a', 'b', 'c'), label_size=16, nrow=1)
#save_plot(filename="capacityLWsumJun_ZNZseason.png", plot=pic, base_height=6, base_width=12, dpi=300)

pic <- plot_grid(p7minsum, p8minsum, p9minsum, labels=c('a', 'b', 'c'), label_size=16, nrow=1)
#save_plot(filename="capacityLWsumJan_ZNZseason.png", plot=pic, base_height=6, base_width=12, dpi=300)

pic <- plot_grid(p7maxsum, p8maxsum, p9maxsumalt, labels=c('a', 'b', 'c'), label_size=16, nrow=1)
#save_plot(filename="capacityLWSBA4sumJun_ZNZseason.png", plot=pic, base_height=6, base_width=12, dpi=300)

pic <- plot_grid(p7minsum, p8minsum, p9minsumalt, labels=c('a', 'b', 'c'), label_size=16, nrow=1)
#save_plot(filename="capacityLWSBA4sumJan_ZNZseason.png", plot=pic, base_height=6, base_width=12, dpi=300)


fig5 <- plot_grid(p7maxsum, p8maxsum, p9maxsum, p7minsum, p8minsum, p9minsum, labels=c('a', 'b', 'c', 'd', 'e', 'f'), label_size=16, nrow=2)
#save_plot(filename="capacityLWsumJunJan_ZNZseason.png", plot=fig5, base_height=10, base_width=12, dpi=300)


