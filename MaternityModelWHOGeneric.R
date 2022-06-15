###################################################################
# BECKY BAGGALEY, SEPTEMBER 2020, LENGTH OF STAY MODEL - ZANZIBAR
# CODE TO DETERMINE DELIVERY ROOM AND MATERNITY WARD OCCUPANCY
# IN TWO GENERIC SCENARIOS FOR SSA BASED ON WHO GUIDELINES
# EDITS BY CAROLIN VEGVARI 19/10/2020
# EDITS AND REFACTORING BY CAROLIN VEGVARI 14/06/2022
# github: https://github.com/kl3mn9/maternity-model
###################################################################


rm(list=ls())
closeAllConnections()

library(lubridate)	# for handling times and dates
library(chron)
library(ggplot2)		# for plotting
library(tidyr)		# for data formating
library(cowplot)		# for composite plots

# set working directory
setwd("C:\\Users\\Carolin\\Documents\\Write-up\\BeckyZanzibarMaternity\\")

set.seed(42)		#for reproducibility


###########################################################################
# Model parameters		# CV: consider moving to parameter file to make script file general to any country
###########################################################################
timeZone 		<- "Africa/Dar_es_Salaam"
prob_comp		<- 0.15	# % deliveries with complications
min_labour		<- 0.25	# minimum duration in labour ward (hours)
min_postp		<- 2		# minimum duration in postpartum ward (hours)
factor_dur_comp	<- 1.5	# factor increase in duration in labour ward if delivery is complicated
prob_CEmOC_comp	<- 0.9	# probability delivery in CEmOC, complicated delivery (NB// assumes all other deliveries in BEmOC)

births		<- 3600	# WHO World Health Report example

# LENGTHS OF STAY PARAMETERS
# POST PARTUM
shapePPUWHO <- 1.058021		#shape parameter, uncomplicated
scalePPUWHO <- 50.15097		#scale parameter, uncomplicated
shapePPCWHO <- 1.230724		#shape parameter, complicated
scalePPCWHO <- 136.0294		#scale parameter, complicated

# DELIVERY ROOM
shapeDelU <- 3.37 		#shape parameter, uncomplicated
scaleDelU <- 1.23 		#scale parameter, uncomplicated


# generating distributions - duration in delivery and postpartum rooms
durDelUncomp <- rgamma(n=births,shape=shapeDelU,scale=scaleDelU)				#duration in delivery room, uncomplicated delivery
durDelComp <- factor_dur_comp*rgamma(n=births,shape=shapeDelU,scale=scaleDelU)	#duration in delivery room, complicated delivery

durPPUWHO <- rgamma(n=births,shape=shapePPUWHO,scale=scalePPUWHO)	#duration postpartum, uncomplicated delivery, WHO analysis
durPPCWHO <- rgamma(n=births,shape=shapePPCWHO,scale=scalePPCWHO)	#duration postpartum, complicated delivery, WHO analysis


###########################################################################
# convenience functions
###########################################################################

#Random date and time function taken from stackoverflow
#http://stackoverflow.com/questions/14720983/efficiently-generate-a-random-sample-of-times-and-dates-between-two-dates
latemail <- function(N, st="2014/01/01", et="2014/12/31") 
{
	st <- as.POSIXct(as.Date(st))
      et <- as.POSIXct(as.Date(et))
      difft <- as.numeric(difftime(et,st,unit="sec"))
      ev <- sort(runif(N, 0, difft))
     	rt <- st + ev
	return(rt)
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

# function to determine occupancy of different wards/rooms depending on starTime, finish entered
getOccupancyByCentreSnapshot <- function(start, finish, breaks, timeZone, facility, facility.numbers)
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


############################################################
# START OF SIMULATION
############################################################

temp3 <- latemail(births)			# each woman set a random date and time of presentation in labour at a health facility
							# the vector orders them by date (for one year - 2014)
tz(temp3) <- timeZone				# set time zone, e.g. to East African Time (for Zanzibar)
labour_start <- temp3[sample(births)]	# labour_start dates and times in random order (means date/time of admission to labour ward)


complicated <- rbinom(births, 1, prob_comp)	# assign whether each woman has a complicated delivery: 1= complicated 0= uncomplicated
                                              	# WHO general

####################################
###Duration in the labour ward######
####################################

dur_labour <- rep(0,births)			#duration in labour ward dependant on whether delivery is complicated
							#and must be minimum of min_labour

dur_labour[complicated==1] <- durDelComp[complicated==1]
dur_labour[complicated==0] <- durDelUncomp[complicated==0]
dur_labour[dur_labour<min_labour]<-min_labour

labour_end <- labour_start + dur_labour*60*60	#date and time that delivery ends (dur_labour in hours converted into seconds)

####################################
###Duration in the maternity ward###
####################################

dur_maternityWHO <- rep(0,births)		#duration in maternity ward dependant on whether delivery is complicated

dur_maternityWHO[complicated==1] <- durPPCWHO[complicated==1]
dur_maternityWHO[complicated==0] <- durPPUWHO[complicated==0]


discharge1WHO <- labour_end + dur_maternityWHO*60*60	# literal discharge time i.e., could be in the middle of the night
									

################################################################################
# WORLD HEALTH REPORT SCENARIOS 1 AND 2
# SCENARIO 1: 1 CEmOC with 10 SBAs, 2 BEmOCs with 5 SBAs each
# SCENARIO 2: 1 CEmOC with 10 SBAs, 5 BEmOCs with 2 SBAs each
# BOTH SCENARIOS: complicated deliveries happen in CEmOC with probability 0.9
#			uncomplicated births equally likely to happen in any facility
################################################################################

facility1 <- rep(0, births)		# facility allocation for Scenario 1
facility2 <- rep(0, births)		# facility allocation for Scenario 2
tempWHO <- runif(births, 0, 1)

# ALLOCATE FACILITIES IN SCENARIO 1 (3 FACILITIES IN TOTAL)
# allocate complicated births
facility1 <- ifelse(complicated == 1 & tempWHO <= prob_CEmOC_comp, 1, 0)
facility1[tempWHO < 0.5 & complicated == 1 & facility1 == 0] <- 2
facility1[tempWHO >= 0.5 & complicated == 1 & facility1 == 0] <- 3

# allocate uncomplicated births
facility1[complicated==0] <- sample(1:3, size=length(facility1[complicated==0]), replace=TRUE)


# ALLOCATE FACILITIES IN SCENARIO 2 (6 FACILITIES IN TOTAL)
# allocate complicated births
facility2 <- ifelse(complicated == 1 & tempWHO <= prob_CEmOC_comp, 1, 0)
facility2[complicated==1 & facility2==0] <- sample(2:6, size=length(facility2[complicated==1 & facility2==0]), replace=TRUE)

# allocate uncomplicated births
facility2[complicated==0] <- sample(1:6, size=length(facility2[complicated==0]), replace=TRUE)


#################################################################################################################################
# OCCUPANCY AT SNAPSHOTS IN TIME (4:00, 12:00, 20:00)
#################################################################################################################################

# times to generate 'snapshots', 
# i.e. times at which to establish delivery room and maternity ward occupancy
# 3 snapshots per day over a year
breaks <- seq(as.POSIXct('2014-01-01 00:00', tz = timeZone), by = '1 hours', length = 365*24+1)
temp <- format(breaks,"%H:%M:%S")
breaks3 <- breaks[temp=="04:00:00" | temp=="12:00:00" |temp=="20:00:00"]


# ALL BIRTHS ############################################################################################

dischargeTime <- getDischargeTime(discharge1WHO)	# ensure that women don't get discharged at night

# SCENARIO 1 

# Occupancy maternity centre (including time spent in delivery room and time in post-partum room
freq1 <- getOccupancyByCentreSnapshot(labour_start, dischargeTime, breaks3, timeZone, facility1, facility.numbers=1:3)

# Occupancy delivery room 
freqLabour1 <- getOccupancyByCentreSnapshot(labour_start, labour_end, breaks3, timeZone, facility1, facility.numbers=1:3)

# Occupancy post-partum room
freqMat1 <- getOccupancyByCentreSnapshot(labour_end, dischargeTime, breaks3, timeZone, facility1, facility.numbers=1:3)


# SCENARIO 2

# Occupancy maternity centre (including time spent in delivery room and time in post-partum room
freq2 <- getOccupancyByCentreSnapshot(labour_start, dischargeTime, breaks3, timeZone, facility2, facility.numbers=1:6)

# Occupancy delivery room 
freqLabour2 <- getOccupancyByCentreSnapshot(labour_start, labour_end, breaks3, timeZone, facility2, facility.numbers=1:6)

# Occupancy post-partum room
freqMat2 <- getOccupancyByCentreSnapshot(labour_end, dischargeTime, breaks3, timeZone, facility2, facility.numbers=1:6)


# COMPLICATED BIRTHS ONLY ################################################################################

labour_start_comp <- labour_start[complicated==1]
labour_end_comp <- labour_end[complicated==1]
dischargeTime_comp <- dischargeTime[complicated==1]


# SCENARIO 1 

# Occupancy maternity centre (including time spent in delivery room and time in post-partum room
freq1_comp <- getOccupancyByCentreSnapshot(labour_start_comp, dischargeTime_comp, breaks3, timeZone, facility1[complicated==1], facility.numbers=1:3)

# Occupancy delivery room 
freqLabour1_comp <- getOccupancyByCentreSnapshot(labour_start_comp, labour_end_comp, breaks3, timeZone, facility1[complicated==1], facility.numbers=1:3)

# Occupancy post-partum room
freqMat1_comp <- getOccupancyByCentreSnapshot(labour_end_comp, dischargeTime_comp, breaks3, timeZone, facility1[complicated==1], facility.numbers=1:3)


# SCENARIO 2

# Occupancy maternity centre (including time spent in delivery room and time in post-partum room
freq2_comp <- getOccupancyByCentreSnapshot(labour_start_comp, dischargeTime_comp, breaks3, timeZone, facility2[complicated==1], facility.numbers=1:6)

# Occupancy delivery room 
freqLabour2_comp <- getOccupancyByCentreSnapshot(labour_start_comp, labour_end_comp, breaks3, timeZone, facility2[complicated==1], facility.numbers=1:6)

# Occupancy post-partum room
freqMat2_comp <- getOccupancyByCentreSnapshot(labour_end_comp, dischargeTime_comp, breaks3, timeZone, facility2[complicated==1], facility.numbers=1:6)


#################################################################################################################################
# PLOT SNAPSHOTS OF WOMEN IN FACILITIES, DELIVERY ROOM AND POST-PARTUM ROOM
#################################################################################################################################

# WOMEN IN FACILITIES - TOTAL
# SCENARIO 1
# Plot total number of births and complicated births over a representative month (snapshots, facility occupancy)
df.total.snaps1 <- data.frame(Snapshot=breaks3, TotalBirths=rowSums(freq1[, 2:ncol(freq1)]), CompBirths=rowSums(freq1_comp[,2:ncol(freq1_comp)]))
df.total.snaps1$UncompBirths <- df.total.snaps1$TotalBirths - df.total.snaps1$CompBirths

# 178:270 are rownumbers of snapshots in March
df.total.snaps1.long <- gather(df.total.snaps1[178:270, -2], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

p1 <- ggplot(df.total.snaps1.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a month") + ylab("Number of women in EmOCs") + ylim(0, 45) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=16))
print(p1)
ggsave("WHO_S1_TotalBirthsMonthSingle.png", plot=p1, dpi=300, width=12, height=8)


# SCENARIO 2
# Plot total number of births and complicated births over a representative month (snapshots, facility occupancy)
df.total.snaps2 <- data.frame(Snapshot=breaks3, TotalBirths=rowSums(freq2[, 2:ncol(freq1)]), CompBirths=rowSums(freq2_comp[,2:ncol(freq2_comp)]))
df.total.snaps2$UncompBirths <- df.total.snaps2$TotalBirths - df.total.snaps2$CompBirths

# 178:270 are rownumbers of snapshots in March
df.total.snaps2.long <- gather(df.total.snaps2[178:270, -2], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

p2 <- ggplot(df.total.snaps2.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a month") + ylab("Number of women in EmOCs") + ylim(0, 45) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=16))
print(p2)
ggsave("WHO_S2_TotalBirthsMonthSingle.png", plot=p2, dpi=300, width=12, height=8)


# WOMEN IN FACILITIES - BY FACILITY
# SCENARIO 1
# Plot number of births and complicated births over a representative month by facility (snapshots, facility occupancy)
freq1.long <- gather(freq1[178:270,], Facility, NumBirths, count1:count3)
freq1_comp.long <- gather(freq1_comp[178:270,], Facility, CompBirths, count1:count3)
df.fac.snaps1 <- merge(freq1.long, freq1_comp.long, by=c("Snapshot", "Facility"), all=TRUE)
df.fac.snaps1 <- df.fac.snaps1[order(df.fac.snaps1$Facility, df.fac.snaps1$Snapshot), ]
df.fac.snaps1$CompBirths[is.na(df.fac.snaps1$CompBirths)] <- 0
df.fac.snaps1$UncompBirths <- df.fac.snaps1$NumBirths - df.fac.snaps1$CompBirths
df.fac.snaps1.long <- gather(df.fac.snaps1[, -3], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

fac.levels1 <- paste0("count", 1:3)
df.fac.snaps1.long$Facility <- factor(df.fac.snaps1.long$Facility, levels=fac.levels1) 
df.fac.snaps1.long <- df.fac.snaps1.long[order(df.fac.snaps1.long$Facility, df.fac.snaps1.long$Snapshot), ]
levels(df.fac.snaps1.long$Facility) <- paste("Facility", 1:3)

p3 <- ggplot(df.fac.snaps1.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a month") + ylab("Number of women in EmOCs") + facet_wrap(~ Facility, ncol=3) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
print(p3)
ggsave("WHO_S1_BirthsFacilityMonthSingle.png", plot=p3, dpi=300, width=18, height=9)


# SCENARIO 2
# Plot number of births and complicated births over a representative month by facility (snapshots, facility occupancy)
freq2.long <- gather(freq2[178:270,], Facility, NumBirths, count1:count6)
freq2_comp.long <- gather(freq2_comp[178:270,], Facility, CompBirths, count1:count6)
df.fac.snaps2 <- merge(freq2.long, freq2_comp.long, by=c("Snapshot", "Facility"), all=TRUE)
df.fac.snaps2 <- df.fac.snaps2[order(df.fac.snaps2$Facility, df.fac.snaps2$Snapshot), ]
df.fac.snaps2$CompBirths[is.na(df.fac.snaps2$CompBirths)] <- 0
df.fac.snaps2$UncompBirths <- df.fac.snaps2$NumBirths - df.fac.snaps2$CompBirths
df.fac.snaps2.long <- gather(df.fac.snaps2[, -3], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

fac.levels2 <- paste0("count", 1:6)
df.fac.snaps2.long$Facility <- factor(df.fac.snaps2.long$Facility, levels=fac.levels2) 
df.fac.snaps2.long <- df.fac.snaps2.long[order(df.fac.snaps2.long$Facility, df.fac.snaps2.long$Snapshot), ]
levels(df.fac.snaps2.long$Facility) <- paste("Facility", 1:6)

p4 <- ggplot(df.fac.snaps2.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a month") + ylab("Number of women in EmOCs") + facet_wrap(~ Facility, ncol=3) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
print(p4)
ggsave("WHO_S2_BirthsFacilityMonthSingle.png", plot=p4, dpi=300, width=18, height=12)


# WOMEN IN DELIVERY ROOMS - BY FACILITY
# SCENARIO 1
# Plot number of births and complicated births over a representative month by facility (snapshots, facility occupancy)
freqLabour1.long <- gather(freqLabour1[178:270,], Facility, NumBirths, count1:count3)
freqLabour1_comp.long <- gather(freqLabour1_comp[178:270,], Facility, CompBirths, count1:count3)
df.del.fac.snaps1 <- merge(freqLabour1.long, freqLabour1_comp.long, by=c("Snapshot", "Facility"), all=TRUE)
df.del.fac.snaps1 <- df.del.fac.snaps1[order(df.del.fac.snaps1$Facility, df.del.fac.snaps1$Snapshot), ]
df.del.fac.snaps1$CompBirths[is.na(df.del.fac.snaps1$CompBirths)] <- 0
df.del.fac.snaps1$UncompBirths <- df.del.fac.snaps1$NumBirths - df.del.fac.snaps1$CompBirths
df.del.fac.snaps1.long <- gather(df.del.fac.snaps1[, -3], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

fac.levels1 <- paste0("count", 1:3)
df.del.fac.snaps1.long$Facility <- factor(df.del.fac.snaps1.long$Facility, levels=fac.levels1) 
df.del.fac.snaps1.long <- df.del.fac.snaps1.long[order(df.del.fac.snaps1.long$Facility, df.del.fac.snaps1.long$Snapshot), ]
levels(df.del.fac.snaps1.long$Facility) <- paste("Facility", 1:3)

p5 <- ggplot(df.del.fac.snaps1.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a month") + ylab("Number of women in delivery rooms") + facet_wrap(~ Facility, ncol=3) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
print(p5)
ggsave("WHO_S1_DelFacilityMonthSingle.png", plot=p5, dpi=300, width=18, height=9)


# SCENARIO 2
# Plot number of births and complicated births over a representative month by facility (snapshots, facility occupancy)
freqLabour2.long <- gather(freqLabour2[178:270,], Facility, NumBirths, count1:count6)
freqLabour2_comp.long <- gather(freqLabour2_comp[178:270,], Facility, CompBirths, count1:count6)
df.del.fac.snaps2 <- merge(freqLabour2.long, freqLabour2_comp.long, by=c("Snapshot", "Facility"), all=TRUE)
df.del.fac.snaps2 <- df.del.fac.snaps2[order(df.del.fac.snaps2$Facility, df.del.fac.snaps2$Snapshot), ]
df.del.fac.snaps2$CompBirths[is.na(df.del.fac.snaps1$CompBirths)] <- 0
df.del.fac.snaps2$UncompBirths <- df.del.fac.snaps2$NumBirths - df.del.fac.snaps2$CompBirths
df.del.fac.snaps2.long <- gather(df.del.fac.snaps2[, -3], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

fac.levels2 <- paste0("count", 1:6)
df.del.fac.snaps2.long$Facility <- factor(df.del.fac.snaps2.long$Facility, levels=fac.levels2) 
df.del.fac.snaps2.long <- df.del.fac.snaps2.long[order(df.del.fac.snaps2.long$Facility, df.del.fac.snaps2.long$Snapshot), ]
levels(df.del.fac.snaps2.long$Facility) <- paste("Facility", 1:6)

p6 <- ggplot(df.del.fac.snaps2.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a month") + ylab("Number of women in delivery rooms") + facet_wrap(~ Facility, ncol=3) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
print(p6)
ggsave("WHO_S2_DelFacilityMonthSingle.png", plot=p6, dpi=300, width=18, height=12)


# WOMEN IN MATERNITY BEDS (POST-PARTUM) - BY FACILITY
# SCENARIO 1
# Plot number of births and complicated births over a representative month by facility (snapshots, facility occupancy)
freqMat1.long <- gather(freqMat1[178:270,], Facility, NumBirths, count1:count3)
freqMat1_comp.long <- gather(freqMat1_comp[178:270,], Facility, CompBirths, count1:count3)
df.mat.fac.snaps1 <- merge(freqMat1.long, freqMat1_comp.long, by=c("Snapshot", "Facility"), all=TRUE)
df.mat.fac.snaps1 <- df.mat.fac.snaps1[order(df.mat.fac.snaps1$Facility, df.mat.fac.snaps1$Snapshot), ]
df.mat.fac.snaps1$CompBirths[is.na(df.mat.fac.snaps1$CompBirths)] <- 0
df.mat.fac.snaps1$UncompBirths <- df.mat.fac.snaps1$NumBirths - df.mat.fac.snaps1$CompBirths
df.mat.fac.snaps1.long <- gather(df.mat.fac.snaps1[, -3], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

fac.levels1 <- paste0("count", 1:3)
df.mat.fac.snaps1.long$Facility <- factor(df.mat.fac.snaps1.long$Facility, levels=fac.levels1) 
df.mat.fac.snaps1.long <- df.mat.fac.snaps1.long[order(df.mat.fac.snaps1.long$Facility, df.mat.fac.snaps1.long$Snapshot), ]
levels(df.mat.fac.snaps1.long$Facility) <- paste("Facility", 1:3)

p7 <- ggplot(df.mat.fac.snaps1.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a month") + ylab("Number of women in post-partum beds") + facet_wrap(~ Facility, ncol=3) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
print(p7)
ggsave("WHO_S1_MatFacilityMonthSingle.png", plot=p7, dpi=300, width=18, height=9)


# SCENARIO 2
# Plot number of births and complicated births over a representative month by facility (snapshots, facility occupancy)
freqMat2.long <- gather(freqMat2[178:270,], Facility, NumBirths, count1:count6)
freqMat2_comp.long <- gather(freqMat2_comp[178:270,], Facility, CompBirths, count1:count6)
df.mat.fac.snaps2 <- merge(freqMat2.long, freqMat2_comp.long, by=c("Snapshot", "Facility"), all=TRUE)
df.mat.fac.snaps2 <- df.mat.fac.snaps2[order(df.mat.fac.snaps2$Facility, df.mat.fac.snaps2$Snapshot), ]
df.mat.fac.snaps2$CompBirths[is.na(df.mat.fac.snaps2$CompBirths)] <- 0
df.mat.fac.snaps2$UncompBirths <- df.mat.fac.snaps2$NumBirths - df.mat.fac.snaps2$CompBirths
df.mat.fac.snaps2.long <- gather(df.mat.fac.snaps2[, -3], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

fac.levels2 <- paste0("count", 1:6)
df.mat.fac.snaps2.long$Facility <- factor(df.mat.fac.snaps2.long$Facility, levels=fac.levels2) 
df.mat.fac.snaps2.long <- df.mat.fac.snaps2.long[order(df.mat.fac.snaps2.long$Facility, df.mat.fac.snaps2.long$Snapshot), ]
levels(df.mat.fac.snaps2.long$Facility) <- paste("Facility", 1:6)

p8 <- ggplot(df.mat.fac.snaps2.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a month") + ylab("Number of women in post-partum beds") + facet_wrap(~ Facility, ncol=3) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
print(p8)
ggsave("WHO_S2_MatFacilityMonthSingle.png", plot=p8, dpi=300, width=18, height=12)



#################################################################################################################################
# PROPORTION OF TIME EACH FACILITY IS OVER CAPACITY OR EMPTY --> EVALUATE CAPACITY EVERY HOUR RATHER THAN EVERY 8 HOURS
#################################################################################################################################

# repeat n times to get mean and 95% credible interval
n <- 100

nrSBAbyFac1 <- c(10, 5, 5)
LW1.ll <- list()
emptyLW1.ll <- list()
#emptyMW1.ll <- list()
birthsPerSBA1.ll <- list()

nrSBAbyFac2 <- c(10, rep(2, 5))
LW2.ll <- list()
emptyLW2.ll <- list()
#emptyMW2.ll <- list()
birthsPerSBA2.ll <- list()

start <- proc.time()

for(i in 1:n)
{
	# randomly draw distributions of duration in delivery and postpartum rooms 
	durDelUncomp <- rgamma(n=births,shape=shapeDelU,scale=scaleDelU)				# duration in delivery room, uncomplicated delivery
	durDelComp <- factor_dur_comp*rgamma(n=births,shape=shapeDelU,scale=scaleDelU)	# duration in delivery room, complicated delivery
	durPPUWHO <- rgamma(n=births,shape=shapePPUWHO,scale=scalePPUWHO)				# duration postpartum, uncomplicated delivery
	durPPCWHO <- rgamma(n=births,shape=shapePPCWHO,scale=scalePPCWHO)				# duration postpartum, complicated delivery

	temp3 <- latemail(births)			# each woman set a random date and time of presentation in labour at a health facility
								# the vector orders them by date (for one year - 2014)
	tz(temp3) <- timeZone				# set time zone, e.g. to East African Time 
	labour_start <- temp3[sample(births)]	# labour_start dates and times in random order (means date/time of admission to labour ward)


	# ALLOCATE FACILITIES
	facility1 <- rep(0, births)		# facility allocation for Scenario 1
	facility2 <- rep(0, births)		# facility allocation for Scenario 2
	tempWHO <- runif(births, 0, 1)

	# ALLOCATE FACILITIES IN SCENARIO 1 (3 FACILITIES IN TOTAL)
	# allocate complicated births
	facility1 <- ifelse(complicated == 1 & tempWHO <= prob_CEmOC_comp, 1, 0)
	facility1[tempWHO < 0.5 & complicated == 1 & facility1 == 0] <- 2
	facility1[tempWHO >= 0.5 & complicated == 1 & facility1 == 0] <- 3

	# allocate uncomplicated births
	facility1[complicated==0] <- sample(1:3, size=length(facility1[complicated==0]), replace=TRUE)

	# ALLOCATE FACILITIES IN SCENARIO 2 (6 FACILITIES IN TOTAL)
	# allocate complicated births
	facility2 <- ifelse(complicated == 1 & tempWHO <= prob_CEmOC_comp, 1, 0)
	facility2[complicated==1 & facility2==0] <- sample(2:6, size=length(facility2[complicated==1 & facility2==0]), replace=TRUE)

	# allocate uncomplicated births
	facility2[complicated==0] <- sample(1:6, size=length(facility2[complicated==0]), replace=TRUE)


	# Duration in the labour ward
	dur_labour <- rep(0,births)			
	dur_labour[complicated==1] <- durDelComp[complicated==1]		# duration in labour ward dependant on whether delivery is complicated
	dur_labour[complicated==0] <- durDelUncomp[complicated==0]		# or uncomplicated
	dur_labour[dur_labour<min_labour] <- min_labour				# and must be minimum of min_labour
	labour_end <- labour_start + dur_labour*60*60				#date and time that delivery ends (dur_labour in hours converted into seconds)

	# Duration in the maternity ward
	dur_maternity <- rep(0,births)		

	# duration in maternity ward dependant on whether delivery is complicated
	dur_maternity[complicated==1] <- durPPCWHO[complicated==1]
	dur_maternity[complicated==0] <- durPPUWHO[complicated==0]

	discharge1 <- labour_end + dur_maternity*60*60		# literal discharge time, could be in the middle of the night
	dischargeTime <- getDischargeTime(discharge1)		# change discharge time at night to early morning


	# get occupancy of maternity facilities and delivery rooms every hour
	breaks <- seq(as.POSIXct('2014-01-01 00:00', tz = "GMT"),by = '1 hours', length = 365*24+1)

	# get labour room occupancy
	freqLabour1Hour <- getOccupancyByCentreSnapshot(labour_start, labour_end, breaks, timeZone, facility1, 1:3)
	freqLabour2Hour <- getOccupancyByCentreSnapshot(labour_start, labour_end, breaks, timeZone, facility2, 1:6)

	# get maternity ward (post-partum) occupancy
	#freqMat1Hour <- getOccupancyByCentreSnapshot(labour_end, dischargeTime, breaks, timeZone, facility1, 1:3)
	#freqMat2Hour <- getOccupancyByCentreSnapshot(labour_end, dischargeTime, breaks, timeZone, facility2, 1:6)
	
	# LABOUR WARD OCCUPANCY HISTOGRAM
	totalHoursLW1 <- colSums(freqLabour1Hour[2:4])
	df1 <- lapply(freqLabour1Hour[,2:4], factor, levels=unique(unlist(freqLabour1Hour[2:4])))
	temp1 <- sapply(df1, function(x) prop.table(table(x)))	
	LW1byHour <- as.data.frame(sweep(temp1, 2, FUN="*", totalHoursLW1))
	LW1byHour$Women <- as.numeric(rownames(LW1byHour))

	totalHoursLW2 <- colSums(freqLabour2Hour[2:7])
	df2 <- lapply(freqLabour2Hour[,2:7], factor, levels=unique(unlist(freqLabour2Hour[2:7])))
	temp2 <- sapply(df2, function(x) prop.table(table(x)))	
	LW2byHour <- as.data.frame(sweep(temp2, 2, FUN="*", totalHoursLW2))
	LW2byHour$Women <- as.numeric(rownames(LW2byHour))
	

	# DETERMINE HOW MUCH OF THE TIME LABOUR ROOMS ARE EMPTY
	hoursLWEmpty1 <- colSums(freqLabour1Hour[, 2:ncol(freqLabour1Hour)]==0)
	percentTimeLWEmpty1 <- hoursLWEmpty1 / nrow(freqLabour1Hour) * 100
	hoursLWEmpty2 <- colSums(freqLabour2Hour[, 2:ncol(freqLabour2Hour)]==0)
	percentTimeLWEmpty2 <- hoursLWEmpty2 / nrow(freqLabour2Hour) * 100

	# DETERMINE HOW MUCH OF THE TIME MATERNITY ROOMS ARE EMPTY
	#hoursMWEmpty1 <- colSums(freqMat1Hour[, 2:ncol(freqMat1Hour)]==0)
	#percentTimeMWEmpty1 <- hoursMWEmpty1 / nrow(freqMat1Hour) * 100
	#hoursMWEmpty2 <- colSums(freqMat2Hour[, 2:ncol(freqMat2Hour)]==0)
	#percentTimeMWEmpty2 <- hoursMWEmpty2 / nrow(freqMat2Hour) * 100

	# DETERMINE BIRTHS PER SBA PER YEAR
	birthsPerSBA1 <- as.vector(table(facility1) / nrSBAbyFac1)
	df.birthsPerSBA1 <- data.frame(Facility=paste("Facility", 1:3), BirthsPerSBA=birthsPerSBA1)
	df.birthsPerSBA1$Facility <- factor(df.birthsPerSBA1$Facility, levels=paste("Facility", 1:3))

	birthsPerSBA2 <- as.vector(table(facility2) / nrSBAbyFac2)
	df.birthsPerSBA2 <- data.frame(Facility=paste("Facility", 1:6), BirthsPerSBA=birthsPerSBA2)
	df.birthsPerSBA2$Facility <- factor(df.birthsPerSBA2$Facility, levels=paste("Facility", 1:6))

	# append results to lists
	LW1.ll[[i]] <- LW1byHour
	LW2.ll[[i]] <- LW2byHour

	emptyLW1.ll[[i]] <- percentTimeLWEmpty1
	emptyLW2.ll[[i]] <- percentTimeLWEmpty2
	#emptyMW1.ll[[i]] <- percentTimeMWEmpty1
	#emptyMW2.ll[[i]] <- percentTimeMWEmpty2

	birthsPerSBA1.ll[[i]] <- df.birthsPerSBA1
	birthsPerSBA2.ll[[i]] <- df.birthsPerSBA2
}

end <- proc.time() - start
print(end)

df.LW1 <- as.data.frame(do.call(rbind, LW1.ll))
df.LW2 <- as.data.frame(do.call(rbind, LW2.ll))
df.emptyLW1 <- as.data.frame(do.call(rbind, emptyLW1.ll))
df.emptyLW2 <- as.data.frame(do.call(rbind, emptyLW2.ll))
#df.emptyMW1 <- as.data.frame(do.call(rbind, emptyMW1.ll))
#df.emptyMW2 <- as.data.frame(do.call(rbind, emptyMW2.ll))
df.birthsSBA1 <- as.data.frame(do.call(rbind, birthsPerSBA1.ll))
df.birthsSBA2 <- as.data.frame(do.call(rbind, birthsPerSBA2.ll))

# save output data
write.csv(x=df.LW1, file="WHO_S1_womenByHour.csv", row.names=FALSE)
write.csv(x=df.LW2, file="WHO_S2_womenByHour.csv", row.names=FALSE)
write.csv(x=df.emptyLW1, file="WHO_S1_pctTimeEmptyLW.csv", row.names=FALSE)
write.csv(x=df.emptyLW2, file="WHO_S2_pctTimeEmptyLW.csv", row.names=FALSE)
#write.csv(x=df.emptyMW1, file="WHO_S1_pctTimeEmptyMW.csv", row.names=FALSE)
#write.csv(x=df.emptyMW2, file="WHO_S2_pctTimeEmptyMW.csv", row.names=FALSE)
write.csv(x=df.birthsSBA1, file="WHO_S1_birthsPerSBA.csv", row.names=FALSE)
write.csv(x=df.birthsSBA2, file="WHO_S2_birthsPerSBA.csv", row.names=FALSE)


#################################################################################################################################
# PLOT PERCENT OF TIME DURING WHICH FACILITIES ARE EMPTY, BIRTHS PER SBA, WOMEN IN FACILITIES BY HOUR
#################################################################################################################################

df.emptyLW1.long <- gather(df.emptyLW1, Facility, PctTimeLWEmpty, count1:count3, factor_key=TRUE)
levels(df.emptyLW1.long$Facility) <- paste("Facility", 1:3)

p9 <- ggplot(df.emptyLW1.long, aes(x=Facility, y=PctTimeLWEmpty)) + theme_classic() + geom_boxplot() + 
	ylab("% Time delivery room is empty") + ylim(0, 100) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=14), axis.text.x=element_text(angle=45, hjust=1, vjust=1))
print(p9)
#ggsave("WHO_S1_emptyLW100.png", plot=p9, dpi=300, width=9, height=5)


df.emptyLW2.long <- gather(df.emptyLW2, Facility, PctTimeLWEmpty, count1:count6, factor_key=TRUE)
levels(df.emptyLW2.long$Facility) <- paste("Facility", 1:6)

p10 <- ggplot(df.emptyLW2.long, aes(x=Facility, y=PctTimeLWEmpty)) + theme_classic() + geom_boxplot() + 
	ylab("% Time delivery room is empty") + ylim(0, 100) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text=element_text(size=14), axis.text.x=element_text(angle=45, hjust=1, vjust=1))
print(p10)
#ggsave("WHO_S2_emptyLW100.png", plot=p10, dpi=300, width=9, height=5)


birthsRequired <- 175
p11 <- ggplot(df.birthsSBA1, aes(x=Facility, y=BirthsPerSBA)) + theme_classic() + geom_boxplot() +
	geom_hline(yintercept=birthsRequired, colour="red", size=1.5) + ylab("Births per SBA per year") +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1))
print(p11)
#ggsave("WHO_S1_birthsPerSBA.png", plot=p11, dpi=300, width=9, height=5)


p12 <- ggplot(df.birthsSBA2, aes(x=Facility, y=BirthsPerSBA)) + theme_classic() + geom_boxplot() +
	geom_hline(yintercept=birthsRequired, colour="red", size=1.5) + ylab("Births per SBA per year") +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1))
print(p12)
#ggsave("WHO_S2_birthsPerSBA.png", plot=p12, dpi=300, width=9, height=5)


df.LW1.long <- gather(df.LW1, Facility, Hours, count1:count3, factor_key=TRUE)
df.LW1.long$Women <- factor(df.LW1.long$Women, levels=unique(df.LW1.long$Women))
levels(df.LW1.long$Facility) <- paste("Facility", 1:3)

p13 <- ggplot(df.LW1.long, aes(x=Women, y=Hours, colour=Facility)) + theme_classic() + geom_boxplot() +
	ylab("Number of hours") + xlab("Number of women in delivery room") + scale_colour_manual(values=c("red", "dodgerblue", "deepskyblue")) +
	theme(axis.title=element_text(size=16), axis.text.y=element_text(size=16), axis.text.x=element_text(size=16, angle=45, hjust=1, vjust=1), legend.title=element_text(size=16), legend.text=element_text(size=16))
print(p13)
#ggsave("WHO_S1_hoursWomen.png", plot=p13, dpi=300, width=12, height=9)


df.LW2.long <- gather(df.LW2, Facility, Hours, count1:count6, factor_key=TRUE)
df.LW2.long$Women <- factor(df.LW2.long$Women, levels=unique(df.LW2.long$Women))
levels(df.LW2.long$Facility) <- paste("Facility", 1:6)

p14 <- ggplot(df.LW2.long, aes(x=Women, y=Hours, colour=Facility)) + theme_classic() + geom_boxplot() +
	ylab("Number of hours") + xlab("Number of women in delivery room") + scale_colour_manual(values=c("red", "dodgerblue", "deepskyblue", "cadetblue", "blue", "navy")) +
	theme(axis.title=element_text(size=16), axis.text.y=element_text(size=16), axis.text.x=element_text(size=16, angle=45, hjust=1, vjust=1), legend.title=element_text(size=16), legend.text=element_text(size=16))
print(p14)
#ggsave("WHO_S2_hoursWomen.png", plot=p14, dpi=300, width=12, height=9)




