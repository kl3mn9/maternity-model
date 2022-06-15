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
library(ggplot2)		# for plotting
library(tidyr)		# for data formating
library(cowplot)		# for composite plots

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

#################################################################################################################################
# OCCUPANCY AT SNAPSHOTS IN TIME (4:00, 12:00, 20:00)
#################################################################################################################################

# times to generate 'snapshots', 
# i.e. times at which to establish delivery room and maternity ward occupancy
# 3 snapshots per day over a year
breaks <- seq(as.POSIXct('2014-01-01 00:00', tz = timeZone), by = '1 hours', length = 365*24+1)
temp <- format(breaks,"%H:%M:%S")
breaks3 <- breaks[temp=="04:00:00" | temp=="12:00:00" |temp=="20:00:00"]


# all births 
dischargeTime <- getDischargeTime(discharge1Z)

facility.numbers <- seq(1, numFacilities)
# omit facilities 30 and 32 because zero deliveries were recorded
# this is specific to Zanzibar
facility.numbers <- facility.numbers[-c(30, 32)]

# Occupancy maternity centre (including time spent in delivery room and time in post-partum room
freqZ <- getOccupancyByCentreSnapshot(labour_start, dischargeTime, breaks3, timeZone, facility, facility.numbers)

# Occupancy delivery room 
freqLabourZ <- getOccupancyByCentreSnapshot(labour_start, labour_end, breaks3, timeZone, facility, facility.numbers)

# Occupancy post-partum room
freqMatZ <- getOccupancyByCentreSnapshot(labour_end, dischargeTime, breaks3, timeZone, facility, facility.numbers)


# complicated births only
labour_start_comp <- labour_start[complicatedZ==1]
labour_end_comp <- labour_end[complicatedZ==1]
dischargeTime_comp <- dischargeTime[complicatedZ==1]

facility.numbers.c <- seq(1, numFacilities)
# only consider facilities carrying out c-sections
facility.numbers.c <- facility.numbers.c[data$csects > 0]
facility.c <- facility[complicatedZ==1]

# Occupancy maternity centre (including time spent in delivery room and time in post-partum room
freqZ_comp <- getOccupancyByCentreSnapshot(labour_start_comp, dischargeTime_comp, breaks3, timeZone, facility.c, facility.numbers.c)

# Occupancy delivery room 
freqLabourZ_comp <- getOccupancyByCentreSnapshot(labour_start_comp, labour_end_comp, breaks3, timeZone, facility.c, facility.numbers.c)

# Occupancy post-partum room
freqMatZ_comp <- getOccupancyByCentreSnapshot(labour_end_comp, dischargeTime_comp, breaks3, timeZone, facility.c, facility.numbers.c)


#################################################################################################################################
# PLOT SNAPSHOTS OF WOMEN IN FACILITIES, DELIVERY ROOM AND POST-PARTUM ROOM
#################################################################################################################################

# WOMEN IN FACILITIES
# Plot total number of births and complicated births over a representative month (snapshots, facility occupancy)
df.total.snaps <- data.frame(Snapshot=breaks3, TotalBirths=rowSums(freqZ[, 2:ncol(freqZ)]), CompBirths=rowSums(freqZ_comp[,2:ncol(freqZ_comp)]))
df.total.snaps$UncompBirths <- df.total.snaps$TotalBirths - df.total.snaps$CompBirths

# 178:270 are rownumbers of snapshots in March
df.total.snaps.long <- gather(df.total.snaps[178:270, -2], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

p1 <- ggplot(df.total.snaps.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a month") + ylab("Number of women in EmOCs") + 
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=16))
print(p1)
#ggsave("TotalBirthsMonthSingle.png", plot=p1, dpi=300, width=12, height=8)


# Plot number of births and complicated births over a representative month by facility (snapshots, facility occupancy)
freqZ.long <- gather(freqZ[178:270,], Facility, NumBirths, count1:count37)
freqZ_comp.long <- gather(freqZ_comp[178:270,], Facility, CompBirths, count5:count36)
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
	xlab("Time - snapshots every 8h over a month") + ylab("Number of women in EmOCs") + facet_wrap(~ Facility, ncol=5) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
#print(p2)
#ggsave("BirthsFacilityMonthSingle.png", plot=p2, dpi=300, width=18, height=18)


# WOMEN IN DELIVERY ROOMS
# Plot total number of births and complicated births over a representative month (snapshots, facility occupancy)
df.del.snaps <- data.frame(Snapshot=breaks3, TotalBirths=rowSums(freqLabourZ[, 2:ncol(freqLabourZ)]), CompBirths=rowSums(freqLabourZ_comp[,2:ncol(freqLabourZ_comp)]))
df.del.snaps$UncompBirths <- df.del.snaps$TotalBirths - df.del.snaps$CompBirths

# 178:270 are rownumbers of snapshots in March
df.del.snaps.long <- gather(df.del.snaps[178:270, -2], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

p3 <- ggplot(df.del.snaps.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a month") + ylab("Number of women in delivery rooms") + 
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=16))
print(p3)
#ggsave("TotalDelMonthSingle.png", plot=p3, dpi=300, width=12, height=8)


# Plot number of births and complicated births over a representative month by facility (snapshots, facility occupancy)
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

p4 <- ggplot(df.del.fac.snaps.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a month") + ylab("Number of women in delivery rooms") + facet_wrap(~ Facility, ncol=5) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
#print(p4)
#ggsave("DelFacilityMonthSingle.png", plot=p4, dpi=300, width=18, height=18)


# WOMEN IN MATERNITY BEDS (POST-PARTUM)
# Plot total number of births and complicated births over a representative month (snapshots, facility occupancy)
df.mat.snaps <- data.frame(Snapshot=breaks3, TotalBirths=rowSums(freqMatZ[, 2:ncol(freqMatZ)]), CompBirths=rowSums(freqMatZ_comp[,2:ncol(freqMatZ_comp)]))
df.mat.snaps$UncompBirths <- df.mat.snaps$TotalBirths - df.mat.snaps$CompBirths

# 178:270 are rownumbers of snapshots in March
df.mat.snaps.long <- gather(df.mat.snaps[178:270, -2], BirthType, NumBirths, CompBirths, UncompBirths, factor_key=TRUE)

p5 <- ggplot(df.mat.snaps.long, aes(x=Snapshot, y=NumBirths, fill=BirthType)) + theme_classic() + geom_bar(stat="identity") +
	scale_fill_manual(values=c("springgreen", "mediumpurple"), labels=c("Complicated births", "Uncomplicated births"), name="") +
	xlab("Time - snapshots every 8h over a month") + ylab("Number of women in post-partum beds") + 
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=16))
print(p5)
#ggsave("TotalMatMonthSingle.png", plot=p5, dpi=300, width=12, height=8)


# Plot number of births and complicated births over a representative month by facility (snapshots, facility occupancy)
freqMatZ.long <- gather(freqMatZ[178:270,], Facility, NumBirths, count1:count37)
freqMatZ_comp.long <- gather(freqMatZ_comp[178:270,], Facility, CompBirths, count5:count36)
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
	xlab("Time - snapshots every 8h over a month") + ylab("Number of women in post-partum beds") + facet_wrap(~ Facility, ncol=5) +
	theme(axis.title=element_text(size=18), axis.text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1, vjust=1), legend.text=element_text(size=16), strip.text=element_text(size=16))
#print(p6)
#ggsave("MatFacilityMonthSingle.png", plot=p6, dpi=300, width=18, height=18)



#################################################################################################################################
# PROPORTION OF TIME EACH FACILITY IS OVER CAPACITY OR EMPTY --> EVALUATE CAPACITY EVERY HOUR RATHER THAN EVERY 8 HOURS
#################################################################################################################################

# repeat n times to get mean and 95% credible interval
n <- 100
overLW.ll <- list()
emptyLW.ll <- list()
birthsPerSBA.ll <- list()
overSBA.ll <- list()

#overMW.ll <- list()
#emptyMW.ll <- list()

start <- proc.time()

for(i in 1:n)
{
	# randomly draw distributions of duration in delivery and postpartum rooms 
	# for births=annual births in Zanzibar
	durDelUncomp <- rgamma(n=births,shape=shapeDelU,scale=scaleDelU)				#duration in delivery room, uncomplicated delivery
	durDelComp <- factor_dur_comp*rgamma(n=births,shape=shapeDelU,scale=scaleDelU)	#duration in delivery room, complicated delivery
	durPPUZ <- rgamma(n=births,shape=shapePPUZ,scale=scalePPUZ)		#duration postpartum, uncomplicated delivery, Zanzibar analysis
	durPPCZ <- rgamma(n=births,shape=shapePPCZ,scale=scalePPCZ)		#duration postpartum, complicated delivery, Zanzibar analysis

	temp3 <- latemail(births)			#each woman set a random date and time of presentation in labour at a health facility
								#the vector orders them by date (for one year - 2014)
	tz(temp3) <- timeZone				# set time zone, e.g. to East African Time (for Zanzibar)
	labour_start <- temp3[sample(births)]	#labour_start dates and times in random order (means date/time of admission to labour ward)


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
	freqLabourZHour <- getOccupancyByCentreSnapshot(labour_start, labour_end, breaks, timeZone, facility, facility.numbers)
	# get maternity ward (post-partum) occupancy
	#freqMatZHour <- getOccupancyByCentreSnapshot(labour_end, dischargeTime, breaks, timeZone, facility, facility.numbers)	

	# How can there be centres with labour ward capacity 0? Do women get transferred there from other centres after labour?
	capacityLW <- data$beds_deliv[-c(30, 32)]		
	capacityMW <- data$beds_matern[-c(30, 32)]
	capacity <- capacityLW + capacityMW

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
	capacitySBA_am <- data$SBA_am[-c(30,32)]
	hoursLWOverSBA_am <- colSums(sweep(freqLabourZHour[format(freqLabourZHour$Snapshot, "%p")=="am", 2:ncol(freqLabourZHour)], 2, capacitySBA_am, ">"))
	capacitySBA_pm <- data$SBA_pm[-c(30,32)]
	hoursLWOverSBA_pm <- colSums(sweep(freqLabourZHour[format(freqLabourZHour$Snapshot, "%p")=="pm", 2:ncol(freqLabourZHour)], 2, capacitySBA_pm, ">"))

	hoursLWOverSBA <- hoursLWOverSBA_am + hoursLWOverSBA_pm 
	overSBA.ll[[i]] <- hoursLWOverSBA / nrow(freqLabourZHour) * 100
}

end <- proc.time() - start
print(end)

df.overLW <- as.data.frame(do.call(rbind, overLW.ll))
df.emptyLW <- as.data.frame(do.call(rbind, emptyLW.ll))
df.overSBA <- as.data.frame(do.call(rbind, overSBA.ll))

# save output data
write.csv(x=df.overLW, file="pctTimeExceedLWCapacity.csv", row.names=FALSE)
write.csv(x=df.emptyLW, file="pctTimeEmptyLW.csv", row.names=FALSE)
write.csv(x=df.overSBA, file="pctTimeExceedSBACapacity.csv", row.names=FALSE)


#################################################################################################################################
# PLOT PERCENT OF TIME DURING WHICH FACILITIES EXCEED CAPACITY OR ARE EMPTY
#################################################################################################################################

df.overLW.long <- gather(df.overLW, Facility, PctTimeOverLWCapacity, count1:count37, factor_key=TRUE)
levels(df.overLW.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p7 <- ggplot(df.overLW.long, aes(x=Facility, y=PctTimeOverLWCapacity)) + theme_classic() + geom_boxplot() + 
	ylab("% Time bed capacity in delivery room exceeded") + ylim(0, 100) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_blank())
print(p7)
#ggsave("bedCapacityExceeded100.png", plot=p7, dpi=300, width=18, height=9)

df.emptyLW.long <- gather(df.emptyLW, Facility, PctTimeLWEmpty, count1:count37, factor_key=TRUE)
levels(df.emptyLW.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p8 <- ggplot(df.emptyLW.long, aes(x=Facility, y=PctTimeLWEmpty)) + theme_classic() + geom_boxplot() + 
	ylab("% Time delivery room is empty") + ylim(0, 100) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_blank())
print(p8)
#ggsave("emptyLW100.png", plot=p8, dpi=300, width=18, height=9)


df.overSBA.long <- gather(df.overSBA, Facility, PctTimeOverSBACapacity, count1:count37, factor_key=TRUE)
levels(df.overSBA.long$Facility) <- paste("Facility", seq(1, 37)[-c(30,32)])

p9 <- ggplot(df.overSBA.long, aes(x=Facility, y=PctTimeOverSBACapacity)) + theme_classic() + geom_boxplot() + 
	ylab("% Time when more women than SBAs in delivery room") + ylim(0, 100) +
	theme(axis.title.x=element_blank(), axis.title.y=element_text(size=14), axis.text.y=element_text(size=14), axis.text.x=element_text(size=12, angle=45, hjust=1, vjust=1))
print(p9)
#ggsave("SBACapacityExceeded100.png", plot=p9, dpi=300, width=18, height=9)

pic <- plot_grid(p7, p8, p9, labels=c('a', 'b', 'c'), nrow=3)
#save_plot(filename="capacityLW100.png", plot=pic, base_height=18, base_width=15, dpi=300)



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
#ggsave("birthsPerSBA.png", plot=p10, dpi=300, width=18, height=9)


