# EDA & DP

## Loading original data set
df.original <- read.csv(file = "HMXPC13_DI_v2_5-14-14.csv", header = TRUE, sep=",", dec=".",
                        na.strings = c("Not Available", NA, "")) # 641138 enrollers

## Splitting 'course_id' into 3 parts
split <- data.frame(do.call('rbind', strsplit(as.character(df.original$course_id),"/", fixed = TRUE))) #New 3 columns
df.original_1 <- cbind(split, subset(df.original, select=-c(course_id))) # New 3 columns cbinded with old data
rm(df.original, split)  # Removing variables that are no longer required 
colnames(df.original_1)[1:3] <- c("university", "course", "semester") # Renaming the newly created columns
df.original <- df.original_1
rm(df.original_1)

## Removing 'registered' & 'roles' field completely
df.original <- subset(df.original, select = -c(registered, roles))

## Spliting dataset into :MIT & Harvard
mit <- subset(df.original, university == "MITx") 
harvard <- subset(df.original, university == "HarvardX") 


## ---------------------------------------MIT DP----------------------------------------------

## Remove NA in grade (as they were redacted during deidentification process)
bad_grade <- is.na(mit$grade)
mit <- mit[!bad_grade,]
rm(bad_grade)

## Replace NA in nevents by 0 (as they rep no interaction after reg date)
bad_nevents <- is.na(mit$nevents)
mit[bad_nevents,"nevents"] <- 0
rm(bad_nevents)

## Remove incomplete flags (as they rep inconsistencies)
nas <- is.na(mit$incomplete_flag)
mit[nas,"incomplete_flag"] <- 0
mit <- subset(mit, incomplete_flag == 0)
rm(nas)
mit <- subset(mit, select = -c(incomplete_flag))

## Replacing NAs in nplay_video by 0
bad_vid <- is.na(mit$nplay_video)
mit[bad_vid,"nplay_video"] <- 0
rm(bad_vid)

## Replacing NAs in nchapters by 0
bad_nchap <- is.na(mit$nchapters)
mit[bad_nchap,"nchapters"] <- 0
rm(bad_nchap)


## Creating a derived variable: % of chapters explored
# 14.73x - Max Chapters = 12
# 2.01x - Max chapters = 13
# 3.091x - 22
# 6.002x - 21
# 6.00x - 19
# 7.00x - 18
# 8.02x - 21
# 8.MReV - 48
mit$d_maxChapters[mit$course == "8.MReV"]  <- 48
mit$d_maxChapters[mit$course == "8.02x"]  <- 21
mit$d_maxChapters[mit$course == "7.00x"]  <- 18
mit$d_maxChapters[mit$course == "6.00x"]  <- 19
mit$d_maxChapters[mit$course == "6.002x"]  <- 21
mit$d_maxChapters[mit$course == "3.091x"]  <- 22
mit$d_maxChapters[mit$course == "2.01x"]  <- 13
mit$d_maxChapters[mit$course == "14.73x"]  <- 12

mit$d_percentChapters <- mit$nchapters/mit$d_maxChapters 
mit$d_percentChapters <- round(mit$d_percentChapters, digits=2) # Rounding off

## Handling NAs in ndays_act
# Some rows have n_days=0 but have values for viewed and grade; They are removed
# For others NA is replaced by 0 (as all other activity variables have 0 for them)
bad <- is.na(mit$ndays_act)
bad2 <- which(mit[bad,"viewed"] == 1)
mit <- mit[-bad2,] # inconsistent values removed

bad <- is.na(mit$ndays_act)
mit[bad,"ndays_act"] <- 0 # Others replaced by 0
rm(bad, bad2)


## Replacing NA in last_event by corresponding start_time_DI
bad <- is.na(mit$last_event_DI)
mit <- transform(mit, last_event_DI = as.character(last_event_DI))
mit[bad,"last_event_DI"] <- as.character(mit[bad,"start_time_DI"])
rm(bad)

## Coercing star_time_DI to Date type
mit <- transform(mit, start_time_DI = as.Date(start_time_DI))
## Coercing last_event_DI to Date type
mit <- transform(mit, last_event_DI = as.Date(last_event_DI))

## Creating derived variable: age 
# Assuming most enrollers provided their YOB  at time of registation
# Age in not exactly accurate bcz of info removed during de-identification
# But its close enough
mit$d_age <- as.numeric(format(mit$start_time_DI,"%Y")) - as.numeric(mit$YoB)

## Creating a derived variable: days before/after course launch did the person join the course
# Minus indicates enroller joined before the course launch date
#Course launch dates:
#6.002x - Fall - 5/Sep/12
#6.002x - Spring - 3/Mar/13
#6.00x - Fall - 26/Sep/12
#6.00x - Spring - 4/Feb/13
#3.091x - Fall - 9/Oct/12
#3.091x - Spring - 5/Feb/13
#14.73x - Spring - 12/Feb/13
#8.02x - Spring - 18/Feb/13
#7.00x - Spring -  5/Mar/13
#2.01x - Spring - 15/Apr/13
#8.MReV - Summer - 1/Jun/13
mit$d_courseLaunchDate[mit$course == "6.002x" & mit$semester == "2012_Fall"] <-"2012-09-05"
mit$d_courseLaunchDate[mit$course == "6.002x" & mit$semester == "2013_Spring"] <-"2013-03-03"
mit$d_courseLaunchDate[mit$course == "6.00x" & mit$semester == "2012_Fall"] <-"2012-09-26"
mit$d_courseLaunchDate[mit$course == "6.00x" & mit$semester == "2013_Spring"] <-"2013-02-04"
mit$d_courseLaunchDate[mit$course == "3.091x" & mit$semester == "2012_Fall"] <-"2012-10-09"
mit$d_courseLaunchDate[mit$course == "3.091x" & mit$semester == "2013_Spring"] <-"2013-02-05"
mit$d_courseLaunchDate[mit$course == "14.73x" & mit$semester == "2013_Spring"] <-"2013-02-12"
mit$d_courseLaunchDate[mit$course == "8.02x" & mit$semester == "2013_Spring"] <-"2013-02-18"
mit$d_courseLaunchDate[mit$course == "6.002x" & mit$semester == "2013_Spring"] <-"2013-03-03"
mit$d_courseLaunchDate[mit$course == "7.00x" & mit$semester == "2013_Spring"] <-"2013-03-05"
mit$d_courseLaunchDate[mit$course == "2.01x" & mit$semester == "2013_Spring"] <-"2013-04-15"
mit$d_courseLaunchDate[mit$course == "8.MReV" & mit$semester == "2013_Summer"] <-"2013-06-01"

# Coercing d_courseLaunchDate  to Date type
mit <- transform(mit, d_courseLaunchDate = as.Date(d_courseLaunchDate))

# Creating variable now
mit$d_timeDiff_FirstInteraction <- mit$start_time_DI - mit$d_courseLaunchDate

## Creating a derived variable: days before/after course WRAP UP did the person stopped interacting
#Course Wrap-Up dates:
#6.002x - Fall - 25/Dec/12
#6.002x - Spring - 01/Jul/13
#6.00x - Fall - 15/Jan/13
#6.00x - Spring - 04/Jun/13
#3.091x - Fall - 15/Jan/13
#3.091x - Spring - 21/Jun/13 
#14.73x - Spring - 21/May/13
#8.02x - Spring - 18/Jun/13
#7.00x - Spring -  06/Jun/13
#2.01x - Spring - 30/Jul/13
#8.MReV - Summer - 15/Sep/13
mit$d_courseWrapDate[mit$course == "6.002x" & mit$semester == "2012_Fall"] <-"2012-12-25"
mit$d_courseWrapDate[mit$course == "6.002x" & mit$semester == "2013_Spring"] <-"2013-07-01"
mit$d_courseWrapDate[mit$course == "6.00x" & mit$semester == "2012_Fall"] <-"2013-01-15"
mit$d_courseWrapDate[mit$course == "6.00x" & mit$semester == "2013_Spring"] <-"2013-06-04"
mit$d_courseWrapDate[mit$course == "3.091x" & mit$semester == "2012_Fall"] <-"2013-01-15"
mit$d_courseWrapDate[mit$course == "3.091x" & mit$semester == "2013_Spring"] <-"2013-06-21"
mit$d_courseWrapDate[mit$course == "14.73x" & mit$semester == "2013_Spring"] <-"2013-05-21"
mit$d_courseWrapDate[mit$course == "8.02x" & mit$semester == "2013_Spring"] <-"2013-06-18"
mit$d_courseWrapDate[mit$course == "6.002x" & mit$semester == "2013_Spring"] <-"2013-07-01"
mit$d_courseWrapDate[mit$course == "7.00x" & mit$semester == "2013_Spring"] <-"2013-06-06"
mit$d_courseWrapDate[mit$course == "2.01x" & mit$semester == "2013_Spring"] <-"2013-07-30"
mit$d_courseWrapDate[mit$course == "8.MReV" & mit$semester == "2013_Summer"] <-"2013-09-15"

# Coercing d_courseWrapDate  to Date type
mit <- transform(mit, d_courseWrapDate = as.Date(d_courseWrapDate))

# Creating variable now
mit$d_timeDiff_LastInteraction <- mit$d_courseWrapDate - mit$last_event_DI
# Minus indicate enroller last interacted after course was wrapped up

# Adding categorical variables to ease slicing and dicing
mit$d_explored <- ifelse(mit$explored == 1, "explored", "not explored")
mit$d_viewed <- ifelse(mit$viewed == 1, "vieweded", "not viewed")
mit$d_certified <- ifelse(mit$certified == 1, "certified", "not certified")


## --------------------------------------Harvard DP--------------------------------------------

## Remove NA in grade (as they were redacted during deidentification process)
bad_grade <- is.na(harvard$grade)
harvard <- harvard[!bad_grade,]
rm(bad_grade)

## Remove incomplete flags (as they rep inconsistencies)
nas <- is.na(harvard$incomplete_flag)
harvard[nas,"incomplete_flag"] <- 0
harvard <- subset(harvard, incomplete_flag == 0)
rm(nas)
harvard <- subset(harvard, select = -c(incomplete_flag))

## Remove CS50x course completely
#harvard <- harvard[harvard$course != "CS50x",]

## Replace NA in nevents by 0 (as they rep no interaction after reg date)
bad_nevents <- is.na(harvard$nevents)
harvard[bad_nevents,"nevents"] <- 0
rm(bad_nevents)

## Replacing NAs in nplay_video by 0
bad_vid <- is.na(harvard$nplay_video)
harvard[bad_vid,"nplay_video"] <- 0
rm(bad_vid)

## Replacing NAs in nchapters by 0
bad_nchap <- is.na(harvard$nchapters)
harvard[bad_nchap,"nchapters"] <- 0
rm(bad_nchap)


## Creating a derived variable: % of chapters explored
# PH207x - Max Chapters = 16
# CB22x - Max chapters = 27
# ER22x - 34
# PH278x - 11
# CS50x - 12
# NOTE: 
# nplay_video is zero && nforum_posts in nearly 0 in all harvard courses & grade is either 0 or 1
# EXCEPT PH207x & PH278x. But their nforum_posts are still low

harvard$d_maxChapters[harvard$course == "PH207x"]  <- 16
harvard$d_maxChapters[harvard$course == "CB22x"]  <- 27
harvard$d_maxChapters[harvard$course == "ER22x"]  <- 34
harvard$d_maxChapters[harvard$course == "PH278x"]  <- 11
harvard$d_maxChapters[harvard$course == "CS50x"]  <- 12

harvard$d_percentChapters <- harvard$nchapters/harvard$d_maxChapters 
harvard$d_percentChapters <- round(harvard$d_percentChapters, digits=2) # Rounding off

## Handling NAs in ndays_act
# Some rows have n_days=0 but have values for viewed and grade; They are removed
# For others NA is replaced by 0 (as all other activity variables have 0 for them)
bad <- is.na(harvard$ndays_act)
bad2 <- which(harvard[bad,"viewed"] == 1)
harvard <- harvard[-bad2,] # inconsistent values removed

bad <- is.na(harvard$ndays_act)
harvard[bad,"ndays_act"] <- 0 # Others replaced by 0
rm(bad, bad2)


## Replacing NA in last_event by corresponding start_time_DI
bad <- is.na(harvard$last_event_DI)
harvard <- transform(harvard, last_event_DI = as.character(last_event_DI))
harvard[bad,"last_event_DI"] <- as.character(harvard[bad,"start_time_DI"])
rm(bad)

## Coercing star_time_DI to Date type
harvard <- transform(harvard, start_time_DI = as.Date(start_time_DI))
## Coercing last_event_DI to Date type
harvard <- transform(harvard, last_event_DI = as.Date(last_event_DI))

## Creating derived variable: age 
# Assuming most enrollers provided their YOB  at time of registation
# Age in not exactly accurate bcz of info removed during de-identification
# But its close enough
harvard$d_age <- as.numeric(format(harvard$start_time_DI,"%Y")) - as.numeric(harvard$YoB)

## Creating a derived variable: days before/after course launch did the person join the course
# Minus indicates enroller joined before the course launch date
#Course launch & wrap up dates:
# PH207x - 15/Oct/12 , 30/Jan/13
# CB22x - 13/Mar/13, 26/Aug/13
# ER22x - 2/Mar/13, 26/Jul/13
# PH278x - 15/May/13, 25/Jul/13
# CS50x - 15/Oct/12, 15/Apr/13

harvard$d_courseLaunchDate[harvard$course == "PH207x" & harvard$semester == "2012_Fall"] <-"2012-10-15"
harvard$d_courseLaunchDate[harvard$course == "CB22x" & harvard$semester == "2013_Spring"] <-"2013-03-13"
harvard$d_courseLaunchDate[harvard$course == "ER22x" & harvard$semester == "2013_Spring"] <-"2013-03-02"
harvard$d_courseLaunchDate[harvard$course == "PH278x" & harvard$semester == "2013_Spring"] <-"2013-05-15"
harvard$d_courseLaunchDate[harvard$course == "CS50x" & harvard$semester == "2012"] <-"2012-10-15"

# Coercing d_courseLaunchDate  to Date type
harvard <- transform(harvard, d_courseLaunchDate = as.Date(d_courseLaunchDate))

# Creating variable now
harvard$d_timeDiff_FirstInteraction <- harvard$start_time_DI - harvard$d_courseLaunchDate

## Creating a derived variable: days before/after course WRAP UP did the person stopped interacting
#Course launch & wrap up dates:
# PH207x - 15/Oct/12 , 30/Jan/13
# CB22x - 13/Mar/13, 26/Aug/13
# ER22x - 2/Mar/13, 26/Jul/13
# PH278x - 15/May/13, 25/Jul/13
# CS50x - 15/Oct/12, 15/Apr/13

harvard$d_courseWrapDate[harvard$course == "PH207x" & harvard$semester == "2012_Fall"] <-"2013-01-30"
harvard$d_courseWrapDate[harvard$course == "CB22x" & harvard$semester == "2013_Spring"] <-"2013-08-26"
harvard$d_courseWrapDate[harvard$course == "ER22x" & harvard$semester == "2013_Spring"] <-"2013-07-26"
harvard$d_courseWrapDate[harvard$course == "PH278x" & harvard$semester == "2013_Spring"] <-"2013-07-25"
harvard$d_courseWrapDate[harvard$course == "CS50x" & harvard$semester == "2012"] <-"2013-04-15"

# Coercing d_courseWrapDate  to Date type
harvard <- transform(harvard, d_courseWrapDate = as.Date(d_courseWrapDate))

# Creating variable now
harvard$d_timeDiff_LastInteraction <- harvard$d_courseWrapDate - harvard$last_event_DI
# Minus indicate enroller last interacted after course was wrapped up

# Adding categorical variables to ease slicing and dicing
harvard$d_explored <- ifelse(harvard$explored == 1, "explored", "not explored")
harvard$d_viewed <- ifelse(harvard$viewed == 1, "vieweded", "not viewed")
harvard$d_certified <- ifelse(harvard$certified == 1, "certified", "not certified")



