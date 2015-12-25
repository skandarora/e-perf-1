## Loading original data set
df.original <- read.csv(file = "HMXPC13_DI_v2_5-14-14.csv", header = TRUE, sep=",", dec=".",
                        na.strings = c("Not Available", NA, "")) # 641138 enrollers

## Removing 'registered' & 'roles' field completely
df.original <- subset(df.original, select = -c(registered, roles))

## Splitting 'course_id' into 3 parts
split <- data.frame(do.call('rbind', strsplit(as.character(df.original$course_id),"/", fixed = TRUE))) #New 3 columns
df.original_1 <- cbind(split, subset(df.original, select=-c(course_id))) # New 3 columns cbinded with old data
rm(df.original, split)  # Removing variables that are no longer required 
colnames(df.original_1)[1:3] <- c("university", "course", "semester") # Renaming the newly created columns

## Replacing NA 'grade' with 0 : 57,400 enrollers
bad <- is.na(df.original_1$grade) # students with NA grades
df.original_1[bad,"grade"] <- 0 
rm(bad)

## Replacing NA in LoE_DI by "Unknown"
bad <- is.na(df.original_1$LoE_DI)
df.original_1 <- transform(df.original_1, LoE_DI = as.character(LoE_DI))
df.original_1[bad,"LoE_DI"] <- "Unknown"
df.original_1 <- transform(df.original_1, LoE_DI = as.factor(LoE_DI))
rm(bad)

## Replacing NA in gender by "Unknown"
bad <- is.na(df.original_1$gender)
df.original_1 <- transform(df.original_1, gender = as.character(gender))
df.original_1[bad,"gender"] <- "Unknown"
df.original_1 <- transform(df.original_1, gender = as.factor(gender))
rm(bad)


## Replacing NA in last_event by corresponding start_time_DI
bad <- is.na(df.original_1$last_event_DI)
df.original_1 <- transform(df.original_1, last_event_DI = as.character(last_event_DI))
df.original_1[bad,"last_event_DI"] <- as.character(df.original_1[bad,"start_time_DI"])
rm(bad)

## Coercing star_time_DI to Date type
df.original_1 <- transform(df.original_1, start_time_DI = as.Date(start_time_DI))
## Coercing last_event_DI to Date type
df.original_1 <- transform(df.original_1, last_event_DI = as.Date(last_event_DI))

## Replacing NA nevents by 0
bad <- is.na(df.original_1$nevents)
df.original_1[bad,"nevents"] <- 0
rm(bad)

## Replacing NA in ndays_act by 0
bad <- is.na(df.original_1$ndays_act)
df.original_1[bad,"ndays_act"] <- 0
rm(bad)

## Remove CS50x if necessary
# Total = 169621 enrollers in CS50
# grade= 0 or 1 discrete
# nforum_posts = 0
# nvideos = 0 but have values for nchapters, ndays_act, nevents
# Has 68,862 incomplete_flag records(Total incomplete_flag recods = 100161)
bad <- df.original_1$course == "CS50x"
df.original_1 <- df.original_1[!bad, ] # 471517 enrollers left
rm(bad)

## Remove inconsistent data(incomplete_flag) if necessary
# Total = 100161
# grade = 0
# nforum_posts = Almost all 0s
# nevents = 0 but have values for ndays_act, nchapters, nevents, nplay_videos
bad <- is.na(df.original_1$incomplete_flag)
df.original_1[bad,"incomplete_flag"] <- 0
rm(bad)
bad <- (df.original_1$incomplete_flag == 1)
df.original_1 <- df.original_1[!bad, ] # 440218 enrollers left
rm(bad)


## Spliting dataset into :MIT & Harvard
mit_data <- subset(df.original_1, university == "MITx") # 280139 (After removing cs50+inconsistent)
harvard_data <- subset(df.original_1, university == "HarvardX") # 160079 (After removing cs50+inconsistent)



