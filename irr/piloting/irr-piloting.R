library(tidycomm)

# challengeCoding ------------------------------------------------------------

challengeCoding <- read.csv("data/challengeCoding.csv", stringsAsFactors = TRUE, strip.white=TRUE)

levels(challengeCoding$code_1)
levels(challengeCoding$code_2)

# Set emptry to NA
challengeCoding$code_2 <- as.factor(ifelse(challengeCoding$code_2 == "", NA, as.character(challengeCoding$code_2)))

# create date frames for code1 and code2
challengeCode1 <- as.data.frame(sapply(levels(challengeCoding$code_1), function(x) as.integer(x == challengeCoding$code_1)))

challengeCode2 <- as.data.frame(sapply(levels(challengeCoding$code_2), function(x) as.integer(x == challengeCoding$code_2)))

# Set missing data to 0
challengeCode2[is.na(challengeCode2)] <- 0

# Add missing columns
challengeCode2 <- cbind(challengeCode2, data.frame(`finding the right data` = 0))
challengeCode2 <- cbind(challengeCode2, data.frame(none = 0))

# Reorder
challengeCode2 <- challengeCode2[c(1, 7, 2, 3, 4, 8, 5, 6)]

# Combine code 1 and 2
challengeCode1 <- challengeCode1 + challengeCode2

# Merge wide coding with dataset
challengeData <- cbind(challengeCoding, challengeCode1)
challengeData <- challengeData[-c(2,3,4)]

# Runn IRR tests
challengeIrr <- test_icr(data, responseid, coder, cohens_kappa = TRUE, fleiss_kappa = TRUE)


# motivationCoding ------------------------------------------------------------

motivationCoding <- read.csv("data/motivationCoding.csv", stringsAsFactors = TRUE, strip.white=TRUE)

levels(motivationCoding$code_1)
levels(motivationCoding$code_2)

# Set emptry to NA
motivationCoding$code_2 <- as.factor(ifelse(motivationCoding$code_2 == "", NA, as.character(motivationCoding$code_2)))

# create date frames for code1 and code2
motivationCode1 <- as.data.frame(sapply(levels(motivationCoding$code_1), function(x) as.integer(x == motivationCoding$code_1)))

motivationCode2 <- as.data.frame(sapply(levels(motivationCoding$code_2), function(x) as.integer(x == motivationCoding$code_2)))

# Set missing data to 0
motivationCode2[is.na(motivationCode2)] <- 0

# check columns
colnames(motivationCode1)
colnames(motivationCode2)


# Add missing columns
motivationCode1 <- cbind(motivationCode1, data.frame(training = 0))
motivationCode2 <- cbind(motivationCode2, data.frame(covid = 0))

# Reorder
motivationCode2 <- motivationCode2[c(1, 2, 8, 3, 4, 5, 6, 7)]

# Combine code 1 and 2
motivationCode1 <- motivationCode1 + motivationCode2

# Merge wide coding with dataset
motivationData <- cbind(motivationCoding, motivationCode1)
motivationData <- motivationData[-c(3,4)]

names(motivationData)[1] <- "responseid"
# Runn IRR tests
motivationIrr <- test_icr(motivationData, responseid, coder, cohens_kappa = TRUE, fleiss_kappa = TRUE)

# dataUsedCoding ------------------------------------------------------------

dataUsedCoding <- read.csv("data/dataUsedCoding.csv", stringsAsFactors = TRUE, strip.white=TRUE)

levels(dataUsedCoding$code_1)
levels(dataUsedCoding$code_2)
levels(dataUsedCoding$code_3)
levels(dataUsedCoding$code_4)


# Set emptry to NA
dataUsedCoding$code_1 <- as.factor(ifelse(dataUsedCoding$code_1 == "", NA, as.character(dataUsedCoding$code_1)))
dataUsedCoding$code_2 <- as.factor(ifelse(dataUsedCoding$code_2 == "", NA, as.character(dataUsedCoding$code_2)))
dataUsedCoding$code_3 <- as.factor(ifelse(dataUsedCoding$code_3 == "", NA, as.character(dataUsedCoding$code_3)))
dataUsedCoding$code_4 <- as.factor(ifelse(dataUsedCoding$code_4 == "", NA, as.character(dataUsedCoding$code_4)))


# create date frames for code1 and code2
dataUsedCode1 <- as.data.frame(sapply(levels(dataUsedCoding$code_1), function(x) as.integer(x == dataUsedCoding$code_1)))

dataUsedCode2 <- as.data.frame(sapply(levels(dataUsedCoding$code_2), function(x) as.integer(x == dataUsedCoding$code_2)))

dataUsedCode3 <- as.data.frame(sapply(levels(dataUsedCoding$code_3), function(x) as.integer(x == dataUsedCoding$code_3)))
dataUsedCode4 <- as.data.frame(sapply(levels(dataUsedCoding$code_4), function(x) as.integer(x == dataUsedCoding$code_4)))

# Set missing data to 0
dataUsedCode1[is.na(dataUsedCode1)] <- 0
dataUsedCode2[is.na(dataUsedCode2)] <- 0
dataUsedCode3[is.na(dataUsedCode3)] <- 0
dataUsedCode4[is.na(dataUsedCode4)] <- 0

# check columns
colnames(dataUsedCode1)
colnames(dataUsedCode2)
colnames(dataUsedCode3)
colnames(dataUsedCode4)


# Add missing columns
dataUsedCode1 <- cbind(dataUsedCode1, data.frame(focusGroup = 0))
dataUsedCode3 <- cbind(dataUsedCode3, data.frame(focusGroup = 0, interview=0))
dataUsedCode4 <- cbind(dataUsedCode4, data.frame(fieldNotes = 0, focusGroup = 0, historical=0, interview=0, media=0, survey =0))


# Reorder
dataUsedCode1 <- dataUsedCode1[c(1, 7, 2, 3, 4, 5, 6)]
dataUsedCode3 <- dataUsedCode3[c(1, 6, 2, 7, 3, 4, 5)]
dataUsedCode4 <- dataUsedCode4[c(2, 3, 4, 5, 6, 1, 7)]


# Combine code2
dataUsedCode1 <- dataUsedCode1 + dataUsedCode2 + dataUsedCode3 + dataUsedCode4

# Merge wide coding with dataset
dataUsedData <- cbind(dataUsedCoding, dataUsedCode1)
dataUsedData <- dataUsedData[-c(3,4,5,6)]

names(dataUsedData)[1] <- "responseid"
# Runn IRR tests
dataUsedIrr <- test_icr(dataUsedData, responseid, coder, cohens_kappa = TRUE, fleiss_kappa = TRUE)


# howUsedCoding ------------------------------------------------------------

howUsedCoding <- read.csv("data/howUsedCoding.csv", stringsAsFactors = TRUE, strip.white=TRUE)

levels(howUsedCoding$code_1)
levels(howUsedCoding$code_2)
levels(howUsedCoding$code_3)



# Set emptry to NA
howUsedCoding$code_1 <- as.factor(ifelse(howUsedCoding$code_1 == "", NA, as.character(howUsedCoding$code_1)))
howUsedCoding$code_2 <- as.factor(ifelse(howUsedCoding$code_2 == "", NA, as.character(howUsedCoding$code_2)))
howUsedCoding$code_3 <- as.factor(ifelse(howUsedCoding$code_3 == "", NA, as.character(howUsedCoding$code_3)))


# create date frames for code1 and code2
howUsedCode1 <- as.data.frame(sapply(levels(howUsedCoding$code_1), function(x) as.integer(x == howUsedCoding$code_1)))

howUsedCode2 <- as.data.frame(sapply(levels(howUsedCoding$code_2), function(x) as.integer(x == howUsedCoding$code_2)))

howUsedCode3 <- as.data.frame(sapply(levels(howUsedCoding$code_3), function(x) as.integer(x == howUsedCoding$code_3)))

# Set missing data to 0
howUsedCode1[is.na(howUsedCode1)] <- 0
howUsedCode2[is.na(howUsedCode2)] <- 0
howUsedCode3[is.na(howUsedCode3)] <- 0

# check columns
colnames(howUsedCode1)
colnames(howUsedCode2)
colnames(howUsedCode3)


# Add missing columns
howUsedCode1 <- cbind(howUsedCode1, data.frame(interviewing = 0))
howUsedCode2 <- cbind(howUsedCode2, data.frame(limitations=0))
howUsedCode3 <- cbind(howUsedCode3, data.frame(coding = 0, limitations = 0, other =0))


# Reorder
howUsedCode1 <- howUsedCode1[c(1, 2, 5, 3, 4)]
howUsedCode2 <- howUsedCode2[c(1, 2, 3, 5, 4)]
howUsedCode3 <- howUsedCode3[c(1, 3, 2, 4, 5)]


# Combine code2
howUsedCode1 <- howUsedCode1 + howUsedCode2 + howUsedCode3

# Merge wide coding with dataset
howUsedData <- cbind(howUsedCoding, howUsedCode1)
howUsedData <- howUsedData[-c(3,4,5)]

names(howUsedData)[1] <- "responseid"
# Runn IRR tests
howUsedIrr <- test_icr(howUsedData, responseid, coder, cohens_kappa = TRUE, fleiss_kappa = TRUE)



# usefulnessCoding ------------------------------------------------------------

usefulnessCoding <- read.csv("data/usefulnessCoding.csv", stringsAsFactors = TRUE, strip.white=TRUE)

levels(usefulnessCoding$code_1)
levels(usefulnessCoding$code_2)
levels(usefulnessCoding$code_3)



# Set emptry to NA
usefulnessCoding$code_1 <- as.factor(ifelse(usefulnessCoding$code_1 == "", NA, as.character(usefulnessCoding$code_1)))
usefulnessCoding$code_2 <- as.factor(ifelse(usefulnessCoding$code_2 == "", NA, as.character(usefulnessCoding$code_2)))
usefulnessCoding$code_3 <- as.factor(ifelse(usefulnessCoding$code_3 == "", NA, as.character(usefulnessCoding$code_3)))


# create date frames for code1 and code2
usefulnessCode1 <- as.data.frame(sapply(levels(usefulnessCoding$code_1), function(x) as.integer(x == usefulnessCoding$code_1)))

usefulnessCode2 <- as.data.frame(sapply(levels(usefulnessCoding$code_2), function(x) as.integer(x == usefulnessCoding$code_2)))

usefulnessCode3 <- as.data.frame(sapply(levels(usefulnessCoding$code_3), function(x) as.integer(x == usefulnessCoding$code_3)))

# Set missing data to 0
usefulnessCode1[is.na(usefulnessCode1)] <- 0
usefulnessCode2[is.na(usefulnessCode2)] <- 0
usefulnessCode3[is.na(usefulnessCode3)] <- 0

# check columns
colnames(usefulnessCode1)
colnames(usefulnessCode2)
colnames(usefulnessCode3)


# Add missing columns

usefulnessCode2 <- cbind(usefulnessCode2, data.frame(availability=0, coding =0))
usefulnessCode3 <- cbind(usefulnessCode3, data.frame(analytical = 0, availability =0, experiential =0, other =0, sameData=0 ))


# Reorder

usefulnessCode2 <- usefulnessCode2[c(1, 6, 7, 2, 3, 4, 5)]
usefulnessCode3 <- usefulnessCode3[c(3, 4, 1, 5, 6, 2, 7)]


# Combine code2
usefulnessCode1 <- usefulnessCode1 + usefulnessCode2 + usefulnessCode3

# Merge wide coding with dataset
usefulnessData <- cbind(usefulnessCoding, usefulnessCode1)
usefulnessData <- usefulnessData[-c(3,4,5)]

names(usefulnessData)[1] <- "responseid"
# Run IRR tests
usefulnessIrr <- test_icr(usefulnessData, responseid, coder, cohens_kappa = TRUE, fleiss_kappa = TRUE)

