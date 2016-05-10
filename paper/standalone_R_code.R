# R code for the cleaning of the data, creation of additional variables, subsetting and lagging of variables
# Author: Malte Berneaud
# Date: 30.04.2016

# Packages needed for the execution of this Rmd are listed in include.packages and checked against the installed packages on the machine executing the code. If they are not installed, they will be installed automatically.
include.packages <- c("dplyr", "ggplot2", "stringr", "readxl", "DataCombine", "texreg",
                      "stargazer", "MASS")
needed.packages <- include.packages[!(include.packages %in% installed.packages()[, "Package"])]
if(length(needed.packages)) install.packages(needed.packages, 
                                             repos = "https://cran.uni-muenster.de/")

lapply(include.packages, library, character.only = TRUE)  # loading all packages at once


# Loading data ------------------------------------------------------------

# Sparkassen Board Membership data
SparkassenBoard <- read_excel("../data/BankBoards_Bavaria_RImport.xlsx",
                              sheet = "BoardComp")
## define variable class:
SparkassenBoard$Incumbent <- as.character(SparkassenBoard$Incumbent)

# Municipal Election data
MayorElection <- read_excel("../data/MayorElectionData.xlsx",
                            sheet = "Bgm_OB")


# Cleaning Sparkassen data set --------------------------------------------

# renaming variables
oldnames <- c("^name$", "^political...employee$", "^governing...opposition$", "retired.1.yes.",
              "Name_CountyMunicipality", "Type_CountyMunicipality")
newnames <- c("NameCandidate1", "political_employee", "governing_opposition", "retired",
              "NameMunicipality", "type_county_municipality")
for (i in seq_along(oldnames)) {
  names(SparkassenBoard) <- gsub(oldnames[i], newnames[i], names(SparkassenBoard))
}

# trimming whitespace:
SparkassenBoard$NameMunicipality <- str_trim(SparkassenBoard$NameMunicipality)
SparkassenBoard$NameCandidate1 <- str_trim(SparkassenBoard$NameCandidate1)

# creating variable for top position
SparkassenBoard$TopPosition <- 0
SparkassenBoard$TopPosition[SparkassenBoard$chair != "no"] <- 1
SparkassenBoard$TopPosition <- as.character(SparkassenBoard$TopPosition)

# Create sub-dataframes
## :unique banks
SparkassenBoard_UniqueBanks <- unique(SparkassenBoard[ ,c("bank_ID", "bank_name", 
                                                          "federal_state", "city", 
                                                          "board_size")])

## :Top Positions only
SparkassenBoard_TopPositions <- subset(SparkassenBoard, TopPosition == 1)


# Cleaning municipality election data set ---------------------------------

# removing newlines
names(MayorElection) <- str_replace_all(names(MayorElection), "[\r\n]" , "")  
# removing punctuation
names(MayorElection) <- gsub("[[:punct:]]", "", names(MayorElection))
# removing whitespace
names(MayorElection) <- str_trim(names(MayorElection))
# correcting "Geschlecht"
names(MayorElection) <- gsub("^Geschl", "Geschlecht", names(MayorElection))

# Changing label names
newnames <- c("IDMunicipality","NameMunicipality", "ElectionDate", "RunOffNecessary", "ElectionType", "ProfPolitician", "NumberEligVoter", "NumberVoters", "InvalBallots", "ValBallots", "NameCandidate1")

names(MayorElection)[1:11] <- newnames

names(MayorElection) <- gsub("gültigeStimmen 1", "VotesCandidate1", names(MayorElection))
names(MayorElection) <- gsub("Stimmen für nicht genannte Bewerber zusammen", "VotesRest", names(MayorElection))

# removing empty column 85
MayorElection[, 85] <- NULL

# Cleaning of gender strings
MayorElection$Geschlecht1 <- str_trim(MayorElection$Geschlecht1)

# cleaning of party names
MayorElection$MayorParty <- MayorElection$"Wahlvorschlag 1"
MayorElection$MayorParty <- str_trim(MayorElection$MayorParty)
# reduce party to main party only
MayorElection$MayorParty <- sub("/.*","", MayorElection$MayorParty)



# Cleaning the party names for bar plotting
MayorElection$CleanParty <- NA
for(i in 1:nrow(MayorElection)) {
  if(is.na(MayorElection$MayorParty[i])) {
    next
  }
  if(grepl("CSU", MayorElection$MayorParty[i])) {
    MayorElection$CleanParty[i] <- "CSU"
  }else if(grepl("SPD", MayorElection$MayorParty[i])) {
    MayorElection$CleanParty[i] <- "SPD"
  } else {
    MayorElection$CleanParty[i] <- "Other"
  }
}


# Creating additional variables -------------------------------------------

# Extracting Phd titles
MayorElection$Phd <- NA
MayorElection$Phd <- ifelse(grepl("Dr.", MayorElection$NameCandidate1), 1, 0)
MayorElection$NameCandidate1 <- gsub(" Dr.", "", MayorElection$NameCandidate1)
MayorElection$NameCandidate1 <- gsub(" jur.", "", MayorElection$NameCandidate1)


# creating the total number of terms for each mayor
tally <- dplyr::tally(group_by(MayorElection, NameCandidate1))
names(tally)[2] <- "TotalTerms"
#merging that back into the data frame
MayorElection <- merge(MayorElection, tally)

# Extracting years from the election date character string
MayorElection$Year <- strtrim(MayorElection$ElectionDate, 4L)
MayorElection$Year <- as.integer(MayorElection$Year)  ## coercing year variable to an integer

# Dummy variable for contested elections
MayorElection$Contested <- ifelse(nchar(MayorElection$`Name 2Nachname Titel Vorname`)>2, 1, 0)

# Calculating votes shares from the data
MayorElection$VoteShareWinner <-  (MayorElection$VotesCandidate1 / MayorElection$ValBallots) * 100


# Merging data sets -------------------------------------------------------
# Ordering the data by municipality
MayorElection$IDMunicipality <- as.integer(MayorElection$IDMunicipality)
MayorElection <- arrange(MayorElection, IDMunicipality, Year)


##: variable for board membership
MayorElection$SparkassenMember <- is.element(MayorElection$NameCandidate1, unique(SparkassenBoard$NameCandidate1))

##: variable for board membership of incumbent
#library(DataCombine) # I moved the loading of the library up in the initiatl code which checks whether all necessary packages are installed, installs missings and then load them (named: "fetching_library") (MB)
MayorElection <- slide(MayorElection, Var = "SparkassenMember",
                       NewVar = "IncumbentSparkassenMember")

# Subsetting the data set (part 1) -------------------------------------------------

# subsetting by election type; excluding run-off elections
MayorElection <- subset(MayorElection, ElectionType != 3)

# Subsetting to contested elections only
MayorElection <- subset(MayorElection, Contested == 1)


# creating dummy for retired mayors ---------------------------------------

# creating a vector with the column numbers of all other candidate names
otherCandidateColumns <- grep("Name ", names(MayorElection))

# cleaning the remaining candidate columns from " Dr." and " jur." strings
for(i in otherCandidateColumns) {
  MayorElection[, i]<- gsub(" Dr.", "", MayorElection[, i])
  MayorElection[, i]<- gsub(" jur.", "", MayorElection[, i])
}

# Excluding mayors who probably retired (JM)
# This loop is quick and produces 7472 observations where the candidate did stand
# for election again
MayorElection <- slide(MayorElection, Var = "NameCandidate1",
                       GroupVar = "IDMunicipality",
                       NewVar = "L.NameCandidate1")
# MayorElection$Retired <- 1
# MayorElection$Retired[MayorElection$NameCandidate1 ==
#                         MayorElection$L.NameCandidate1] <- 0
# MayorElection$Retired[MayorElection$`Name 2Nachname Titel Vorname` ==
#                         MayorElection$L.NameCandidate1] <- 0
# MayorElection$Retired[MayorElection$`Name 3Nachname Titel Vorname` ==
#                         MayorElection$L.NameCandidate1] <- 0
# MayorElection$Retired[MayorElection$`Name 4Nachname Titel Vorname` ==
#                         MayorElection$L.NameCandidate1] <- 0

# excluding mayors who did not run again (likely because of retirement) (MBK)
# This loop is rather hacky and therefore quite slow and produces 7491 elections
# where a mayor stood for re-election

# creating empty variable to be filled in next step
MayorElection$StandAgain <- NA
# Loop starts at 2nd observation because otherwise 0 would be returned for i-1,
# which results in NA for the logical condition of the if structure
for(i in 2:nrow(MayorElection)) {
  # Creating a vector with the names of all candidates which ran in it
  allCandidates <- as.character(MayorElection[i, c(1, otherCandidateColumns)])
  # Partially matching the name of the previous mayor among the names of all
  # candidates of the current election, if it there's a match, 1 will be assigned to
  # the StandAgain variable. If there is no match, 0 is assigned.
  if(is.na(MayorElection$L.NameCandidate1[i])) {
    next()
  }
  if(any(pmatch(MayorElection$L.NameCandidate1[i], allCandidates), na.rm = TRUE)) {
    MayorElection$StandAgain[i] <- 1
  } else {
    MayorElection$StandAgain[i] <- 0
  }
}



# creating lagged variables -----------------------------------------------
## Lagged DV
# creating lagged incumbency for the calculation of the re-election binary variable
# MayorElection <- slide(MayorElection, Var = "NameCandidate1", TimeVar = "ElectionDate", NewVar = "L.NameCandidate1")
MayorElection$Reelection <- ifelse(MayorElection$NameCandidate1 == MayorElection$L.NameCandidate1, 1, 0)

# Lagging variables for analysis, lag of SparkassenMembership is already implemented above. Lag variable is called "IncumbentSparkassenMember"
MayorElection <- slide(MayorElection, Var = "Geschlecht1", TimeVar = "ElectionDate", NewVar = "L.Geschlecht1")
MayorElection <- slide(MayorElection, Var = "VoteShareWinner", TimeVar = "ElectionDate", NewVar = "L.VoteShareWinner")

# Subsetting for years and excluding retired mayors -----------------------------------
MayorElection <- subset(MayorElection, Year >= 2006)
# MayorElection <- subset(MayorElection, Retired == 0)
MayorElection <- subset(MayorElection, StandAgain == 1)


