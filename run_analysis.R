#Skilgreini hvar ég er að vinna verkefnið 

setwd("~/Coursera/Getting and cleaning data/Programming assignment")

#Skilgreinum slóðina a fileinn

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"


# Til að skilgreing t.d slóð sem er lengar en working directoryr væri gert "./Data/camers.csv"
# eða fyrir windos þá þarf að hafa öfug slash "C:\\users\Gissi\downloads" -> sem dæmi til að setja working directory.
download.file(fileUrl,destfile= "Pr_Ass.zip")

