library(stringr)

`%!in%` <- Negate(`%in%`)

# Load the data

artworks <- read.csv("PRA 1/Artworks.csv", header = TRUE, sep = ",", encoding = "UTF-8")


# Manage gender
View(table(artworks$Gender))

artists.female <- grep("(Female)", artworks$Gender)
artists.male <- grep("(Male)", artworks$Gender)

mixedArtists <- intersect(artists.female, artists.male)
maleArtists <- setdiff(artists.male, mixedArtists)
femaleArtists <- setdiff(artists.female, mixedArtists)
maleFemaleArtists <- unique(c(artists.female, artists.male))
unknownGender <- rownames(artworks[-maleFemaleArtists,])
unknownGender <- as.numeric(unknownGender)


# Manage groups

maleIndividual <- which(artworks$Gender == "(Male)")
femaleIndividual <- which(artworks$Gender == "(Female)")
unknownIndividual <- which(artworks$Gender == "()")

unknown <- which(artworks$Gender == "")
individualWorks <- c(maleIndividual, femaleIndividual, unknownIndividual)
groupalWorks <- which(c(unknown, individualWorks) %in% 1:138151)
groupalWorks <- artworks[-groupalWorks,]
groupalWorks <- rownames(groupalWorks)
groupalWorks <- as.numeric(groupalWorks)

artworks$Groupal <- NA
artworks$Groupal[individualWorks] <- "Individual"
artworks$Groupal[groupalWorks] <- "Groupal"
artworks$Groupal[unknown] <- "Unknown"


# Change genders

artworks$genderCleaned <- NA
artworks$genderCleaned[mixedArtists] <- "Mixed"
artworks$genderCleaned[femaleArtists] <- "Female"
artworks$genderCleaned[maleArtists] <- "Male"
artworks$genderCleaned[unknownGender] <- "Unknown"




artworks <- artworks[,-8]

write.csv(artworks, "Artworks_clean.csv", row.names = FALSE, col.names = TRUE)
