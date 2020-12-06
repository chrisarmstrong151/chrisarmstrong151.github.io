setwd("C:/Users/Helios/Desktop/Visual Analytics/Project/visualization/D3Visualizations-master/LA_SF")
rm(list=ls())


# Get the data.
raw_data <- read.csv('data_final.csv')

# Drop the extraneous columns.

# The predictions should still be valid since they were
# made on this set of columns anyways.

dropping <- c('X', 'censustractandblock')

raw_data <- raw_data[, !(names(raw_data) %in% dropping)]

# We are only keeping results for Los Angeles county.
# This means that the tract is '6037', so subset based
# on those columns.

# Loop over the column to find the indices that match
# for '6037'.

matching_indices <- unlist(lapply(seq_along(raw_data$rawcensustractandblock), function(x) {

	testing <- strsplit(as.character(raw_data$rawcensustractandblock[x]), '')
	
	if(identical(testing[[1]][1:4], c('6', '0', '3', '7'))) {
		x
	}

}))

raw_data <- raw_data[matching_indices, ]

# To work more easily with the census data, we need to show
# the precision for the census column.

raw_data[, c('rawcensustractandblock')] <- as.character(raw_data[, c('rawcensustractandblock')])

# The precision for the raw data has to match that of the LA set, 
# so we need to loop and add 0s to the beginning and end of
# the string as necessary.

# First, add the zeros to the beginning of the strings.
raw_data[, c('rawcensustractandblock')] <- unlist(lapply(raw_data$rawcensustractandblock, function(x) {

	original <- strsplit(x, '')[[1]]
	
	# Add the beginning 0.
	original <- c('0', original)
	
	# Now add the ending 0.  Basically, if the 12 position in the string
	# is NA AND there is no decimal, then add the 0.
	if(is.na(original[12]) && (original[10] %in% c('.'))) {
	
		original <- c(original, '0')
	
	}
	
	# Collapse the string back together.	
	original <- paste(original, collapse='')
	
	# Now just remove the decimals.
	original <- gsub('\\.', '', original)
	
	original

}))

# Remove records with no logerror or ML prediction.
# ...

# Change rawcensustractandblock to match the id column name in
# the LA file.  Really it's easy to just create a new column
# and drop the old one.

raw_data$id2 <- raw_data$rawcensustractandblock
raw_data <- raw_data[, !(names(raw_data) %in% c('rawcensustractandblock'))]

# Write the file.
# write.csv(raw_data, 'cleaned.csv', quote = FALSE, row.names = FALSE)



# ----- EXTRAS ----- #


# Help determine the ranges or discretization of visualization inputs.
inputty <- lapply(names(raw_data), function(x) paste(x, length(unique(raw_data[,c(x)]))))

# Basic Linear Regression.

# Create 10 models and keep track of the MSE and absolute error for each one.

MSE_vec <- c()
abs_vec <- c()

for(i in 1:10){

	# Randomly sample a thousand data points.
	dataset_length <- length(raw_data[, 1])
	random_records <- sample(dataset_length, 10000, replace = FALSE)
	random_train <- raw_data[c(random_records), ]
	
	# Now regress.
	#fit.simple <- lm(logerror ~ factor(bathroomcnt) + factor(bedroomcnt) + calculatedfinishedsquarefeet + factor(propertylandusetypeid) + yearbuilt + structuretaxvaluedollarcnt + factor(assessmentyear), data = random_train)
	
	fit.simple <- lm(logerror ~ factor(bathroomcnt) + factor(bedroomcnt) + calculatedfinishedsquarefeet + yearbuilt + structuretaxvaluedollarcnt, data = random_train)
	
	# See how it does on a test set.
	
	# Create the set without the records in the training set.
	training_records <- seq_along(dataset_length) - random_records
	sans_training <- raw_data[training_records, ]
	
	dataset_length <- length(sans_training[, 1])
	random_records <- sample(dataset_length, 35, replace = FALSE)
	random_test <- sans_training[c(random_records), ]
	
	predicted <- predict(fit.simple, random_test)
	
	# See how the MSE is.
	MSE <- sum(((random_test$logerror - predicted)^2)/dataset_length)	
	absolute_error <- sum(abs(random_test$logerror - predicted))
	
	MSE_vec <- c(MSE_vec, MSE)
	abs_vec <- c(abs_vec, absolute_error)

}

MSE_vec
abs_vec


