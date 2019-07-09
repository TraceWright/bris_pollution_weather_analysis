install.packages("rpart.plot")
install.packages(c("cluster", "factoextra"))
library(rpart)
library(rpart.plot)
library(factoextra)
library(cluster)

# data loading
sb <- read.csv("./southbrisbane-aq-2018.csv")
aus <- read.csv("./weatherAUS.csv")

# filter aus dataset for Brisbane location
bris <- aus[aus$Location == "Brisbane", ]

# standardise date format across datasets
bris$FormattedDates <- as.Date(bris$Date)
sb$FormattedDates <- as.Date(sb$Date, format = "%d/%m/%Y")

# filter aus dataset for and 2018 dates
bris_dateFiltered <- bris[bris$FormattedDates >= "2018-01-01" & bris$FormattedDates <= "2018-12-31",]

checkForDuplicates <- any(duplicated(bris_dateFiltered$FormattedDates)) # 365 observations in bris_dateFiltered with no duplicate dates

# merge datasets
merged <- merge(x = bris_dateFiltered, y = sb, by = "FormattedDates", all = TRUE)
merged <- subset(merged, select=-c(Location, Date.x, Date.y)) # remove location (all records are Brisbane), remove unformatted dates

histo_function <- function(data, datamin, datamax, breaks, dataname) {
  hist(
    data.matrix(data),
    breaks = seq(datamin,
                 datamax,
                 length.out = breaks),
    col = "pink",
    main = "Distribution of Values",
    xlab = dataname
  )
}

# visualise column values distribution
merged.names <- c(names(merged))
for (i in 1:length(merged.names)) {
 histo_function(
   merged[merged.names[i][1]],
   min(data.matrix(na.omit(merged[merged.names[i][1]]))),
   max(data.matrix(na.omit(merged[merged.names[i][1]]))),
   101,
   merged.names[i][1]
 )
}

# impute air quality NA's with a mean of the daily values for each variables
# Nitrogen.Oxide..ppm., Nitrogen.Dioxide..ppm., Nitrogen.Oxides..ppm., Carbon.Monoxide..ppm. (all have missing values for each day of the year)
dates <- unique(merged$FormattedDates)
for (i in 1:length(dates)) {
  merged[merged$FormattedDates == as.Date(dates[i]) & is.na(merged$Nitrogen.Oxide..ppm.),]$Nitrogen.Oxide..ppm. <- round(mean(na.omit(merged[merged$FormattedDates == as.Date(dates[i]),]$Nitrogen.Oxide..ppm.)), digits=3)
  merged[merged$FormattedDates == as.Date(dates[i]) & is.na(merged$Nitrogen.Dioxide..ppm.),]$Nitrogen.Dioxide..ppm. <- round(mean(na.omit(merged[merged$FormattedDates == as.Date(dates[i]),]$Nitrogen.Dioxide..ppm.)), digits=3)
  merged[merged$FormattedDates == as.Date(dates[i]) & is.na(merged$Nitrogen.Oxides..ppm.),]$Nitrogen.Oxides..ppm. <- round(mean(na.omit(merged[merged$FormattedDates == as.Date(dates[i]),]$Nitrogen.Oxides..ppm.)), digits=1)
  merged[merged$FormattedDates == as.Date(dates[i]) & is.na(merged$Carbon.Monoxide..ppm.),]$Carbon.Monoxide..ppm. <- round(mean(na.omit(merged[merged$FormattedDates == as.Date(dates[i]),]$Carbon.Monoxide..ppm.)), digits=1)
}

# PM2.5..ug.m.3.
dates <- unique(merged[is.na(merged$PM2.5..ug.m.3.),]$FormattedDates)
for (i in 1:length(dates)) {
  merged[merged$FormattedDates == as.Date(dates[i]) & is.na(merged$PM2.5..ug.m.3.),]$PM2.5..ug.m.3. <- round(mean(na.omit(merged[merged$FormattedDates == as.Date(dates[i]),]$PM2.5..ug.m.3.)), digits=1)
}

# PM10..ug.m.3.
dates <- unique(merged[is.na(merged$PM10..ug.m.3.),]$FormattedDates)
for (i in 1:length(dates)) {
  merged[merged$FormattedDates == as.Date(dates[i]) & is.na(merged$PM10..ug.m.3.),]$PM10..ug.m.3. <- round(mean(na.omit(merged[merged$FormattedDates == as.Date(dates[i]),]$PM10..ug.m.3.)), digits=1)
}

# impute MinTemp
mintemp <- na.omit(subset(merged, select = c(FormattedDates,MinTemp,Time,Air.Temperature..degC.)))
agg_mintemp <- subset(aggregate(.~FormattedDates, mintemp, min), select=c(FormattedDates, MinTemp, Air.Temperature..degC.))
agg_mintemp$difference <- agg_mintemp$Air.Temperature..degC. - agg_mintemp$MinTemp # there seems to be a trend after 08/18 whereby the difference is > 5 degrees
summary_difference_before_aug <- summary(agg_mintemp[agg_mintemp$FormattedDates <= as.Date("2018-08-01"),]$difference)
summary_difference_after_aug <- summary(agg_mintemp[agg_mintemp$FormattedDates >= as.Date("2018-08-01"),]$difference)

dates <- unique(merged[is.na(merged$MinTemp),]$FormattedDates)
for (i in 1:length(dates)) {
  ifelse(merged[merged$FormattedDates==dates[i],]$FormattedDates <= as.Date("2018-08-01"),
         merged[merged$FormattedDates == dates[i],]$MinTemp <- min(merged[merged$FormattedDates == dates[i],]$Air.Temperature..degC.) - summary_difference_before_aug["Mean"],
         merged[merged$FormattedDates == dates[i],]$MinTemp <- min(merged[merged$FormattedDates == dates[i],]$Air.Temperature..degC.) - summary_difference_after_aug["Mean"]
  )
}

# check updated
subset(merged, merged$FormattedDates %in% dates, select = c(FormattedDates,MinTemp,Air.Temperature..degC.))

# impute MaxTemp
maxtemp <- na.omit(subset(merged, select = c(FormattedDates,MaxTemp,Time,Air.Temperature..degC.)))
agg_maxtemp <- subset(aggregate(.~FormattedDates, maxtemp, max), select=c(FormattedDates, MaxTemp, Air.Temperature..degC.))
agg_maxtemp$difference <- agg_maxtemp$Air.Temperature..degC. - agg_maxtemp$MaxTemp # there seems to be a trend after 08/18 whereby the difference is > 5 degrees
summary_difference_before_aug <- summary(agg_maxtemp[agg_maxtemp$FormattedDates <= as.Date("2018-08-01"),]$difference)
summary_difference_after_aug <- summary(agg_maxtemp[agg_maxtemp$FormattedDates >= as.Date("2018-08-01"),]$difference)

dates <- unique(merged[is.na(merged$MaxTemp),]$FormattedDates)
for (i in 1:length(dates)) {
  ifelse(merged[merged$FormattedDates==dates[i],]$FormattedDates <= as.Date("2018-08-01"),
         merged[merged$FormattedDates == dates[i],]$MaxTemp <- min(merged[merged$FormattedDates == dates[i],]$Air.Temperature..degC.) - summary_difference_before_aug["Mean"],
         merged[merged$FormattedDates == dates[i],]$MaxTemp <- min(merged[merged$FormattedDates == dates[i],]$Air.Temperature..degC.) - summary_difference_after_aug["Mean"]
  )
}

# check updated
subset(merged, merged$FormattedDates %in% dates, select = c(FormattedDates,MaxTemp,Air.Temperature..degC.))

# impute Rainfall
dates <- unique(merged[is.na(merged$Rainfall),]$FormattedDates)
merged[is.na(merged$Rainfall),]$Rainfall <- 0 # most common value

# explore Sunshine
sunshine <- subset(merged, select=c(FormattedDates,Sunshine,Rainfall,Cloud9am,Cloud3pm))
sunshine_nas <- subset( merged, is.na(merged$Sunshine), select=c(FormattedDates,Sunshine))

correlation_result <- cor(x = sunshine$Sunshine, y = sunshine$Rainfall, method = c("pearson")) # -0.33
correlation_result <- cor(x = sunshine$Sunshine, y = sunshine$Cloud9am, method = c("pearson")) # -0.65
correlation_result <- cor(x = sunshine$Sunshine, y = sunshine$Cloud3pm, method = c("pearson")) # -0.75
# inverse relationship between sunshine and cloud, as intuitively expected.
# therefore, follow trend of day after to impute missing value for , which has similar 

# impute Sunshine
merged[merged$FormattedDates == as.Date("2018-01-06"),]$Sunshine <- merged[merged$FormattedDates == as.Date("2018-01-07"),]$Sunshine

# impute Evaporation
# only 1 missing value, not a predictor for this study so just impute with average of day before and day after
merged[merged$FormattedDates == as.Date("2018-07-12"),]$Evaporation <- 3.0

# impute Wind Speed
merged[merged$FormattedDates == as.Date("2018-09-04") & is.na(merged$Wind.Speed..m.s.),]$Wind.Speed..m.s. <- round(mean(na.omit(merged[merged$FormattedDates == as.Date("2018-09-04"),]$Wind.Speed..m.s.)), digits = 1)
merged[merged$FormattedDates == as.Date("2018-09-05") & is.na(merged$Wind.Speed..m.s.),]$Wind.Speed..m.s. <- round(mean(na.omit(merged[merged$FormattedDates == as.Date("2018-09-05"),]$Wind.Speed..m.s.)), digits = 1)

# impute Relative.Humidity....
merged[merged$FormattedDates == as.Date("2018-09-04") & is.na(merged$Relative.Humidity....),]$Relative.Humidity.... <- round(mean(na.omit(merged[merged$FormattedDates == as.Date("2018-09-04"),]$Relative.Humidity)), digits = 1)
merged[merged$FormattedDates == as.Date("2018-09-05") & is.na(merged$Relative.Humidity....),]$Relative.Humidity....  <- round(mean(na.omit(merged[merged$FormattedDates == as.Date("2018-09-05"),]$Relative.Humidity)), digits = 1)

# impute Wind.Direction..degTN.
merged[merged$FormattedDates == as.Date("2018-09-04") & is.na(merged$Wind.Direction..degTN.),]$Wind.Direction..degTN. <- round(mean(na.omit(merged[merged$FormattedDates == as.Date("2018-09-04"),]$Wind.Direction..degTN.)), digits = 1)
merged[merged$FormattedDates == as.Date("2018-09-05") & is.na(merged$Wind.Direction..degTN.),]$Wind.Direction..degTN.  <- round(mean(na.omit(merged[merged$FormattedDates == as.Date("2018-09-05"),]$Wind.Direction..degTN.)), digits = 1)

# reduce dimensions Wind Direction
merged$WindDirDaily <- NA
dates <- unique(merged$FormattedDates)
for (i in 1:length(dates)) {
  dir_cos <- cos(merged[merged$FormattedDates == dates[i],]$Wind.Direction..degTN.*(pi/180)) * merged[merged$FormattedDates == dates[i],]$Wind.Speed..m.s.
  dir_sin <- sin(merged[merged$FormattedDates == dates[i],]$Wind.Direction..degTN.*(pi/180)) * merged[merged$FormattedDates == dates[i],]$Wind.Speed..m.s.
  dir_cos_weight <- sum(dir_cos, na.rm=TRUE) / length(dir_cos)
  dir_sin_weight <- sum(dir_sin, na.rm=TRUE) / length(dir_sin)
  wind_dir <- atan(dir_sin_weight/dir_cos_weight)
  wind_dir <- ifelse(dir_cos_weight < 0, wind_dir <- (wind_dir + pi)*(180/pi), wind_dir*(180/pi) )
  wind_dir <- ifelse(wind_dir < 0, wind_dir+360, wind_dir)
  merged[merged$FormattedDates == dates[i],]$WindDirDaily <- wind_dir
}

# categorise Wind Direction
wind_labels=c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW","N")
WIND_GROUPS <- cut(c(merged$WindDirDaily), 
                   breaks=c(-1, 11.25, 33.75, 56.25, 78.75, 101.25, 123.75, 146.25, 168.75, 191.25, 213.75, 236.25, 258.75, 281.25, 303.75, 327.25, 348.75, 361), 
                   wind_labels
)

merged$WindDirDaily_numerical <- as.numeric(data.frame(merged$WindDirDaily, WIND_GROUPS)$WIND_GROUPS)

# check updated
check <- subset(merged, select = c(FormattedDates,WindDirDaily,WindDirDaily_numerical,Wind.Direction..degTN.))
check[is.na(check$WindDirDaily_numerical),]

# select features for analysis
selected_columns <- c("FormattedDates","MinTemp","MaxTemp","Rainfall", "Relative.Humidity....","Wind.Speed..m.s.", "WindDirDaily_numerical", "Pressure9am", "Pressure3pm", "Cloud9am", "Cloud3pm", "Sunshine", "Evaporation","Nitrogen.Oxide..ppm.","Nitrogen.Dioxide..ppm.","Nitrogen.Oxides..ppm.","Carbon.Monoxide..ppm.","PM2.5..ug.m.3.","PM10..ug.m.3.")
cleaned <- subset(merged, select = selected_columns)
sapply(cleaned, function(x)any(is.na(x))) # check for nas

# aggregate rows to one observation per day
daily_agg <- subset(aggregate(.~FormattedDates,cleaned,mean), select=selected_columns)
daily_agg$WindDirDaily_category <- levels(WIND_GROUPS)[daily_agg$WindDirDaily_numerical]
write.csv(daily_agg, file="daily_agg.csv")

#------------------------------------------------------------------------------------#

#  CORRELATIONS

#------------------------------------------------------------------------------------#

correlations <- round(cor(x = daily_agg[2:6], y = daily_agg[14:19], method = c("pearson")), digits=2)
write.csv(correlations, file = "correlations.txt") 

#------------------------------------------------------------------------------------#

#  DECISION TREES

#------------------------------------------------------------------------------------#

daily_agg_tree <- daily_agg

# PM10 decision tree - categorical
# categorise levels of PM10..ug.m.3.
GROUPS_PM10 <- cut(c(daily_agg_tree$PM10..ug.m.3.),
                   breaks=c(-0.1, 16.4, 32.9, 49.9, 74.9), 
                   labels=c("Very Good","Good","Fair","Poor")
)
daily_agg_tree$PM10 <- data.frame(daily_agg_tree$PM10..ug.m.3., GROUPS_PM10)
pm_10 <- daily_agg_tree$PM10$GROUPS_PM10
daily_agg_tree$PM10_Categorised <- as.character(pm_10)


# PM2.5 decision tree - categorical
# categorise levels of PM2.5..ug.m.3.
max_PM2.5 <- max(daily_agg_tree$PM2.5..ug.m.3., 37.4) +1 # capture max values
min_PM2.5 <- min(daily_agg_tree$PM2.5..ug.m.3., 0) -1 # capture min values
GROUPS_PM2.5 <- cut(c(daily_agg_tree$PM2.5..ug.m.3.), 
                    breaks=c(min_PM2.5, 8.2, 16.4, 25, 37.4, max_PM2.5), 
                    labels=c("Very Good","Good","Fair","Poor", "Very Poor")
)
daily_agg_tree$PM2.5 <- data.frame(daily_agg_tree$PM2.5..ug.m.3., GROUPS_PM2.5)
pm_2.5 <- daily_agg_tree$PM2.5$GROUPS_PM2.5
daily_agg_tree$PM2.5_Categorised <- as.character(pm_2.5)

# CM decision tree - categorical
# categorise levels of Carbon.Monoxide..ppm.
max_CM <- max(daily_agg_tree$Carbon.Monoxide..ppm., 13.5) +1 # capture max values
min_CM <- min(daily_agg_tree$Carbon.Monoxide..ppm., 0) -1 # capture min values

GROUPS_CM <- cut(c(daily_agg_tree$Carbon.Monoxide..ppm.), 
                 breaks=c(min_CM, 2.9, 5.8, 8.9, 13.4, max_CM), 
                 labels=c("Very Good","Good","Fair","Poor", "Very Poor")
)
daily_agg_tree$CM <- data.frame(daily_agg_tree$Carbon.Monoxide..ppm., GROUPS_CM)
cm <- daily_agg_tree$CM$GROUPS_CM
daily_agg_tree$CM_Categorised <- as.character(cm)

# ND decision tree - categorical
# categorise levels of Nitrogen.Dioxide..ppm.
max_ND <- max(daily_agg_tree$Nitrogen.Dioxide..ppm., 0.18) +1 # capture max values
min_ND <- min(daily_agg_tree$Nitrogen.Dioxide..ppm., 0) -1 # capture min values

GROUPS_ND <- cut(c(daily_agg_tree$Nitrogen.Dioxide..ppm.), 
                 breaks=c(min_ND, 0.039, 0.078, 0.119, 0.179, max_ND), 
                 labels=c("Very Good","Good","Fair","Poor", "Very Poor")
)
daily_agg_tree$ND <- data.frame(daily_agg_tree$Nitrogen.Dioxide..ppm., GROUPS_ND)
nd <- daily_agg_tree$ND$GROUPS_ND
daily_agg_tree$ND_Categorised <- as.character(nd)


# Split the data set into training and test data
set.seed(4321)
ind <- sample(2, nrow(daily_agg_tree), replace=TRUE, prob=c(0.7, 0.3))
train_data <- daily_agg_tree[sample(nrow(daily_agg_tree[ind==1,])),]
test_data <- daily_agg_tree[ind==2,]

# PM10 decision tree - categorical
# categorise levels of PM10..ug.m.3.
rpart.control(minsplit = 20)
tree_2 = rpart(PM10_Categorised ~ MinTemp + MaxTemp + Rainfall + WindDirDaily_category + Wind.Speed..m.s. + Relative.Humidity...., data=train_data)
prp(tree_2)
p2 <- predict(tree_2, test_data, type="class")
pm10_dt <- table(test_data$PM10_Categorised, predicted=p2)
write.csv(pm10_dt, file = "pm10_dt.txt")


# PM2.5 decision tree - categorical
# categorise levels of PM2.5..ug.m.3.
tree_3 = rpart(PM2.5_Categorised ~ MinTemp + MaxTemp + Rainfall + WindDirDaily_category + Wind.Speed..m.s. + Relative.Humidity...., data=train_data)
prp(tree_3)
p3 <- predict(tree_3, test_data, type="class")
pm2.5_dt <- table(test_data$PM2.5_Categorised, predicted=p3)
write.csv(pm2.5_dt, file = "pm2-5_dt.txt")


# CM decision tree - categorical
# categorise levels of Carbon.Monoxide..ppm.
# tree_4 = rpart(CM_Categorised ~ MinTemp + MaxTemp + Rainfall + WindDirDaily + Wind.Speed..m.s. + Relative.Humidity...., data=train_data)
# prp(tree_4)
# p4 <- predict(tree_4, test_data, type="class")
# table(test_data$CM_Categorised, predicted=p4)

# ND decision tree - categorical
# categorise levels of Nitrogen.Dioxide..ppm.
# tree_5 = rpart(ND_Categorised ~ MinTemp + MaxTemp + Rainfall + WindDirDaily + Wind.Speed..m.s. + Relative.Humidity...., data=train_data)
# prp(tree_5)
# p5 <- predict(tree_5, test_data, type="class")
# table(test_data$CM_Categorised, predicted=p5)

#------------------------------------------------------------------------------------

# PM10 decision tree - categorical, hourly
# pm10_hourly <- as.data.frame(cleaned[!is.na(cleaned$PM10..ug.m.3.),])

# categorise levels of PM10..ug.m.3.
GROUPS_PM10 <- cut(c(cleaned$PM10..ug.m.3.),
                   breaks=c(-0.1, 16.4, 32.9, 49.9, 74.9), 
                   labels=c("Very Good","Good","Fair","Poor")
)
cleaned$PM10 <- data.frame(cleaned$PM10..ug.m.3., GROUPS_PM10)
pm_10 <- cleaned$PM10$GROUPS_PM10
cleaned$PM10_Categorised <- as.character(pm_10)

# Split the data set into training and test data
set.seed(4321)
ind <- sample(2, nrow(cleaned), replace=TRUE, prob=c(0.7, 0.3))
train_hourly_data <- cleaned[ind==1,]
test_hourly_data <- cleaned[ind==2,]

tree_6 = rpart(PM10$GROUPS_PM10 ~ MinTemp + MaxTemp + Rainfall + Wind.Speed..m.s. + Relative.Humidity...., data=train_hourly_data)
prp(tree_6)
p6 <- predict(tree_6, test_hourly_data, type="class")
p6_table <- table(test_hourly_data$PM10_Categorised, predicted=p6)
write.csv(p6_table, file = "pm10_hour_dt.txt")

#------------------------------------------------------------------------------------#

#  CLUSTERING

#------------------------------------------------------------------------------------#

# remove FormattedDates as a row and add it in as a row.name
daily_agg_cluster = data.frame(x=subset(daily_agg, select=-c(FormattedDates,WindDirDaily_category)), row.names=daily_agg$FormattedDates)

# Index with the function to remove values > +/- 2 sd from mean
# k-means clusters are sensitive to outliers
daily_agg_cluster <- daily_agg_cluster[!apply(sapply(daily_agg_cluster, function(x) abs(scale(x)) >= 2), 1, any), ]

# Apply z-score scale
# This scales the values around the mean value using a Gaussian distribution
daily_agg_cluster <- scale(daily_agg_cluster)

# View the firt 3 rows
head(daily_agg_cluster, n = 3)

# Find optimal cluster number - this can takes a long time....
# take the recomended number from the graph and insert it in below
fviz_nbclust(daily_agg_cluster, kmeans, method = "gap_stat")
optimal_k_value <- 2 # Number of clusters we want

# Apply K-means clustering
set.seed(123)
k_my_data <- kmeans(daily_agg_cluster, optimal_k_value, nstart = 25)
fviz_cluster(k_my_data, data = daily_agg_cluster,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(),
             geom = "point")



