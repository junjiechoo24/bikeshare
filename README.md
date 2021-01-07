# Capital Bikeshare 

The data set is the Capital Bikeshare data set, from https://www.capitalbikeshare.com/system-data. This data comes from Washington D.C.’s bikeshare program, which records every individual ride taken. Each ride is tagged with its start and end time, start and end station (there are hundreds of locations where bikes can be rented across the city), as well as whether the rider has bought a one-time rental or is a member of the bikeshare program.

A script named getdata_bikeshare.R is used to download and clean the data so that it’s ready for R and organized in a simple format. 

The following question was studied: Can any routes be detected (i.e., a particular combination of start & end station) where the average time it takes to travel the route, changes over the course of the time period? For example, if a bike lane is added to a major road, this may reduce the travel time for that particular route.

Methodology:

1. Cluster the data by route, and run a linear model on confounders only, i.e. Days since Jan1 2010 is not part of the model.
2. Within each cluster, use the residuals for permutation test, but group the residuals by day and permute the groups instead of the individual residuals, in order to account for the dependency of data belonging to the same day.
