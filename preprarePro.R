################################################################################
                        # Exploration
################################################################################


# Download and load data
fileUrl <- "https://d3c33hcgiwev3.cloudfront.net/4tiY2fqCQa-YmNn6gnGvzQ_1e7320c30a6f4b27894a54e2de50a805_brfss2013.RData?Expires=1614902400&Signature=cj3ZrzacK4iKJSar8jn3UrI1noO0MmmYrN4qf68nA9nMwVYyhWUd52vosLmnC4p4ti9tbsJAdShV~QsJHE2cnEgzxspWk2YeYaBYW6SZq-86MpZd4VgyY72bZ~OnHB0UFxrHqmZQpD8JiNM9w-m61nj9tFq3hDGgOrt78xczoIY_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"
download.file( fileUrl, destfile = "brfss2013.gz", method = "curl")
library(R.utils)
gunzip("brfss2013.gz")
load("brfss2013.RData")


# Understand data