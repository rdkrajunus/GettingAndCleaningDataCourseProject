---
title: "Getting and Cleaning Data: Course Project"
author: "Richard Krajunus"
date: "01/25/2015"
output: html_document
---
## File Descriptions
This project contains 3 files:

* codebook.md - The code book describes each of the features that are in 
tidyMeanOfFeatures.txt, a file generated by run_analysis.R
* README.md - This readme file explaining each of the files in this directory
* run_analysis.R - An R script that tidies the Human Activity Recognition Using
Smartphones Dataset Version 1.0

run_analysis.R can generate a fourth file, tidyMeanOfFeatures.txt, which is the tidied version of the Human Activity Recognition Using Smartphones Dataset Version 1.0.  tidyMeanOfFeatures.txt wil be created in the working directory.

## Usage of run_analysis.R

run_analysis.R is an R script which was written to process and tidy the Human
Activity Recognition Using Smartphones Dataset Version 1.0.  This dataset was provided by Coursera instructors during the Getting and Cleaning Data Coursera course beginning January 5, 2015.  A copy of the dataset can be found here:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

To run run_analysis.R:

1. Install the [dplyr](http://cran.r-project.org/web/packages/dplyr/index.html) package.
1. Download the dataset, and extract the zip file to a directory
1. Set the working directory to the directory which contains the `UCI HAR Dataset` folder. During my testing, I placed run_analysis.R in the directory which contained the `UCI HAR Dataset` folder, therefore, run_analysis.R and the `UCI HAR Dataset` folder were siblings.  I set the working directory to the parent folder, like so:
```
Parent directory/ (also the working directory)
        run_analysis.R
        UCI HAR Dataset/
```
1. Run run_analysis.R

## How run_analysis.R works

run_analysis.R produces a tidy version of the dataset by taking the following steps:

1. Read the columns and convert them to descriptive names by performing the following steps:
    1. For columns starting with `t`, replace that with `time`
    1. For columns starting with `f`, replace that with `frequency`
    1. For columns containing `Acc`, replace that with `Accelerometer`
    1. For columns containing `Gyro`, replace that with `Gyroscope`
    1. For columns containing `Mag`, replace that with `Magnitude`
    1. For columns containing `BodyBody`, replace that with `Body`
    1. For columns containing `-`, replace that with the empty string
    1. For columns containing `mean()`, replace that with `Mean`
    1. For columns containing `std()`, replace that with `StdDev`.
    
    I did not choose the expansive name, "StandardDeviation" since it would be much longer and it would add minimal additional clarity, and I also thought similarly of expanding columns ending in X, Y or Z to something like InXAxis.  camelCase was chosen because if all the column names were lower case, they would be too difficult to read, due to their length.

1. Read the training data into a dataframe.  Due to the size of the files, the script will only read the columns of interest, not the entire dataset.  The columns of interest are the columns where both a mean and standard deviation are available.
1. Add an additional column, `subjectId`, to the training dataframe that identifies which subject carried out the experiment.
1. Add an additional column, `activityId`, to the training dataframe that identifies which activity the subject carried out.
1. Read the test data into a separate dataframe.
1. Add an additional column, `subjectId`, to the test dataframe that identifies which subject carried out the experiment.
1. Add an additional column, `activityId`, to the test dataframe that identifies which activity the subject carried out.
1. Read the activity descriptions into a dataframe, and convert the activity labels into camelCase.
1. Merge the test and training dataframes by appending the test rows to the training rows.
1. Add a column, `activityName`, to the merged dataframe, containing the camelCase activity description corresponding to the `activityId` in the dataset
1. Compute the mean of each column, grouped by `subjectId` and `activityName`
1. Rename the columns so that they're described as a `meanOf<columnName>`
1. Select the `subjectId` and `activityName`, so that they lead each row of the dataset, followed by the data columns.  The `activityId` is not included in the select, as it provides no additional information, since the `activityName` is already selected.

## License
The following license was attached to the dataset, so I thought to include it
here:

Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.