**Title: Getting and Cleaning Data Course Project Codebook**
Author: Bartlomiej Bilski
date: 25/01/2023

**Project Description:**
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
The goal is to prepare tidy data that can be used for later analysis. 

**Collection Data:**
The data collection was done with an experiment carried out with a group of 30 volunteers (Subject) within an age bracket of 19-48 years. 
Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) 
on the waist. 
Using its embedded accelerometer and gyroscope, data was captured via 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. 

The experiments was video-recorded to label the data manually. The obtained dataset was randomly partitioned into two sets, 
where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and 
then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). 
The sensor acceleration signal, which has gravitational and body motion components, was separated 
using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, 
therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained 
by calculating variables from the time and frequency domain.

Feature Selection 
=================
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. 
These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. 
Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. 

Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) 
using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). 
Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm 
(tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing 
fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 
These signals were used to estimate variables of the feature vector for each pattern:'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

For the purpose of this project, the set of variables that were selected from these signals data are: 
mean(): Mean value
std(): Standard deviation

**Data Dictionary:**

**Subject**   1-30
This represents the Group of 30 volunteers within an age bracket of 19-48 years.

**Activity**
This represents the activities performed by each volunteer.
**Activity code**
        WALKING 
        WALKING_UPSTAIRS 
        WALKING_DOWNSTAIRS 
        SITTING 
        STANDING 
        LAYING

**tBodyAcc-mean-XYZ**
This represents the mean time taken for body acceleration signal for each pattern(XYZ) measured in Hz.

**tBodyAcc-std-XYZ**
This represents the standard deviation of the time taken for body acceleration signal for each pattern(XYZ) measured in Hz.

**tGravityAcc-mean-XYZ**
This represents the mean time taken for gravity acceleration signal for each pattern(XYZ) measured in Hz.

**tGravityAcc-std-XYZ**
This represents the standard deviation of the time taken for gravity acceleration signal for each pattern(XYZ) measured in Hz.

**tBodyAccJerk-mean-XYZ**
This represents the mean time taken for body acceleration jerk signal for each pattern(XYZ) measured in Hz.

**tBodyAccJerk-std-XYZ**
This represents the standard deviation for the time taken for body acceleration jerk signal for each pattern(XYZ) measured in Hz.

**tBodyGyro-mean-XYZ**
This represents the mean time taken for body gyroscope signal for each pattern(XYZ) measured in Hz.

**tBodyGyro-std-XYZ**
This represents the standard deviation for the time taken for body gyroscope signal for each pattern(XYZ) measured in Hz.

**tBodyGyroJerk-mean-XYZ**
This represents the mean time taken for body gyroscope jerk signal for each pattern(XYZ) measured in Hz.

**tBodyGyroJerk-std-XYZ**
This represents the standard deviation for the time taken for body gyroscope jerk signal for each pattern(XYZ) measured in Hz.

**tBodyAccMag-mean**
This represents the mean time taken for body acceleration magnitude signal measured in Hz.

**tBodyAccMag-std**
This represents the standard deviation of the time taken for body acceleration magnitude signal measured in Hz.

**tGravityAccMag-std**
This represents the standard deviation of time taken for gravity acceleration magnitude signal measured in Hz.

**tGravityAccMag-mean**
This represents the mean time taken for gravity acceleration magnitude signal measured in Hz.

**tBodyAccJerkMag-mean**
This represents the mean time taken for body acceleration jerk magnitude signal measured in Hz.

**tBodyAccJerkMag-std**
This represents the standard deviation of the time taken for body acceleration jerk magnitude signal in measured Hz.

**tBodyGyroMag-mean**
This represents the mean time taken for body gyroscope magnitude signal measured in Hz.

**tBodyGyroMag-std**
This represents the standard deviation of the time taken for body gyroscope magnitude signal measured in Hz.

**tBodyGyroJerkMag-mean**
This represents the mean time taken for body gyroscope jerk magnitude signal measured in Hz.

**tBodyGyroJerkMag-std**
This represents the standard deviation of the time taken for body gyroscope jerk magnitude signal measured in Hz.

**fBodyAcc-XYZ-mean**
This represents the mean of the frequency of body acceleration signal for each pattern(XYZ) measured in Hz.

**fBodyAcc-XYZ-std**
This represents the standard deviation of the frequency of body acceleration signal for each pattern(XYZ) measured in Hz.

**fBodyAccJerk-mean-XYZ**
This represents the mean of the frequency of body acceleration jerk signal for each pattern(XYZ) measured in Hz.

**fBodyAccJerk-std-XYZ**
This represents the standard deviation of the frequency of body acceleration jerk signal for each pattern(XYZ) measured in Hz.

**fBodyGyro-mean-XYZ**
This represents the mean of the frequency of body gyroscope signal for each pattern(XYZ) measured measured in Hz.

**fBodyGyro-std-XYZ**
This represents the standard deviation of the frequency of body gyroscope signal for each pattern(XYZ) measured in Hz.

**fBodyAccMag-mean**
This represents the mean of the frequency of body acceleration magnitude signal measured in Hz.

**fBodyAccMag-std**
This represents the standard deviation of the frequency of body acceleration magnitude signal measured in Hz.

**fBodyBodyAccJerkMag-mean**
This represents the mean of the frequency of body acceleration jerk magnitude signal measured in Hz.

**fBodyBodyAccJerkMag-std**
This represents the standard deviation of the frequency of body acceleration jerk magnitude signal measured in Hz.

**fBodyBodyGyroMag-mean**
This represents the mean of the frequency of body gyroscope magnitude signal measured in Hz.

**fBodyBodyGyroMag-std**
This represents the standard deviation of the frequency of body gyroscope magnitude signal measured in Hz.

**fBodyBodyGyroJerkMag-mean**
This represents the mean of the frequency of body gyroscope jerk magnitude signal measured in Hz.

**fBodyBodyGyroJerkMag-std**
This represents the standard deviation of the frequency of body gyroscope jerk magnitude signal measured in Hz.


**The set of variables that were estimated from these signals are: **

mean(): Mean value
std(): Standard deviation
mad(): Median absolute deviation 
max(): Largest value in array
min(): Smallest value in array
sma(): Signal magnitude area
energy(): Energy measure. Sum of the squares divided by the number of values. 
iqr(): Interquartile range 
entropy(): Signal entropy
arCoeff(): Autorregresion coefficients with Burg order equal to 4
correlation(): correlation coefficient between two signals
maxInds(): index of the frequency component with largest magnitude
meanFreq(): Weighted average of the frequency components to obtain a mean frequency
skewness(): skewness of the frequency domain signal 
kurtosis(): kurtosis of the frequency domain signal 
bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean

