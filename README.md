# Getting-and-Cleaning-Data
Load, transform and summarise velocity and acceleration statistics from Samsung Galaxy S

# Purpose:

The run_analysis.R process reads and summarises the test and training datasets of acceleration and angular velocity of 30 test subjects completing six activities collected from the accelerometers and gyroscopes of Samsung Galaxy S smartphones. Further details on the makeup of the underlying datasets is available at: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# Process:

•	Checks for existence of raw data in working directory and downloads and unzips if missing.
•	Combines raw data files into a single tidy data frame.
•	Removes any files added during the process.
•	Outputs the final data frame as a txt document.
