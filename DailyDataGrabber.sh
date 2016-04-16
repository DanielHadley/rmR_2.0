#!/bin/sh

#### Created by Daniel Hadley #####
# Automates the dail downloading of new rat data

### Pull the latest version in case changes were made

cd /home/pi/Github/rmR_2.0/
git pull

# run the script that downloads the data 
sudo Rscript /home/pi/Github/rmR_2.0/DailyDataGrabber.R





