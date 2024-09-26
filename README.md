# Specifying within-between models

#### Dr. Calum J. R. Webb, Sheffield Methods Institute, the University of Sheffield

The training is designed to be 'hands-on' to ensure that you are able to apply the statistical techniques and quantitative methods that are taught in a real research context.

If you are new to `R` and `Rstudio`, you will first need to install and configure the statistical programming language and the GUI on your device. 

Completing all of the setting up steps for the training should take no longer than an hour. I have made my best effort to ensure that the instructions are as detailed as possible and that all of the code and steps have been trialed on a fresh installation of `R`, but if you come across any problems, please do let me know.


## Installing R and Rstudio

If you have never used R or Rstudio before, the first thing you will need to do is install both the programming language (`R`) and the GUI (graphical user interface, `Rstudio`). I recommend following [this guide from R for Data Science](https://r4ds.hadley.nz/intro#prerequisites). 

In short: 

1) First, go to [https://cloud.r-project.org](https://cloud.r-project.org), download and install the latest version of `R` for your operating system.

2) Next, go to [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/), download and install the latest version of `Rstudio` for your operating system. 

3) Try opening `Rstudio`, **not R**, and see whether the screen you are greeted with looks like the one shown in [R for Data Science](https://r4ds.hadley.nz/intro#prerequisites). 


## Downloading a copy of the repository

This repository contains all of the data and code we will be using in the exercises throughout the short course. You will need to download a copy of the repository.

1) At the top of this page, you should see a green button that says "Code" with a drop-down arrow. Click on the drop-down arrow, and then click "Download ZIP". You shouldn't need an account.

2) Next, unzip the zip folder and open it in your file viewer. For Mac users, you can just click on the zip file and MacOS will automatically unzip it in your downloads folder. You can then open the unzipped folder. For Windows users, you may need to find the zip folder in your downloads folder, right click it, and then click "Extract all...", or use the unzip option on the ribbon menu. For Ubuntu/Debian, you should be able to right click the file and select "Extract here".

3) Double-click the `within-between-example.Rproj` 'blue box' icon R project file to open the course R project in Rstudio.

4) In the bottom right panel (by default), make sure you are on the "Files" tab, then open the `code.R` file by clicking on its name. This should open in the top left of your Rstudio window. 

## Installing required packages and dependencies

We will make use of several libraries on the short course that should be installed **prior to the start of the training**. 

When you load up the code.R script, Rstudio should prompt you to install any packages that it detects are used but which are not installed on your system. In short, you will need the following packages:

* install.packages("tidyverse")
* install.packages("lme4")
* install.packages("car")
* install.packages("leaflet")
* install.packages("janitor")
  
  
<br><br><br>
