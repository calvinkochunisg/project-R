# Automatic Chart Analysis - ACA

With this Program we wanted to achieve automatic chart analysis generation with live data form a free online web-api (https://stock-shark.com/).

## Installation
Not much installation is needed. The Code is entirely written in R and the program uses some external R-Packages. You will need a newer version of R-Studio and an Internet connection.

1. Download the R-Script File from this github page
2. Open the R-Scrip File with R-Studio
3. Within R-Studios Script Editor Press Ctrl + A and then Ctrl + Enter to run the code
4. If you do not have all needed packages preinstalled you will ned to answer with "y" on the first question. The session might crash because some packages need R-Studio to restart
5. Once all the needed packages are installed you can answer the first question with "n" from now on.

You are ready to go!

## Running the Program
You will be asked to input a Ticker a Start-Date and an End-Date for the plot.
The programm will connect to Stock-Sharks API. Note that on the free plan you only get 50 requests a day.
Make sure to input your values in the needed formats otherwise your inputs will be replaced with the defined standard values.

If you have done everything correctly a PDF will pop up with your generated Chart. The PDF-File will be saved in the same folder as the R-Script File.

### Contributors
This Program was created by:

Calvin Koch
calvin.koch@student.unisg.ch
20-611-539

and

Roman Gfeller
roman.gfeller@student.unisg.ch
00-000-000
