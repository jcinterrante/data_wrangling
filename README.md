# Data Skills 2 - R
## Winter Quarter 2021

## Homework 2
## Due: Sunday February 7th before midnight on GitHub Classroom

__Question 1 (10%):__ Complete the final project quiz on Canvas.  Full points for answering question 1 with anything of substance.  Note that your idea can evolve, or even change entirely - I will not be checking your actual final project against this answer.

__Question 2 (40%):__ For this question, you will be using the City of Chicago [Data Portal](https://data.cityofchicago.org) to create a choropleth using the ggplot and sf libraries.  You should:
  * Download two [datasets](https://data.cityofchicago.org/browse?limitTo=datasets) that interest you
  * Download one [shapefile](https://data.cityofchicago.org/browse?tags=shapefiles) if your datasets are the same geographic division, or two otherwise

Do not use the Major Streets shapefile, since we will use it in question 3.  Use this to create two choropleths, one for each data file.  At a minimum you should use the "fill" aes in ggplot to color based on your selected data, but you are free to use others as well.  Include 2-4 sentences in a comment describing what research question your choropleths begin to address or display.

Effort to improve the appearnce of your plots relative to the default will be rewarded, as will proper usage of tidyverse style, and loops/containers/functions should they be appropriate for your code.

Save this code as "question_2.R", and include your downloads from the Data Portal in your repo.  Save your choropleths as .png files and commit them as well.

__Question 3 (40%):__ Using what you created for question 2, convert it into a Shiny app.  Allow at least two elements to be controlled in the UI (e.g. time, and type of data).  Then add the option to toggle streets on and off in your choropleth, using the [Major Streets shapefile](https://data.cityofchicago.org/Transportation/Major-Streets/ueqs-5wr6).  

Save this code as "app.R", and include the Major Streets shapefile in your repo.

__Question 4 (10%):__ Create a free account on [shinyapps.io](https://www.shinyapps.io/) and upload your app.R file and data files from question 3 to it.  Check that the url it generates is working, and include the url to your Shiny app in a comment at the end of your app.R file.
