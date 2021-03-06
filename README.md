STAT 547: Homework 08
================
Stephen Chignell
November 21, 2018

**Welcome to the repository!** Here you'll find all the files and associated material for [Howework 8](http://stat545.com/Classroom/assignments/hw08/hw08.html).

### Overview

For this assignment, I chose to modify Dean Attali's [tutorial](https://deanattali.com/blog/building-shiny-apps-tutorial/) and [code](https://deanattali.com/blog/building-shiny-apps-tutorial/#12-final-shiny-app-code) in order to create my own version of the the British Columbia Liquor store Shiny app. The [original data set](http://www.opendatabc.ca/dataset/bc-liquor-store-product-price-list-current-prices) is available from [OpenDataBC](http://www.opendatabc.ca/).

### The Shiny app

My app is hosted on [shinyapps.io](https://www.shinyapps.io), and can be viewed [**here**](https://smc-test-shiny.shinyapps.io/British_Columbia_Liquor_Prices/).

#### Description

Like Dean's original tool, this app filters data about different types of BC Liquor. However, the modified version has a number of additional features available to the user:

1.  **DT Tables:** This provides a search bar and interactive sorting that makes it easy to quickly isolate a record based on a specific keyword or phrase.

2.  **Images!** I have added the store logo to the page as well as an image of liquor bottles next to the table numbers (an homage to the way that Goooooogle extends their o's in a multi-page search result).

3.  **CSS Styling:** I added a CSS theme called "Journal" from [Bootswatch.com](https://bootswatch.com/) to make the app look more professional.

4.  **Check boxes:** I changed the radio buttons to check boxes to allow users to select multiple drink types.

5.  **Sweetness Slider:** I added a second slider widget to allow the user to filter by sweetness.

6.  **Downloads:** There is a now a button that allows the user to download the data as .csv file, based on the current filters applied. The .csv file is labeled with the current date to promote data provenance.

### The code

If you would like to reference or reproduce the modified code behind this app, you can find it in the repository [here](https://github.com/STAT545-UBC-students/hw08-schignel/blob/master/bcl/app.R).

**Best of luck with your selection. Cheers!**

![](https://media.giphy.com/media/RLfDm4jtYWt68/giphy.gif)
