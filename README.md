# Market-Basket-Analysis-Instacart
This project implements a dashboard using R Shiny Dashboard Library. The dasboard represents a market basket analysis of Instacart. It allows
the user to specify where the data is found that will be used to produce Descriptive and Prescriptive Data Analytics. The prescriptive data 
analytics implements an Apriori Algorithm with the flexibility of changing the algorithm parameters through the dashboard.

### Getting Started
* To clone (download the repository), open Git Bash (for Windows users), terminal for Linux/Mac users and run the following commands:
```
$ cd DIRECTORY
example:
$ cd Desktop
$ git clone https://github.com/Hera94/Market-Basket-Analysis-Instacart
```

### Prerequisites
* Install R and R Studio.
  a helpful link: https://courses.edx.org/courses/UTAustinX/UT.7.01x/3T2014/56c5437b88fa43cf828bff5371c6a924/
* Download and Install Git Bash

### User Manual
Use the sidebar through the different tabs of the dashboard
* Upload Tables - this tab is used to define the folder where all the data files are found. please choose the folder "instacart-market-basket-analysis".
* Tables - this tab helps to take a glance at the data of each dataset.
* Descriptive Data Analytics- this tab has four subtabs representing descriptive graphs of the dataset.
* Prescriptive Data Analytics- this tab implements a Market Basket Analysis. The Apriori algorithm is made interactive as the user has controls to change the parameters of the algorithm. Also, the user can choose if he wants to do the Apriori per product, per Aisle, or per Department. The user will see the marketing insights, Association rule scatterplots, association rule graph and association rules.

### Tips
* Check [Screenshots.pdf](https://github.com/Hera94/Market-Basket-Analysis-Instacart/blob/master/Screenshots.pdf) for screenshots of the dashboard
* It is recommended to set the default browser to Google Chrome and once you run the app, click on "Open in Browser".
