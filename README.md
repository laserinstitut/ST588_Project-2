# Melbourne Housing Explorer

An interactive Shiny app for exploring Melbourne housing records.
Users can filter properties, download selected records, and compare
prices and property characteristics using tables and graphs.

## Data

The app uses `Melbourne_housing_FULL.csv` from Anthony Pino’s
[Melbourne Housing Market dataset on Kaggle](https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market).

The file contains 34,857 records and 21 original variables, including
price, property type, rooms, building area, land size, region, and
distance from the city centre. Prices are in Australian dollars.

## Features

- Filter records by property type and region.
- Apply optional ranges for two numeric variables.
- Update the selected data using the Apply Filters button.
- View and download the selected records as a CSV file.
- Create one-way and two-way contingency tables and bar charts.
- Calculate numerical summaries overall or by category.
- Create histograms, box plots, scatter plots, and heatmaps.
- Choose variables for coloring and faceting.

## Running the app

Install the required packages once:

```r
install.packages(c("shiny", "tidyverse", "DT", "scales"))
