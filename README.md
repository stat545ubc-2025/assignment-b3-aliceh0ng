# Individual Assignment 3 (STAT545 B)

This R Shiny called `Strains` app visualizes time-course plate reader data generated on BioTek microplate readers (exported as `.xlsx` files). The app lets you either upload your own Excel file or explore a sample dataset, then interactively map wells on a 96-well plate to strains and experimental conditions and see how OD600 or fluorescence changes over time.

There are four main features of this app:

- FEATURE 1: Plot showing OD600 or fluorescence data over time for the selected strain. The plot updates automatically whenever you change the plate layout, selected strain, or conditions.
- FEATURE 2: Table showing the current assignments of strain and condition(s) for each well. This table updates in real time as you click wells on the 96-well plate layout.
- FEATURE 3: A slider control that lets you choose up to which row (timepoint index) of the dataset to display, so you can focus on a specific portion of the time course.
- FEATURE 4: An upload option for BioTek `.xlsx` exports, plus an optional built-in example dataset so the app can be explored without providing your own file.

You can test out the app for youreslf at [https://aliceh0ng.shinyapps.io/strains/](https://aliceh0ng.shinyapps.io/strains/)

## Repository Contents
- **README.md** – Current document, explaining the ShinyApp and features of the app.
- **Reflection.md** - Statement on Gen AI use.
- **Strains (Folder)** - Contains the code for the ShinyApp, 
  - **app.R** - Script for the ShinyApp
  - **osmeE_antisense_08222023.xlsx** - Sample dataset to use as demo
  - **rsconnect** - folder containing deployment metadata from publishing ShinyApp

## ShinyApp Instructions For Use

Below is a video showing an example of exactly how this ShinyApp can be used.

