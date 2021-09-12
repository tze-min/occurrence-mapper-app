# Occurrence Mapper

The Shiny dashboard app accepts your occurrence data and returns its table view, statistical summaries and an interactive map. The pickers in the sidebar lets you filter the occurrences displayed on the map, based on their scientific name, genus, family or order. You can select points on the map and view your selection in a table, as well as download it.

Created for the Ecological Adaptations Lab at Yale-NUS College.

## How to use

If running locally, open the .Rproj file, navigate to app.R and click "Run App".

To upload your occurrence data, your file must be in .csv and requires the following column headers (case sensitive) in no particular sequence. NAs are allowed in the data. Additional columns in your data can be left alone; they'll be displayed as well.

| Column Name | Description |
|---|---|
| id | Unique identifier of record |
| sciname | Scientific name of observation |
| genus | Genus of observation |
| family | Family of observation |
| order | Order of observation |
| date | Date of collection or record |
| recorder | Name(s) of the persons who recorded the observation |
| longitude | Longitude of record |
| latitude | Latitude of record |
