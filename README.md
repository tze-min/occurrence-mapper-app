# Occurrence Mapper

This Shiny dashboard app accepts your occurrence data and returns its table view, statistical summaries and an interactive map. The pickers in the sidebar let you filter the occurrences displayed on the map, based on their scientific name, genus, family or order. You can select points on the map and view your selection in a table, as well as download it.

Created for the Ecological Adaptations Lab at Yale-NUS College in 2021.

## SECTION 1. Project Structure

The app's project structure is as follows:

```
occurrence-mapper
│   app.R
│   occurrence-mapper.Rproj
│   README.md
│   styles.css
│
└───data
        clean-sentosa-observations-155147-formatted.csv
        IDL_database_28July20-formatted.csv
        template-dataset.csv
        raw.zip
```

It comprises three major components. `app.R` contains code for the frontend (what components are available on the app) as well as the backend (data processing, interactivity). `styles.css` is a CSS file for additional control over the frontend's design. 

The subfolder `data` contains four items:
1. template-dataset.csv - a demo dataset you can load into the app
2. clean-sentosa-observations-155147-formatted.csv - a formatted Sentosa insect occurrence dataset from iNaturalist
3. IDL_database_28July20-formatted.csv - a formatted Singapore bee occurrence dataset from expert-collected and citizen science sources
4. raw.zip - the original, pre-formatted datasets that can't be loaded and have been left here for reference

## SECTION 2. How To Use

If running locally, open the .Rproj file in RStudio, navigate to `app.R` and click "Run App" on the top right hand corner.

To upload your occurrence data, your file must be in .csv and requires the following column headers (case sensitive) in no particular sequence. NAs are allowed in the data. Additional columns in your data can be left alone; they will be displayed as well. You can open up `template-dataset.csv` to see an example of a viable dataset.

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

## SECTION 3. Bug Reports, Contact and Future Development

Bug reports are always welcome. If you come across one, feel free to open an [issue](https://github.com/tze-min/occurrence-mapper/issues) via the green 'New Issue' button on the right and include details of the bug with screenshots if relevant. See [example](https://github.com/tze-min/occurrence-mapper/issues/6). Alternatively, you could send the details to me via email or text.

If you would like to work on this app further, you can refer to helpful tutorials and guides I've linked in Section 4 below. If you have any questions, you can also reach out to me! :)

## SECTION 4. References and Useful Links

- The inspiration for this app was Edward Parker's [COVID-19 Tracker](https://shiny.rstudio.com/gallery/covid19-tracker.html), whose code is available as a [GitHub repository](https://github.com/eparker12/nCoV_tracker).
- RStudio's official guide to shinydashboard - especially their [Getting Started](http://rstudio.github.io/shinydashboard/get_started.html) and [Structure](http://rstudio.github.io/shinydashboard/structure.html) pages - helps you understand how `app.R` is structured.
- An [overview](https://shiny.rstudio.com/articles/reactivity-overview.html) of reactivity in Shiny helps you understand how the app's interactivity works, aka. the relationship between the ui and server functions within `app.R`. E.g. When you click on a point on the map, how is the data received by a table in another tab updated?
- If you're interested in hosting the app on [shinyapps.io](https://www.shinyapps.io/), sign up for an account first and the site's dashboard will contain instructions regarding how to connect your local app to the site, enabling access to the app via a URL instead.