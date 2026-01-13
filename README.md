# COACH-AdvisorTool-ShinyApp
Source code for the COACH - Advisor Tool to support the sustainable management of cockle fisheries in the Ria de Aveiro (http://coach.web.ua.pt/en/tool/)

## Folder structure

- `app.R`: Shiny application
- `www/`: UI assets and translation files
- `data/scenarios_projection/`: Download the raster scenario files (*.tif) from https://doi.org/10.5281/zenodo.18234527 and place them directly into this folder. **Do not use subfolders**!

## R Environment

Tested with:

- R 4.4.1 (2024-06-14)

Required packages (versions used for development):

shiny 1.11.1  
shiny.i18n 0.2.0  
leaflet 2.2.3  
viridis 0.6.5  
RColorBrewer 1.1-3  
leafem 0.2.5  
raster 3.6-32  
sp 2.2-0


