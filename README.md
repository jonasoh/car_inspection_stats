# Car inspection stats

Shiny app for exploring Finnish [car inspection data](https://trafi2.stat.fi/PXWeb/pxweb/en/TraFi/TraFi__Katsastuksen_vikatilastot/020_kats_tau_102.px/). 

The app is live on [shinyapps.io](https://jonasoh.shinyapps.io/car_inspection_stats/).

## Updating the data

1. Download one CSV per inspection year from the [Traficom Statistics Database](https://trafi2.stat.fi/PXWeb/pxweb/en/TraFi/TraFi__Katsastuksen_vikatilastot/020_kats_tau_102.px/) into `raw_data/` (already `.gitignore`d).
2. Run `Rscript preprocess.R`. This reads all of `raw_data/*.csv`, applies `model_name_map.csv` to harmonize model names that Traficom renamed/regrouped starting in 2023, and writes the compact `app_data.rds` that `app.R` loads.
3. If the script reports pre-2023 model names with no 2023+ counterpart, check whether they're genuinely discontinued or need a new entry in `model_name_map.csv`.
4. Commit the updated `app_data.rds`.
