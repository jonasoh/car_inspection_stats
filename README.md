# Car inspection stats

Shiny app for exploring Finnish [car inspection data](https://trafi2.stat.fi/PXWeb/pxweb/en/TraFi/TraFi__Katsastuksen_vikatilastot/020_kats_tau_102.px/).

The app (`app/`) runs entirely in the browser via [webR](https://docs.r-wasm.org/webr/latest/)/[shinylive](https://posit-dev.github.io/r-shinylive/) — no R server needed once it's built. `build.R` exports a static site that can be hosted anywhere.

## Building the static site

1. Install shinylive once: `Rscript -e 'install.packages("shinylive")'`.
2. Run `Rscript build.R`. This exports `app/` into `site/` (`.gitignore`d).
3. Preview locally with `Rscript -e 'httpuv::runStaticServer("site")'` and open the printed URL. Opening `site/index.html` directly via `file://` won't work, since the app's service worker needs to be served over http.
4. Deploy by copying the contents of `site/` to any static web host.

Note that the first visit downloads webR and the app's R packages (tens of MB); the browser caches them after that.

## Updating the data

1. Download one CSV per inspection year from the [Traficom Statistics Database](https://trafi2.stat.fi/PXWeb/pxweb/en/TraFi/TraFi__Katsastuksen_vikatilastot/020_kats_tau_102.px/) into `raw_data/` (already `.gitignore`d).
2. Run `Rscript preprocess.R`. This reads all of `raw_data/*.csv`, applies `model_name_map.csv` to harmonize model names that Traficom renamed/regrouped starting in 2023, and writes the compact `app/app_data.rds` that `app/app.R` loads.
3. If the script reports pre-2023 model names with no 2023+ counterpart, check whether they're genuinely discontinued or need a new entry in `model_name_map.csv`.
4. Commit the updated `app/app_data.rds`, then rebuild the static site (see above).
