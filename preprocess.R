# Preprocess raw Traficom periodic-inspection data (raw_data/*.csv, one file
# per inspection year) into a compact RDS file consumed by app/app.R.
#
# Usage: Rscript preprocess.R
#
# Source: https://trafi2.stat.fi/PXWeb/pxweb/en/TraFi/TraFi__Katsastuksen_vikatilastot/
#
# Notes on the raw format:
#  - Long format: one row per (model, registration year, fault object,
#    metric, inspection year), value in the last column ("." = missing).
#  - Encoding is Windows-1252, not UTF-8.
#  - From 2023 on Traficom renamed/regrouped a number of models (e.g. the
#    BMW "1".."7" series became "1-sarja".."7-sarja", Lexus IS200/IS250/...
#    were merged into a single "IS", etc). model_name_map.csv maps the old
#    (pre-2023) names onto the new ones so each model forms one continuous
#    series across all years. Its optional third column, display_name,
#    overrides the (post-2023, sometimes Finnish) "new" name with the name
#    shown in the (English-language) app, e.g. "BMW - 1-sarja" is displayed
#    as "BMW - 1 Series"; left blank, the "new" name is displayed as-is.

library(data.table)

message('Reading raw_data/*.csv ...')
files <- list.files('raw_data', pattern='\\.csv$', full.names=TRUE)
stopifnot(length(files) > 0)

read_one <- function(f) {
    dt <- fread(f, encoding='Latin-1', na.strings='.', colClasses='character')
    setnames(dt, c('brand_and_model_series', 'registration_year', 'main_fault_object',
                   'information', 'year_of_inspection', 'value'))
    # raw files are Windows-1252, not UTF-8/Latin-1
    for (col in c('brand_and_model_series', 'main_fault_object', 'information')) {
        dt[[col]] <- iconv(dt[[col]], from='CP1252', to='UTF-8')
    }
    dt[, registration_year := as.integer(registration_year)]
    dt[, year_of_inspection := as.integer(year_of_inspection)]
    dt[, value := as.numeric(value)]
    dt
}

raw <- rbindlist(lapply(files, read_one))
message(sprintf('  %d rows from %d files', nrow(raw), length(files)))

# drop rows that don't represent a single, real fault object or a real
# model (brand-level totals), and rows with no data at all
raw <- raw[!main_fault_object %chin% 'All main objects and objects'
          ][!grepl('Gas installation', main_fault_object)  # new in 2023, would double-count
          ][!grepl('Mallit yhteens', brand_and_model_series)  # per-brand total row
          ][!is.na(value)]

# wide format: one row per (inspection year, model, reg year, fault object)
wide <- dcast(raw, year_of_inspection + brand_and_model_series + registration_year + main_fault_object ~ information,
              value.var='value')
setnames(wide, make.names(names(wide)))
setnames(wide,
         c('Number.of.periodic.inspections', 'Average.mileage', 'Median.mileage',
           'Demand.for.repairs..number.of.faults.', 'Fails..number.of.faults.', 'Driving.bans..number.of.faults.'),
         c('number_of_inspections', 'average_mileage', 'median_mileage',
           'demand_for_repairs', 'rejections', 'driving_bans'),
         skip_absent=TRUE)
wide[, median_mileage := NULL]  # not used by the app, and can't be recombined across merged models

# apply the pre/post-2023 model name map so renamed/regrouped models form one series
name_map <- fread('model_name_map.csv', encoding='UTF-8', colClasses='character', na.strings='')
setkey(name_map, old)
wide[, brand_and_model_series := {
    mapped <- name_map[.(brand_and_model_series), new, on='old']
    ifelse(is.na(mapped), brand_and_model_series, mapped)
}]

unmapped_pre2023 <- setdiff(
    unique(wide[year_of_inspection <= 2022, brand_and_model_series]),
    unique(wide[year_of_inspection >= 2023, brand_and_model_series])
)
if (length(unmapped_pre2023) > 0) {
    message(sprintf('  %d pre-2023 model names have no 2023+ counterpart (series ends in 2022):',
                     length(unmapped_pre2023)))
    message(paste('   -', sort(unmapped_pre2023), collapse='\n'))
}

# apply display_name overrides (e.g. "new" names Traficom left in Finnish)
display_map <- unique(name_map[!is.na(display_name), .(new, display_name)])
stopifnot('display_name must be in "Brand - Model" form'=all(grepl(' - ', display_map$display_name, fixed=TRUE)))
dupes <- display_map[, .N, by=new][N > 1]
if (nrow(dupes) > 0) {
    stop(sprintf('conflicting display_name values for: %s', paste(dupes$new, collapse=', ')))
}
setkey(display_map, new)
wide[, brand_and_model_series := {
    displayed <- display_map[.(brand_and_model_series), display_name, on='new']
    ifelse(is.na(displayed), brand_and_model_series, displayed)
}]

# re-aggregate after the name mapping, since some old names merge into the same new name
wide <- wide[, .(number_of_inspections=sum(number_of_inspections),
                  average_mileage=as.integer(round(weighted.mean(average_mileage, number_of_inspections))),
                  demand_for_repairs=sum(demand_for_repairs),
                  rejections=sum(rejections),
                  driving_bans=sum(driving_bans)),
             by=.(year_of_inspection, brand_and_model_series, registration_year, main_fault_object)]

wide <- wide[number_of_inspections > 0]

# derive brand/make from "Brand - Model", and vehicle age; app.R displays
# "Brand and model series" with the ' - ' separator replaced by a space
wide[, c('brand', 'make') := tstrsplit(brand_and_model_series, ' - ', fixed=TRUE)]
wide[, brand_and_model_series := paste(brand, make)]
wide[, vehicle_age := year_of_inspection - registration_year]

fault_categories <- sort(unique(wide$main_fault_object))
model_related <- c('Axles, wheels and suspension (all objects)',
                    'Chassis and body (all objects)',
                    'Brake systems (all objects)',
                    'Steering equipment (all objects)',
                    'Environmental hazards (all objects)')
years <- sort(unique(wide$registration_year))
ages <- sort(unique(wide$vehicle_age))
cars <- sort(unique(wide$brand_and_model_series))

message('Computing summary tables ...')

stats_model_year <- wide[main_fault_object %in% model_related,
                         .(fault_pct=sum(c(demand_for_repairs, rejections, driving_bans))/number_of_inspections[1],
                           average_mileage=average_mileage[1], brand=brand[1],
                           number_of_inspections=number_of_inspections[1]),
                         by=.(year_of_inspection, brand_and_model_series, registration_year)
                         ][, .(fault_pct=round(weighted.mean(fault_pct, number_of_inspections), 3),
                               average_mileage=as.integer(weighted.mean(average_mileage, number_of_inspections)),
                               number_of_inspections=sum(number_of_inspections), brand=brand[1]),
                           by=.(brand_and_model_series, registration_year)]

stats_by_fault <- wide[, .(fault_pct=sum(c(demand_for_repairs, rejections, driving_bans))/number_of_inspections[1],
                           number_of_inspections=number_of_inspections[1],
                           brand=brand[1],
                           average_mileage=average_mileage),
                         by=.(year_of_inspection, brand_and_model_series, registration_year, main_fault_object)]

avg_stats_by_fault <- stats_by_fault[, .(fault_pct=weighted.mean(fault_pct, number_of_inspections),
                                         number_of_inspections=sum(number_of_inspections)),
                                     by=.(year_of_inspection, registration_year, main_fault_object)]

# pre-collapse over inspection years into the per-(model, reg year, fault) and
# per-(reg year, fault) weighted means the app's model-overview table needs,
# so the app only has to filter these at runtime
model_stats_by_fault <- stats_by_fault[, .(model_value=weighted.mean(fault_pct * 100, number_of_inspections)),
                                       by=.(brand_and_model_series, registration_year, main_fault_object)]
avg_model_stats_by_fault <- avg_stats_by_fault[, .(avg_value=weighted.mean(fault_pct * 100, number_of_inspections)),
                                               by=.(registration_year, main_fault_object)]

stats_age <- wide[main_fault_object %in% model_related,
                  .(fault_pct=sum(c(demand_for_repairs, rejections, driving_bans))/number_of_inspections[1],
                    average_mileage=average_mileage[1],
                    number_of_inspections=number_of_inspections[1]),
                  by=.(year_of_inspection, brand_and_model_series, vehicle_age)
                   ][, .(fault_pct=round(weighted.mean(fault_pct, number_of_inspections), 3),
                         average_mileage=as.integer(weighted.mean(average_mileage, number_of_inspections)),
                         number_of_inspections=sum(number_of_inspections)),
                     by=.(brand_and_model_series, vehicle_age)]

brand_stats_by_age <- stats_model_year[, .(fault_pct=weighted.mean(fault_pct, number_of_inspections),
                                           average_mileage=weighted.mean(as.numeric(average_mileage), number_of_inspections)),
                                       by=.(brand, registration_year)
                                       ][, .(rank=frank(fault_pct), brand=brand,
                                         average_mileage=as.integer(average_mileage)),
                                         by=.(registration_year)]

app_data <- list(
    stats_model_year=stats_model_year,
    stats_age=stats_age,
    model_stats_by_fault=model_stats_by_fault,
    avg_model_stats_by_fault=avg_model_stats_by_fault,
    brand_stats_by_age=brand_stats_by_age,
    fault_categories=fault_categories,
    years=years,
    ages=ages,
    cars=cars
)

saveRDS(app_data, 'app/app_data.rds', compress='xz')

message(sprintf('Wrote app/app_data.rds (%.1f MB)', file.size('app/app_data.rds') / 1e6))
message(sprintf('  inspection years: %s', paste(sort(unique(wide$year_of_inspection)), collapse=', ')))
message(sprintf('  registration years: %d-%d', min(years), max(years)))
message(sprintf('  models: %d', length(cars)))
if ('Honda CIVIC' %chin% cars) message('  default model "Honda CIVIC" is present') else message('  WARNING: "Honda CIVIC" not found in model list')
