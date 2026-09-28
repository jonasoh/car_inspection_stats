# Export the Shiny app as a static webR/shinylive site into site/.
#
# Usage: Rscript build.R
#
# Then preview locally with, e.g.:
#   Rscript -e 'httpuv::runStaticServer("site")'
# (opening site/index.html directly via file:// won't work, since the
# app's service worker needs to be served over http).
#
# Deploy by copying the contents of site/ to any static web host.

if (!requireNamespace('shinylive', quietly=TRUE)) {
    stop('Package "shinylive" is required. Install it with install.packages("shinylive").')
}

shinylive::export(
    'app',
    'site',
    template_params=list(title='Car inspection stats')
)
