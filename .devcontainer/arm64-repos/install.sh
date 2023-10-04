#!/bin/bash

set -e

ARCH=$(uname -m)

## source install if using RSPM and arm64 image
if [ "$ARCH" = "aarch64" ]; then

CRAN="https://cloud.r-project.org"

## Add a default CRAN mirror
echo "options(repos = c(CRAN = '${CRAN}'), download.file.method = 'libcurl')" >"${R_HOME}/etc/Rprofile.site"

## Set HTTPUserAgent for RSPM (https://github.com/rocker-org/rocker/issues/400)
cat <<EOF >>"${R_HOME}/etc/Rprofile.site"
# https://docs.rstudio.com/rspm/admin/serving-binaries/#binaries-r-configuration-linux
options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])))
EOF

fi

