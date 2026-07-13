# FIAapi

An R package providing a lightweight interface to the USDA Forest Inventory and Analysis (FIA) EVALIDator API.

## Overview

`FIAapi` simplifies access to FIA EVALIDator data from R, allowing users to:

- Submit queries to the FIA EVALIDator API
- Retrieve estimates directly into R
- Convert API responses into analysis-ready data frames
- Integrate FIA estimates into reproducible workflows

The package is intended for FIA analysts, researchers, forest managers, and students working with FIA data.

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("radt0005/FIAapi")
```

## Example

```r
library(FIAapi)

# Example API request
# Replace with actual package functions

result <- fia_query(
  state = "VA",
  variable = "BIO_ACRE"
)

head(result)
```

## Why Use FIAapi?

The FIA EVALIDator web interface is powerful but can be difficult to integrate into automated workflows. `FIAapi` allows users to:

- Work entirely within R
- Reproduce analyses with scripts
- Automate repeated queries
- Integrate FIA estimates into larger analytical pipelines

## Data Source

This package provides access to data served through the USDA Forest Service FIA EVALIDator API.

Forest Inventory and Analysis (FIA) is the nation's continuous forest census program and provides information on:

- Forest area
- Tree volume
- Biomass
- Carbon
- Growth, removals, and mortality
- Forest composition and condition

For more information, visit:

https://www.fs.usda.gov/research/programs/fia

## Development Status

This package is currently under active development.

Interfaces, function names, and returned objects may change as the package evolves.

## Contributing

Issues, feature requests, and pull requests are welcome.

Please open an issue on GitHub to discuss proposed changes.

## License

MIT License

Copyright (c) 2026 Philip Radtke
`
