![MicroStrategy logo][logo]

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/mstrio)][cran]
![](http://cranlogs.r-pkg.org/badges/mstrio)

# mstrio: Simple and Secure Access to MicroStrategy Data <!-- omit in toc -->

**MicroStrategy for RStudio and mstrio for R are no longer supported and developed as of March 2024 and have been archived. You can still use the last available version.**

**mstrio** provides a high-level interface for [Python][py_github] and [R][r_github] and is designed to give data scientists and developers simple and secure access to MicroStrategy data. It wraps [MicroStrategy REST APIs][mstr_rest_docs] into simple workflows, allowing users to connect to their MicroStrategy environment, fetch data from cubes and reports, create new datasets, and add new data to existing datasets. And, because it enforces MicroStrategy's user and object security model, you don't need to worry about setting up separate security rules.

With **mstrio**, it's easy to integrate cross-departmental, trustworthy business data in machine learning workflows and enable decision-makers to take action on predictive insights in MicroStrategy Reports, Dossiers, HyperIntelligence Cards, and customized, embedded analytical applications.

**MicroStrategy for RStudio** is an RStudio addin which provides a graphical user interface for **mstrio** methods with the help of which user can perform all of the import and export actions without writing a single line of code manually. MicroStrategy for RStudio is contained within **mstrio** package and is available after installation in the *Addins* dropdown menu.

# Table of Contents <!-- omit in toc -->
<!--ts-->
- [Installation](#installation)
  - [Prerequisites](#prerequisites)
    - [mstrio](#mstrio)
    - [MicroStrategy for RStudio](#microstrategy-for-rstudio)
  - [Install the `mstrio` package](#install-the-mstrio-package)
- [Versioning & Main Features](#versioning--main-features)
  - [Versioning](#versioning)
  - [Main Features](#main-features)
- [Usage](#usage)
  - [Connect to MicroStrategy](#connect-to-microstrategy)
    - [Authentication Methods](#authentication-methods)
    - [SSL Certificates](#ssl-certificates)
    - [Proxy](#proxy)
  - [Import data from Cubes and Reports](#import-data-from-cubes-and-reports)
  - [Export Data into MicroStrategy with Datasets](#export-data-into-microstrategy-with-datasets)
    - [Create a New Dataset](#create-a-new-dataset)
    - [Update a Dataset](#update-a-dataset)
    - [Certify a dataset](#certify-a-dataset)
    - [Limitations](#limitations)
- [More resources](#more-resources)
- [Other](#other)
<!--te-->

# Installation

## Prerequisites

### mstrio
* R 3.6.0+
* MicroStrategy 2019 Update 4 (11.1.4)+

### MicroStrategy for RStudio
* RStudio Desktop 1.2.1335+ or RStudio Server 1.2.5042+
* [CORS enabled on MicroStrategy Library server][cors_manual]
* [Cookies sent by MicroStrategy Library server have `SameSite` parameter set to `None`][same_site_manual]


## Install the `mstrio` package
Installation is easy when using [CRAN][cran]. Read more about installation on MicroStrategy's [product documentation][mstr_help_docs].

```R
install.packages("mstrio")
```

# Versioning & Main Features

## Versioning
Current version: **11.3.5.101** (25 March 2022). Check out [Release Notes][release_notes] to see what's new.

Functionalities may be added to **mstrio** either in combination with annual MicroStrategy platform releases or through updates to platform releases. To ensure compatibility with APIs supported by your MicroStrategy environment, it is recommended to install a version of **mstrio** that corresponds to the version number of your MicroStrategy environment.

The current version of mstrio is 11.3.5.101 and is supported on MicroStrategy 2019 Update 4 (11.1.4) and later. To leverage MicroStrategy for RStudio, mstrio (11.1.4) and MicroStrategy 2019 Update 4 (11.1.4) or higher are required.

If you intend to use mstrio with MicroStrategy version older than 11.1.4, refer to the [CRAN package archive][cran_archive] to download mstrio 10.11.1, which is supported on:

- MicroStrategy 2019 (11.1)
- MicroStrategy 2019 Update 1 (11.1.1)
- MicroStrategy 2019 Update 2 (11.1.2)
- MicroStrategy 2019 Update 3 (11.1.3)

To install a specific, archived version of mstrio, first obtain the URL for the version you need from the [package archive on CRAN][cran_archive], and install as follows:

```R
packageurl <- "https://cran.r-project.org/src/contrib/Archive/mstrio/mstrio_10.11.0.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
```

To install a specific, archived version of mstrio from a local tarball use the following script:

```R
remotes::install_local("path/to/local/tarball/")
```

## Main Features

- Connect to your MicroStrategy environment
- Import data from a Report into an R Data Frame
- Import data from a Cube into an R Data Frame
- Filter cubes and reports by selecting attributes and metrics or specifying a view filter
- Export data into MicroStrategy by creating Datasets
- Update, replace, or append new data to an existing Dataset

To learn more about the package take a look at the **mstrio vignettes**.

# Usage

## Connect to MicroStrategy

The `Connection` object manages your connection to MicroStrategy. Connect to your MicroStrategy environment by providing the URL to the MicroStrategy REST API server, your username, password and the ID of the Project to connect to. When a `Connection` object is created the user will be automatically logged-in.

  **Note**: to log into Library and use mstrio user needs to have UseLibrary privilege.

```R
library(mstrio)

base_url <- "https://your-microstrategy-server.com/MicroStrategyLibrary/api"
username <- "username"
password <- "password"
project_name <- "MicroStrategy Tutorial"

conn <- Connection$new(base_url=base_url, username=username, password=password, project_name=project_name)
```

The URL for the REST API server typically follows this format: _https://your-microstrategy-server.com/MicroStrategyLibrary/api_
Validate that the REST API server is running by accessing _https://your-microstrategy-server.com/MicroStrategyLibrary/api-docs_ in your web browser.

To manage the connection the following methods are made available:

```R
conn$connect()
conn$renew()
conn$close()
conn$status()
```

### Authentication Methods

Currently, supported authentication modes are **Standard** (the default) and **LDAP**. To use LDAP, add `login_mode=16` when creating your `Connection` object:

```R
conn <- Connection$new(base_url=base_url, username=username, password=password, project_name=project_name, login_mode=16)
```

Optionally, the `Connection` object can be created by passing the `identity_token` parameter, which will create a delegated session. The identity token can be obtained by sending a request to MicroStrategy REST API `/auth/identityToken` endpoint.

```R
conn = Connection$new(base_url=base_url, identity_token=identity_token, project_id=project_id)
```

### SSL Certificates

By default, SSL certificates are validated with each API request. To turn this off, use `ssl_verify` flag:

```R
conn <- Connection$new(base_url=base_url, username=username, password=password, project_name=project_name, ssl_verify=FALSE)
```

### Proxy

Optionally, proxy settings can be set for the MicroStrategy `Connection` object.

```R
proxies <- '<ip_address>:<port>'
conn <- Connection$new(base_url=base_url, username=username, password=password, project_name=project_name, proxies=proxies)
```

User can also specify username and password in `proxies` parameter if needed:

```R
proxies <- '<username>:<password>@<ip_address>:<port>'
conn <- Connection$new(base_url=base_url, username=username, password=password, project_name=project_name, proxies=proxies)
```

## Import data from Cubes and Reports

In some cases, better fetching performance can be achieved by utilizing the parallel download of data chunks.
This feature is controlled by the `parallel` flag, but is disabled by default, as sequential download is more stable.
To import the contents of a published Cube into a Data Frame for analysis in R, use the `Cube` class:

```R
my_cube <- Cube$new(connection=conn, cube_id=cube_id)
df <- my_cube$to_dataframe()
```

To import Reports into a DataFrame for analysis in R use the appropriate `Report` class:

```R
my_report <- Report$new(connection=conn, report_id=report_id, parallel=TRUE)
df <- my_report$to_dataframe()
```

By default, all rows are imported when `my_cube$to_dataframe()` or `my_report$to_dataframe()` are called. Filter the
contents of a Cube / Report by passing the selected object IDs for the metrics, attributes, and attribute elements to the
`apply_filters()` method.

To get the list of object IDs of the metrics, attributes, or attribute elements that are available within the
Cube / Report MicroStrategy objects use the following `Cube` / `Report` class properties:

```R
my_cube$metrics
my_cube$attributes
```

If you need to filter by attribute elements, call `my_cube$get_attr_elements()` or `my_report$get_attr_elements()`
which will fetch all unique attribute elements per attribute. The attribute elements are available within the
`Cube` / `Report` object instances:

```R
my_cube$attr_elements
```

Then, choose those elements by passing their IDs to the `my_cube$apply_filters()` method. To see the chosen elements,
call `my_cube$selected_attributes, my_cube$selected_metrics, my_cube$selected_attr_elements`. To clear any active
filters, call `my_cube$clear_filters()`.

```R
my_cube$apply_filters(
   attributes=list("A598372E11E9910D1CBF0080EFD54D63", "A59855D811E9910D1CC50080EFD54D63"),
   metrics=list("B4054F5411E9910D672E0080EFC5AE5B"),
   attr_elements=list("A598372E11E9910D1CBF0080EFD54D63:Los Angeles", "A598372E11E9910D1CBF0080EFD54D63:Seattle"))

my_cube$selected_attributes
my_cube$selected_metrics
my_cube$selected_attr_elements

df <- my_cube$to_dataframe()
```

If you need to exclude specific attribute elements, pass the `operator="NotIn"` parameter to the `apply_filters()`
method.

```R
my_cube$apply_filters(
    attributes=["A598372E11E9910D1CBF0080EFD54D63", "A59855D811E9910D1CC50080EFD54D63"],
    metrics=["B4054F5411E9910D672E0080EFC5AE5B"],
    attr_elements=["A598372E11E9910D1CBF0080EFD54D63:Los Angeles", "A598372E11E9910D1CBF0080EFD54D63:Seattle"],
    operator="NotIn")
df <- my_cube$to_dataframe()
```

## Export Data into MicroStrategy with Datasets

### Create a New Dataset

With **mstrio** you can create and publish single or multi-table Datasets. This is done by passing R Data Frames to the
`Dataset` constructor which translates the data into the format needed by MicroStrategy.

```R
stores_df <- data.frame("store_id" = c(1, 2, 3),
                        "location" = c("New York", "Seattle", "Los Angeles"),
                        stringsAsFactors = FALSE)

sales_df <- data.frame("store_id" = c(1, 2, 3),
                       "category" = c("TV", "Books", "Accessories"),
                       "sales" = c(400, 200, 100),
                       "sales_fmt" = c("$400", "$200", "$100"),
                       stringsAsFactors = FALSE)

ds = Dataset$new(connection=conn, name="Store Analysis")
ds$add_table(name="Stores", data_frame=stores_df, update_policy="add")
ds$add_table(name="Sales", data_frame=sales_df, update_policy="add")
ds$create()
```

By default `Dataset$create()` will create a Dataset, upload the data to the Intelligence Server and publish it. If you just want to _create_ the Dataset and upload the row-level data but leave it unpublished, use `Dataset$create(auto_publish=FALSE)`. If you want to _create_ an empty Dataset, use `Dataset$create(auto_upload=FALSE, auto_publish=FALSE)`. Skipped actions can be performed later using `Dataset.update()` and `Dataset.publish()` methods.

When using `Dataset$add_table()`, R data types are mapped to MicroStrategy data types. By default, numeric data
(integers and floats) are modeled as MicroStrategy Metrics and non-numeric data are modeled as MicroStrategy
Attributes. This can be problematic if your data contains columns with integers that should behave as Attributes
(e.g. a row ID), or if your data contains string-based, numeric-_looking_ data which should be Metrics (e.g. formatted
sales data: `["$450", "$325"]`). To control this behavior, provide a list of columns that you want to convert from one
type to another.

```R
ds$add_table(name="Stores", data_frame=stores_df, update_policy="add",
             to_attribute=list("store_id"))

ds$add_table(name="Sales", data_frame=sales_df, update_policy="add",
             to_attribute=list("store_id"),
             to_metric=list("sales_fmt"))
```

It is also possible to specify where the dataset should be created by providing a folder ID in `Dataset$create(folder_id=folder_id)`.

After creating the Dataset, you can obtain its ID using `Datasets$dataset_id`. This ID is needed for updating the data later.

### Update a Dataset

When the source data changes and users need the latest data for analysis and reporting in MicroStrategy, **mstrio**
allows you to update the previously created dataset.

```R
ds <- Dataset$new(connection=conn, dataset_id=dataset_id)
ds$add_table(name="Stores", data_frame=stores_df, update_policy="update")
ds$add_table(name="Sales", data_frame=stores_df, update_policy="upsert")
ds$update()
```

The `update_policy` parameter controls how the data in the Dataset gets updated. Currently supported update operations are `add` (inserts entirely new data), `update` (updates existing data), `upsert` (simultaneously updates existing data and inserts new data), and `replace` (truncates and replaces the data).

By default `Dataset$update()` will upload the data to the Intelligence Server and publish the Dataset. If you just want
to update the Dataset but not publish the row-level data, use `Dataset$update(auto_publish=FALSE)`. To publish it later, use `Dataset$publish()`.

By default, the raw data is transmitted to the server in increments of 100,000 rows. For very large datasets (>1 GB) it
is beneficial to increase the number of rows transmitted to the Intelligence Server with each request. Do this with the
`chunksize` parameter:

```R
ds$update(chunksize=500000)
```

### Certify a dataset
Use `Dataset$certify()` to certify / decertify an existing dataset.

### Limitations
Updating Datasets that were **not** created using the MicroStrategy REST API is not possible. This applies for example to Cubes created via MicroStrategy Web client.

# More resources

- [Tutorials for mstrio][mstr_datasci_comm]
- [Check out mstrio for Python][py_github]
- [Learn more about the MicroStrategy REST API][mstr_rest_docs]
- [MicroStrategy REST API demo documentation][mstr_rest_demo]

# Other

RStudio and Shiny are trademarks of RStudio, Inc.

[cran]: https://cran.r-project.org/web/packages/mstrio/index.html
[cran_archive]: https://cran.r-project.org/src/contrib/Archive/mstrio/
[py_github]: https://github.com/MicroStrategy/mstrio-py/
[r_github]: https://github.com/MicroStrategy/mstrio
[mstr_datasci_comm]: https://community.microstrategy.com/s/topic/0TO44000000AJ2dGAG/python-r-u108?language=en_US
[mstr_rest_demo]: https://demo.microstrategy.com/MicroStrategyLibrary/api-docs/index.html
[mstr_rest_docs]: https://microstrategy.github.io/rest-api-docs/
[mstr_help_docs]: https://www2.microstrategy.com/producthelp/current/FederatedAnalytics/en-us/Content/mstr_for_rstudio.htm
[cors_manual]: https://microstrategy.github.io/embedding-sdk-docs/config/#enable-cross-origin-resource-sharing-cors
[same_site_manual]: https://microstrategy.github.io/embedding-sdk-docs/config/#allow-samesite-cookies
[release_notes]: https://github.com/MicroStrategy/mstrio/blob/master/NEWS.md
[logo]: https://github.com/MicroStrategy/mstrio/blob/master/man/mstr-logo.png?raw=true
