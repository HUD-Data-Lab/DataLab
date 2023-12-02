# Copyright (C) 2023 Gwen Beebe
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>. 

library(tidyverse)
library(stringr)
library(janitor)
library(leaflet)
library(shiny)
library(shinythemes)
  # https://leaflet-extras.github.io/leaflet-providers/preview/
source("HCC_map_setup.R")

apartment_table <- read_airtable(
  airtable("tblkXnU696PKRCD44",
           "appPF91MMKhIUKGvD")) %>%
  clean_names() %>%
  mutate(complex = as.character(complex))

complexes_table <- read_airtable(
  airtable("tblVBYtoLvnkD4rDJ",
           "appPF91MMKhIUKGvD")) %>%
  clean_names()

management_table <- read_airtable(
  airtable("tblUsVVz3wav6uubv",
           "appPF91MMKhIUKGvD")) %>%
  clean_names()