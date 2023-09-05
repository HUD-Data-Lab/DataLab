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

if(!exists("kit_type")) {
  kit_type <- ""
}

if (kit_type == "old_kit") {

  relevant_CoC <- "XX-500"

  # report_start_date <- ymd(Export[1,]$ExportStartDate)
  # report_end_date <- ymd(Export[1,]$ExportEndDate)
  report_start_date <- ymd("2020-10-1")
  report_end_date <- ymd("2021-9-30")

} else {

  relevant_CoC <- "XX-501"

  if (exists("lookback_stop_date")) {
    report_start_date <- lookback_stop_date
    report_end_date <- lookback_stop_date %m+% years(8) %m-% days(1)
  } else {
    report_start_date <- ymd("2021-10-1")
    report_end_date <- ymd("2022-9-30")
  }
}
