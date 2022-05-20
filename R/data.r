#' Norwegian Covid-19 data for ICU and hospitalization
#'
#' This data was extracted on 2022-05-04.
#'
#' @format A splfmt_rts_data_v1 with 919 rows and 18 variables:
#' \describe{
#'   \item{granularity_time}{day/isoweek}
#'   \item{granularity_geo}{nation}
#'   \item{country_iso3}{nor}
#'   \item{location_code}{norge}
#'   \item{border}{2020}
#'   \item{age}{total}
#'   \item{isoyear}{Isoyear of event}
#'   \item{isoweek}{Isoweek of event}
#'   \item{isoyearweek}{Isoyearweek of event}
#'   \item{season}{Season of event}
#'   \item{seasonweek}{Seasonweek of event}
#'   \item{calyear}{Calyear of event}
#'   \item{calmonth}{Calmonth of event}
#'   \item{calyearmonth}{Calyearmonth of event}
#'   \item{date}{Date of event}
#'   \item{icu_with_positive_pcr_n}{Number of new admissions to the ICU with a positive PCR test}
#'   \item{hospitalization_with_covid19_as_primary_cause_n}{Number of new hospitalizations with Covid-19 as the primary cause}
#' }
#' @source \url{https://github.com/folkehelseinstituttet/surveillance_data/blob/master/covid19/_DOCUMENTATION_data_covid19_hospital_by_time.txt}
"norway_covid19_icu_and_hospitalization"





#' Covid-19 data for confirmed cases in Norway (nation and county)
#'
#' This data comes from MSIS registry, and is recorded based on the test date.
#' Number of cases and percentage per 100.000 population are recorded.
#'
#' This data was extracted on 2022-05-04.
#'
#'
#'
#' @format A splfmt_rts_data_v1 with 11028 rows and 18 variables:
#' \describe{
#'   \item{granularity_time}{day/isoweek}
#'   \item{granularity_geo}{nation, county}
#'   \item{country_iso3}{nor}
#'   \item{location_code}{norge, 11 counties}
#'   \item{border}{2020}
#'   \item{age}{total}
#'   \item{isoyear}{Isoyear of event}
#'   \item{isoweek}{Isoweek of event}
#'   \item{isoyearweek}{Isoyearweek of event}
#'   \item{season}{Season of event}
#'   \item{seasonweek}{Seasonweek of event}
#'   \item{calyear}{Calyear of event}
#'   \item{calmonth}{Calmonth of event}
#'   \item{calyearmonth}{Calyearmonth of event}
#'   \item{date}{Date of event}
#'   \item{covid19_cases_testdate_n}{Number of confirmed covid19 cases}
#'   \item{covid19_cases_testdate_pr100000}{Percentage per 100.000 of confirmed covid19 cases}
#' }
#' @source \url{https://github.com/folkehelseinstituttet/surveillance_data/blob/master/covid19/_DOCUMENTATION_data_covid19_msis_by_time_location.txt}
"covid19_msis_cases_by_time_location"


