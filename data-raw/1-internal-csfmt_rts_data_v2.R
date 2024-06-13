library(data.table)

# csfmt_rts_data_v2_date_to
csfmt_rts_data_v2_date_to <- data.table(
  granularity_time = "date",
  date = c(seq(as.Date("1950-01-01"),as.Date("2100-01-01"),1), as.Date("9999-09-09"))
)
csfmt_rts_data_v2_date_to[, isoyear := cstime::date_to_isoyear_n(date)]
csfmt_rts_data_v2_date_to[, isoweek := cstime::date_to_isoweek_n(date)]
csfmt_rts_data_v2_date_to[, isoyearweek := cstime::date_to_isoyearweek_c(date)]
csfmt_rts_data_v2_date_to[, isoquarter := cstime::date_to_isoquarter_n(date)]
csfmt_rts_data_v2_date_to[, isoyearquarter := cstime::date_to_isoyearquarter_c(date)]
csfmt_rts_data_v2_date_to[, season := cstime::date_to_season_c(date)]
csfmt_rts_data_v2_date_to[, seasonweek := cstime::date_to_seasonweek_n(date)]
csfmt_rts_data_v2_date_to[, calyear := cstime::date_to_calyear_n(date)]
csfmt_rts_data_v2_date_to[, calmonth := cstime::date_to_calmonth_n(date)]
csfmt_rts_data_v2_date_to[, calyearmonth := cstime::date_to_calyearmonth_c(date)]
setkey(csfmt_rts_data_v2_date_to, date)

# csfmt_rts_data_v2_season_to
csfmt_rts_data_v2_season_to <- csfmt_rts_data_v2_date_to[
  ,
  .(
    granularity_time = "season",
    date = max(date)),
  keyby=.(season)
]
csfmt_rts_data_v2_season_to[, isoyear := NA_integer_]
csfmt_rts_data_v2_season_to[, isoweek := NA_integer_]
csfmt_rts_data_v2_season_to[, isoyearweek := NA_character_]
csfmt_rts_data_v2_season_to[, isoquarter := NA_integer_]
csfmt_rts_data_v2_season_to[, isoyearquarter := NA_character_]
csfmt_rts_data_v2_season_to[, seasonweek := NA_integer_]
csfmt_rts_data_v2_season_to[, calyear := NA_integer_]
csfmt_rts_data_v2_season_to[, calmonth := NA_integer_]
csfmt_rts_data_v2_season_to[, calyearmonth := NA_character_]
setkey(csfmt_rts_data_v2_season_to, season)

# csfmt_rts_data_v2_isoyearweek_to
csfmt_rts_data_v2_isoyearweek_to <- data.table(
  granularity_time = "isoyearweek",
  isoyearweek = unique(csfmt_rts_data_v2_date_to$isoyearweek)
)
csfmt_rts_data_v2_isoyearweek_to[, isoyear := cstime::isoyearweek_to_isoyear_n(isoyearweek)]
csfmt_rts_data_v2_isoyearweek_to[, isoweek := cstime::isoyearweek_to_isoweek_n(isoyearweek)]
csfmt_rts_data_v2_isoyearweek_to[, isoquarter := cstime::isoyearweek_to_isoquarter_n(isoyearweek)]
csfmt_rts_data_v2_isoyearweek_to[, isoyearquarter := cstime::isoyearweek_to_isoyearquarter_c(isoyearweek)]
csfmt_rts_data_v2_isoyearweek_to[, season := cstime::isoyearweek_to_season_c(isoyearweek)]
csfmt_rts_data_v2_isoyearweek_to[, seasonweek := cstime::isoyearweek_to_seasonweek_n(isoyearweek)]
csfmt_rts_data_v2_isoyearweek_to[, calyear := NA_integer_]
csfmt_rts_data_v2_isoyearweek_to[, calmonth := NA_integer_]
csfmt_rts_data_v2_isoyearweek_to[, calyearmonth := NA_character_]
csfmt_rts_data_v2_isoyearweek_to[, date := cstime::isoyearweek_to_last_date(isoyearweek)]
csfmt_rts_data_v2_isoyearweek_to[isoyearweek=="9999-36", date := as.Date("9999-09-12")]
setkey(csfmt_rts_data_v2_isoyearweek_to, isoyearweek)

# csfmt_rts_data_v2_isoyear_to
csfmt_rts_data_v2_isoyear_to <- data.table(
  granularity_time = "isoyear",
  isoyear = c(1900:2100, 9999)
)
csfmt_rts_data_v2_isoyear_to[, isoweek := cstime::isoyear_to_last_isoweek_n(isoyear)]
csfmt_rts_data_v2_isoyear_to[, isoyearweek := cstime::isoyear_to_last_isoyearweek_c(isoyear)]
csfmt_rts_data_v2_isoyear_to[, isoquarter := NA_integer_]
csfmt_rts_data_v2_isoyear_to[, isoyearquarter := NA_character_]
csfmt_rts_data_v2_isoyear_to[, season := NA_character_]
csfmt_rts_data_v2_isoyear_to[, seasonweek := NA_real_]
csfmt_rts_data_v2_isoyear_to[, calyear := NA_integer_]
csfmt_rts_data_v2_isoyear_to[, calmonth := NA_integer_]
csfmt_rts_data_v2_isoyear_to[, calyearmonth := NA_character_]
csfmt_rts_data_v2_isoyear_to[, date := cstime::isoyear_to_last_date(isoyear)]
setkey(csfmt_rts_data_v2_isoyear_to, isoyear)


# saving internal

env = new.env()
if(file.exists("R/sysdata.rda")) load("R/sysdata.rda", envir = env)

env$csfmt_rts_data_v2_date_to <- csfmt_rts_data_v2_date_to
env$csfmt_rts_data_v2_season_to <- csfmt_rts_data_v2_season_to
env$csfmt_rts_data_v2_isoyearweek_to <- csfmt_rts_data_v2_isoyearweek_to
env$csfmt_rts_data_v2_isoyear_to <- csfmt_rts_data_v2_isoyear_to

for(i in names(env)){
  .GlobalEnv[[i]] <- env[[i]]
}
txt <- paste0("usethis::use_data(",paste0(names(env),collapse=","),", overwrite = TRUE, internal = TRUE, compress = 'xz', version = 3)")
eval(parse(text = txt))



