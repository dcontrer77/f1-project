## get list of existing race data
filenames <- list.files(path = "data", pattern="*.csv")

## remove old race data and replace with new race data
# file.remove(file.path("data", filenames))
# temp <- tempfile(fileext = ".zip")
# download.file(url = "http://ergast.com/downloads/f1db_csv.zip", destfile = temp)
# unzip(zipfile = temp, exdir = "data")
# rm(temp)

dat <- paste0("data/", filenames) %>% 
  lapply(read.csv, header = F)

## create list of column names for each data frame
col_names <- list("circuits" = c("circuitId", "circuitRef", "name", 
                                 "location", "country", "lat", 
                                 "lng", "alt", "url"), 
                  "constructor_results" = c("constructorResultsId", "raceId", "constructorId", 
                                            "points", "status"),
                  "constructor_standings" = c("constructorStandingsId", "raceId", "constructorId", 
                                              "points", "position", "positionText", "wins"),
                  "constructors" = c("constructorId", "constructorRef", "name", 
                                     "nationality", "url"),
                  "driver_standings" = c("driverStandingsId", "raceId", "driverId", "points", 
                                         "position", "positionText", "wins"),
                  "driver" = c("driverId", "driverRef", "number", "code", "firstName", 
                               "lastName", "dob", "nationality", "url"),
                  "lap_times" = c("raceId", "driverId", "lap", 
                                  "position", "time", "milliseconds"),
                  "pit_stops" = c("raceId", "driverId", "stop", "lap", 
                                  "time", "duration", "milliseconds"),
                  "qualifying" = c("qualifyId", "raceId", "driverId", "constructorId", 
                                   "number", "position", "q1", "q2", "q3"),
                  "races" = c("raceId", "year", "round", "circuitId", 
                              "name", "date", "time", "url"),
                  "results" = c("resultId", "raceId", "driverId", "constructorId", 
                                "number", "grid", "position", "positionText", "positionOrder", 
                                "points", "laps", "time", "milliseconds", "fastestLap", "rank", 
                                "fastestLapTime", "fastestLapSpeed", "statusId"),
                  "seasons" = c("year", "url"),
                  "status" = c("statusId", "status"))

## name individual data frames
filenames <- gsub(pattern = "\\..*", replacement = "", x = filenames)
names(dat) <- filenames

## rename individual data frames
for (i in filenames) {
  colnames(dat[[i]]) <- col_names[[i]]
}

dat <- lapply(dat, function(x){
  x[] <- lapply(x, function(x) replace(x, x %in% "\\N", NA))
  return(x)
})

## clean workspace
rm(col_names, filenames, i)

## data cleaning
dat[["circuits"]] <- dat[["circuits"]] %>% 
  select(-alt, -url)
dat[["constructor_results"]] <- dat[["constructor_results"]] %>% 
  select(-status)
dat[["constructors"]] <- dat[["constructors"]] %>% 
  select(-url)
dat[["driver_standings"]] <- dat[["driver_standings"]] %>% 
  select(-positionText)
dat[["driver"]] <- dat[["driver"]] %>% 
  select(-code, -number, -url) %>% 
  mutate(dob = dob %>% as.POSIXct(tz = "UTC"))
dat[["lap_times"]] <- dat[["lap_times"]] %>% 
  mutate(time = time %>% ms)
dat[["pit_stops"]] <- dat[["pit_stops"]] %>%
  select(-time) %>% 
  mutate(duration = duration %>% as.character %>% as.numeric)
dat[["qualifying"]] <- dat[["qualifying"]] %>% 
  mutate(q1Ms = q1 %>% ms %>% as.numeric * 1000,
         q2Ms = q2 %>% ms %>% as.numeric * 1000,
         q3Ms = q3 %>% ms %>% as.numeric * 1000) %>% 
  select(-q1, -q2, -q3)
dat[["races"]] <- dat[["races"]] %>% 
  select(-time, -url) %>% 
  mutate(date = date %>% as.POSIXct(tz = "UTC"))
dat[["results"]] <- dat[["results"]] %>% 
  mutate(number = number %>% as.integer,
         position = position %>% as.integer,
         endResult = ifelse(position %in% NA, positionText %>% as.character, "C"),
         milliseconds = milliseconds %>% as.character %>% as.numeric,
         fastestLap = fastestLap %>% as.integer,
         rank = rank %>% as.integer,
         fastestLapTime = fastestLapTime %>% ms,
         fastestLapTimeMs = fastestLapTime %>% ms %>% as.numeric * 1000,
         fastestLapSpeed = fastestLapSpeed %>% as.character %>% as.numeric) %>% 
  select(-positionText, -time)