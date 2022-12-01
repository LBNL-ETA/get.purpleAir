#' Get sensors within a bounding box, with fields sensor_index, latitude,
#' longitude, altitude
#'
#' @param read.key API read key from Purple Air
#' @param nwlng longitude of the northwest corner
#' @param nwlat latitude of the northwest corner
#' @param selng longitude of the southeast corner
#' @param selat latitude of the southeast corner
#' @param location.type location of the sensor. Possible values are "outside",
#'     "inside", NULL when NULL, the location filter is not applied in the url
#' @return A list of sensors.
#' @export
#' @examples
#' get.sensor.list.bbox("********-****-****-****-************", -122.5801, 45.5925, -122.46593, 45.4664)
get.sensor.list.bbox <- function(read.key, nwlng, nwlat, selng, selat, location.type="outside") {
    if (is.null(location.type)) {
        url = sprintf("https://api.purpleair.com/v1/sensors?fields=location_type%%2Clatitude%%2Clongitude%%2Caltitude&nwlng=%.5f&nwlat=%.5f&selng=%.5f&selat=%.5f", nwlng, nwlat, selng, selat)
    } else {
        if (location.type == "outside") {
            loc.num = 0
        } else {
            loc.num = 1
        }
        url = sprintf("https://api.purpleair.com/v1/sensors?fields=location_type%%2Clatitude%%2Clongitude%%2Caltitude&location_type=%s&nwlng=%.5f&nwlat=%.5f&selng=%.5f&selat=%.5f", loc.num, nwlng, nwlat, selng, selat)
    }
    response = httr::GET(url, httr::add_headers(`X-API-Key` = read.key))
    print(response)
    response
}

#' Convert sensor list response to a dataframe
#'
#' @param response Output of get.sensor.list.bbox
#' @return A dataframe
#' @export
#' @examples
#' get.sensor.list.bbox("********-****-****-****-************", -122.5801, 45.5925, -122.46593, 45.4664)
sensor.list.response.to.df <- function(response) {
    data.content = httr::content(response)$data
    stations <- tibble::tibble(sensor_index = numeric(),
                               location_type = numeric(), latitude = numeric(),
                               longitude = numeric(), altitude = numeric())

    for (i in seq_along(data.content)) {
        stations[i, ] <- data.content[[i]]
    }
    stations
}

#' create a sensor group
create.group <- function(write.key) {
    ## curl -X POST https://api.purpleair.com/v1/groups -H "Content-Type: application/json" -H "X-API-Key: ********-****-****-****-************" -d '{"name":"My Group"}'
    httr::POST("https://api.purpleair.com/v1/groups",
               body = list(name = "East Portland Stations"), encode = "json",
               httr::add_headers(`Content-Type` = "application/json",
                                 `X-API-Key` = write.key))
}

#' Add a member to a group
add.one.member <- function(write.key, group.id, sensor.id) {
    ## curl -X POST https://api.purpleair.com/v1/groups/***/members -H "Content-Type: application/json" -H "X-API-Key: ********-****-****-****-************" -d '{"sensor_index":"*****"}'
    httr::POST(sprintf("https://api.purpleair.com/v1/groups/%d/members", group.id),
               body = sprintf('{"sensor_index":"%d"}', sensor.id),
               httr::add_headers(`Content-Type` = "application/json",
                                 `X-API-Key` = write.key))
}

#' get details of the sensor group
get.group.detail <- function(read.key, group.id) {
    ## GET https://api.purpleair.com/v1/groups/**** HTTP/1.1
    ## X-API-Key: ********-****-****-****-************
    httr::GET(sprintf("https://api.purpleair.com/v1/groups/%d", group.id),
              httr::add_headers(`X-API-Key` = read.key))
}

delete.group <- function(write.key, group.id) {
    ## DELETE https://api.purpleair.com/v1/groups/*** HTTP/1.1
    ## api_key: my-api-write-key
    httr::DELETE(sprintf("https://api.purpleair.com/v1/groups/%d", group.id),
                 httr::add_headers(`X-API-Key` = write.key))
}

## have problem, cannot get data
## "The url used in the request was invalid. Please check it a...
get.data.from.group <- function(read.key, group.id, start.time, end.time) {
    ## curl -X GET https://api.purpleair.com/v1/groups/***/members/*** -H "X-API-Key: ********-****-****-****-************"
    ## https://api.purpleair.com/v1/groups/:group_id/members/:member_id/history/csv
    httr::GET(sprintf("https://api.purpleair.com/v1/groups/%d/members/history/csv?start_timestamp=%s&end_timestamp=%s&fields=sensor_index%%2Ctime_stamp%%2Ctemperature%%2Chumidity%%2Cpm2.5", group.id, start.time, end.time),
              httr::add_headers(`X-API-Key` = read.key))
}

get.data.from.sensor <- function(read.key, sensor.id, start.time, end.time) {
    ## https://api.purpleair.com/v1/sensors/:sensor_index/history/csv
    httr::GET(sprintf("https://api.purpleair.com/v1/sensors/%d/history/csv?start_timestamp=%s&end_timestamp=%s&fields=sensor_index%%2Ctime_stamp%%2Ctemperature%%2Chumidity%%2Cpm2.5",
                      sensor.id, start.time, end.time),
              httr::add_headers(`X-API-Key` = read.key))
}
