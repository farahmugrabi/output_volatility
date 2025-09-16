## By Quentin Bro de Comères
# rm(list=ls())

# get_data("https://data-api.ecb.europa.eu/service/data","MNA.Q.N.AT.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.V.N")
# ecb_api = "https://data-api.ecb.europa.eu/service/data"
# get_data(ecb_api, "MNA.Q.N.AT.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.V.N")

get_data = function(api, key, filter = NULL, ...) {
  
  make_request = function (query_url, header_type, ...) 
  {
    accept_headers <- c(metadata = "application/vnd.sdmx.genericdata+xml;version=2.1", 
                        data = "application/vnd.sdmx.structurespecificdata+xml;version=2.1")
    req <- httr::GET(query_url, httr::add_headers(Accept = accept_headers[header_type], 
                                                  `Accept-Encoding` = "gzip, deflate"), ...)
    check_status(req)
    req
  }
  
  check_status = function (req) 
  {
    if (req$status_code >= 400) 
      stop("HTTP failure: ", req$status_code, "\n", httr::content(req, 
                                                                  "text"))
  }
  
  create_query_url = function (api_key, key, filter = NULL) 
  {
    ### Change API Key here
    url <- api_key
    flow_ref <- regmatches(key, regexpr("^[[:alnum:]]+", key))
    key_q <- regmatches(key, regexpr("^[[:alnum:]]+\\.", key), 
                        invert = TRUE)[[1]][2]
    if (any(names(filter) == "")) {
      stop("All filter parameters must be named!")
    }
    if ("updatedAfter" %in% names(filter)) {
      filter$updatedAfter <- curl::curl_escape(filter$updatedAfter)
    }
    names <- curl::curl_escape(names(filter))
    values <- curl::curl_escape(as.character(filter))
    query <- paste0(names, "=", values, collapse = "&")
    query <- paste0("?", query)
    query_url <- paste0(url, "/", flow_ref, "/", key_q, query)
    query_url
  }
  
  api_key = api
  if (!"detail" %in% names(filter)) {
    filter <- c(filter, detail = "dataonly")
  }
  if (!filter[["detail"]] %in% c("full", "dataonly")) {
    return(get_dimensions(key))
  }
  query_url <- create_query_url(api_key, key, filter = filter)
  req <- make_request(query_url, "data", ...)
  tmp <- tempfile()
  writeLines(httr::content(req, "text", encoding = "utf-8"), 
             tmp)
  result <- rsdmx::readSDMX(tmp, FALSE)
  unlink(tmp)
  df <- as.data.frame(result)
  df <- structure(df, class = c("tbl_df", "tbl", "data.frame"), 
                  names = tolower(names(df)))
  df
  
}



