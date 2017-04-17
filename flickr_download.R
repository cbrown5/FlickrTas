# Download tagged photos from flickr
# CJ Brown 17 April 2017

rm(list = ls())
library(httr)
library(magrittr)
library(jsonlite)
library(xml2)
library(purrr)
library(PlotTools)

#To check one photo see:
#flickr.photos.geo.photosForLocation
#http://flickr.com/photo.gne?id=33201638594

thiswd <- "~/Code/FlickrTas/data/"

#
# Params
#

#save(api_key, file = paste0(thiswd, "api_key.rda"))
#save(api_secret, file = paste0(thiswd, "api_secret.rda"))

#You will need to get and load your own api_key and api_secret.
# These can easily be obtained through your Flickr account.
# Below files just contain characters with the key and secret.
load(paste0(thiswd, "api_key.rda"))
load(paste0(thiswd, "api_secret.rda"))

#list of hashtags to search on. Each element is a unique search.
hashtag_list <- c("tasmania,endemic", "native,hen", "nativehen","blackcurrawong", "greenrosella","scrubtit","honeyeater","duskyrobin","scrubwren","pardalote","thornbill","yellowwattlebird")
nsearch <- length(hashtag_list)

save(hashtag_list,
    file = paste0(thiswd,"searches.rda"))

npages <- 4
nperpage <- "250" #limit for geo queries is 250

#bbox params
#minimum_longitude, minimum_latitude, maximum_longitude, maximum_latitude
bbox <- c(144.44, -43.69, 148.63, -39.47)

# ---------------
# Authorize
# ---------------
#This should open a window in your browser and request authorization. 

flickr.app <- oauth_app("r to flickr",api_key,api_secret)

flickr.endpoint <- oauth_endpoint(
  request = "https://www.flickr.com/services/oauth/request_token"
  , authorize = "https://www.flickr.com/services/oauth/authorize"
  , access = "https://www.flickr.com/services/oauth/access_token"
)


tok <- oauth1.0_token(
  flickr.endpoint
  , flickr.app
  , cache = F
)

# ---------------
# Flickr download
# ---------------

#Loop over searches
stime <- proc.time()

for (isearch in 2:nsearch){
#for (isearch in 1:nsearch){
print(paste0(isearch,"/",nsearch))
hashtag <- hashtag_list[isearch]
print(hashtag)
#
# Get photo IDs
#

dout <- NULL

for (ipage in 1:npages){
    dat <- GET(url=sprintf(
    "https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=%s&format=json&nojsoncallback=1&tags=%s&tag_mode=all&per_page=%s&page=%i&bbox=%f,%f,%f,%f",
    api_key,
    hashtag,
    nperpage,
    ipage,
    bbox[1],
    bbox[2],
    bbox[3],
    bbox[4],
    tok$credentials$oauth_token
    )) %>%
    content(as = "text") %>%
    jsonlite::fromJSON()

    dout <- c(dout, list(dat))

    #end if we maxed out pages
    if(length(dat$photos$photo$id) < nperpage) break
}

save(dout,
    file = paste0(thiswd,"photo_info",isearch, ".rda"))

#
# Get tags and coordinates attached to photos
#

#pages with photos
npget <- sum(unlist(lapply(dout, function(x) length(x$photos$photo)>0)))

length(dout) #actual pages downloaded
xy <- NULL
tags <- NULL

for (ipage in 1:npget){
dat <- dout[[ipage]]
npic <- length(dat$photos$photo$id)
xytemp <- data.frame(x = rep(NA, npic), y = rep(NA, npic),
    accuracy = rep(NA, npic), author = rep(NA, npic), photoid = rep(NA, npic))
tagstemp <- lapply(1:npic, function(x) NA)

for (ipic in 1:npic){
    photoid <- dat$photos$photo$id[ipic]
    thispic <- GET(url=sprintf(
      "https://api.flickr.com/services/rest/?method=flickr.tags.getListPhoto&api_key=%s&photo_id=%s",
       api_key,
        photoid
      )) %>%
        content(as = "text")
    x <- read_xml(thispic)
    tagstemp[[ipic]] <- xml_text(xml_find_all(x, "//tag"))
    xytemp$author[ipic] <- xml_attr(xml_find_all(x, "//tag"), "author")[1]
    xytemp$photoid[ipic] <- photoid
    thispicll <- GET(url=sprintf(
      "https://api.flickr.com/services/rest/?method=flickr.photos.geo.getLocation&api_key=%s&photo_id=%s",
       api_key,
        photoid
      )) %>%
        content(as = "text")
    x2 <- read_xml(thispicll)
    outcome <- xml_attr(x2, "stat")
    print(ipic)
    print(photoid)
    if (outcome != "fail") {
         xtemp <- xml_find_all(x2, "//location")
        xytemp$x[ipic] <- as.numeric(xml_attr(xtemp, "latitude"))
        xytemp$y[ipic] <- as.numeric(xml_attr(xtemp, "longitude"))
        xytemp$accuracy[ipic] <- as.numeric(xml_attr(xtemp, "accuracy"))
        }
    }
    xy <- rbind(xy, xytemp)
    tags <- c(tags, tagstemp)

}
savelist <- list(xy = xy, tags = tags)
    save(savelist,
        file = paste0(thiswd,"xy_tags_search_",isearch, ".rda"))

    #Sys.sleep(900) #pause for 15 minutes so as not to abuse rate limits
}

proc.time() - stime
