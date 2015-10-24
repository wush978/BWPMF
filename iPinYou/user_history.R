library(methods)
library(parallel)
cl <- parallel::makeForkCluster(8)
library(R.utils)
library(data.table)
library(magrittr)
library(dplyr)
library(RSQLite)
db <- dbConnect(dbDriver("SQLite"), "ipinyou.db")
clusterEvalQ(cl, {
  library(R.utils)
  library(data.table)
  library(magrittr)
  library(dplyr)
})

clusterEvalQ(cl, {
  bidclass <- c(
    BidID = "character",
    Timestamp = "character",
    iPinyouID = "character",
    UserAgent = "character",
    IP = "character",
    Region = "character",
    City = "character",
    AdExchange = "character",
    Domain = "character",
    URL = "character",
    AnonymousURLId = "character",
    AdSlotId = "character",
    AdSlotWidth = "character",
    AdSlotHeight = "character",
    AdSlotVisibility = "character",
    AdSlotFormat = "character",
    AdSlotFloorPrice = "numeric",
    CreativeID = "character",
    BiddingPrice = "numeric",
    adid = "character", 
    usertag = "character"
  )
})

src.path <- list(
  "2nd" = dir("ipinyou.contest.dataset/training2nd", pattern = "^bid.*txt", full.names = TRUE),
  "3rd" = dir("ipinyou.contest.dataset/training3rd", pattern = "^bid.*txt", full.names = TRUE)
)
retval <- list(
  "2nd" = list(),
  "3rd" = list()
)

for(season in names(src.path)) {
  print(src.path[[season]])
  retval[[season]] <- parLapply(cl, src.path[[season]], function(path) {
    dst <- gsub(".bz2", "", path) %>% path.expand()
    if (!file.exists(dst)) {
      bunzip2(filename = path, destname = dst)
    }
    target <- c("Timestamp", "Domain", "iPinyouID")
    id <- match(target, names(bidclass))
    tb <- 
      fread(dst, sep = "\t", header = FALSE, 
            select = id,
            colClasses = as.vector(bidclass),
            na.strings = "Na",
            col.names = names(bidclass)[sort(id)],
            nrows = -1L, data.table = FALSE)
    tb %>% dplyr::filter(iPinyouID != "") %>%
      dplyr::mutate(Timestamp = strptime(Timestamp, "%Y%m%d%H%M%OS") %>% as.POSIXct)
  })
  df <- rbindlist(retval[[season]]) %>% as.data.frame
  dbWriteTable(db, sprintf("user_history_%s", season), df, 
               overwrite = TRUE, append = FALSE) %>%
    stopifnot
  gc()
  clusterEvalQ(cl, gc())
}
stopCluster(cl)
for(tbname in dbListTables(db)) {
  sprintf("row count of %s: %d\n",
          tbname,
          dbGetQuery(db, sprintf("SELECT count(*) from %s", tbname))[[1]][1]) %>%
    cat
  dbGetQuery(db, sprintf("SELECT * FROM %s LIMIT 10", tbname))
  for(target in c("Timestamp", "iPinyouID")) {
    sql <- sprintf(
      "CREATE INDEX IF NOT EXISTS Timestamp_%s ON %s (%s)",
      tbname,
      tbname,
      target)
    dbGetQuery(db, sql)
  }
  
}
dbDisconnect(db)
