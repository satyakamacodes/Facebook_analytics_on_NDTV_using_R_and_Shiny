closeAllConnections()
rm(list = ls())

print(require(Rfacebook))


fbAuth <- fbOAuth(app_id = "XXXX",
                  app_secret = "XXXX")

start.time <- Sys.time()

ndtvpage <- getPage("ndtv",
                     token = fbAuth,
                     n = 2000,
                     since='2017/11/01', 
                     until='2018/02/28',
                     feed = T,
                     reactions = T,
                     verbose = T)

end.time <- Sys.time()

process.time <- print(end.time - start.time)

#View(data.frame(ndtvpage))

setwd("SET YOUR WORKING DIRECTORY")

saveRDS(ndtvpage,
        "ndtvpage.rds")

View(data.frame(ndtvpage))


