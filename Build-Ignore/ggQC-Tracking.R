require(jsonlite)

txt <- paste0("https://cranlogs.r-pkg.org/downloads/daily/2016-01-01:", Sys.Date(),"/ggQC,IQCC,qcc")
download.data <- jsonlite::fromJSON(txt , simplifyDataFrame = T)

head(download.data)

df <- rbind(
data.frame(download.data$downloads[1], pkg=download.data$package[1]),
data.frame(download.data$downloads[2], pkg=download.data$package[2])#,
#data.frame(download.data$downloads[3], pkg=download.data$package[3])
)
df$day <- as.POSIXct(df$day)

require(ggplot2)
ggplot(df[df$day > "2018-10-01",], aes(x=day, y=downloads, color=pkg)) +
  geom_point() +
  geom_path() #+
  #scale_y_continuous(limits = c(0,30))

