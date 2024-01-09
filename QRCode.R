#installed.packages("qrcode")


library(qrcode)
code <- qr_code("https://kqcmew-hayden0bock.shinyapps.io/CARBOWMOW/")
print(code)
plot(code)
