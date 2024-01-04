#installed.packages("qrcode")


library(qrcode)
code <- qr_code("https://kqcmew-hayden0bock.shinyapps.io/CARBOMOW/")
print(code)
plot(code)
