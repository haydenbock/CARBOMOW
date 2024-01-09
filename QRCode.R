#installed.packages("qrcode")


library(qrcode)
code <- qr_code("https://kqcmew-hayden0bock.shinyapps.io/Carbomow-v3/")
print(code)
plot(code)
