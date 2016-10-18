##### Facebook Analytics #######
####Step 1 : Facebook Authorizations ############
install.packages("Rfacebook")
library(Rfacebook)
install.packages("Rook")
library(Rook)
fb_oauth <- fbOAuth(app_id= "1066600210123639", app_secret= "07b74fac017c79656f8e7e0f9f9882ea")
save(fb_oauth, file= "fb_oauth")
load("fb_oauth")
me<-getUsers("me",token=fb_oauth)
me$name
View(me)
###########################################

#####Step 2: facebook data mining with RFacebook Package
pages <- searchPages( string="MakeMyTrip.com", token=fb_oauth, n=100 )
fb_page <- getPage(page= "120740541030", token=fb_oauth,n=35,feed=TRUE,reactions=TRUE)
head(fb_page,n=1)
pac<-subset(fb_page,fb_page$message=="PayAtCheckout")
pac
table(fb_page$message,fb_page$likes_count)
