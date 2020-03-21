

# make sure the neccessary libraries are loaded.
library(RWordPress)
library(knitr)


# Put the WordPress username / password and URL for your WordPress site.
options(WordpressLogin = c(intperspective = "z$&%XQb65b2Zr"),
        WordpressURL = "https://utrechthistoricalinsight.wordpress.com/test")

# knitr parses the Rmd file and pushes the html to your WordPress site
# ?knit2wp for details
knit2wp('index.Rmd'
        , title = 'Test'
        , action = "newPost"
        ,publish = TRUE)