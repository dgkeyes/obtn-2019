# Packages ----------------------------------------------------------------

library(fs)


# Copy plots to Dropbox ---------------------------------------------------

# Get rid of existing directory

# dir_delete("/Users/davidkeyes/Dropbox/ObtN 2019-2020/2019 ObtN/2019 ObtN Layout DKNH Only/2019 ObtN Artwork/plots")

# Copy in new plots

dir_copy("plots",
         "/Users/davidkeyes/Dropbox/ObtN 2019-2020/2019 ObtN/2019 ObtN Layout DKNH Only/2019 ObtN Artwork")


