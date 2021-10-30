# cricr
How to install
devtools::install_github("kusaldesilva"/"cricr")

Example for scrape match details of ICC matches

iccmatchdata(MatchFormat = "TEST", TeamGender = "MEN")
Possible Match Formats:TEST, ODI, T20I, T20, List_A, FIRST_CLASS, OTHER
Possible Team Genders: Men, Women

iplmatchdata(Year = 2008, TeamGender = "MEN")
Possible Years for Men: 2008 and above
Possible Years for Women: 2018 and above
Possible Team Genders: Men, Women

extractdata(matchIDs = c(521))
You can use match id data which can be found from iccmatchdata() and iplmatchdata() functions
