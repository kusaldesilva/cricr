
#' Scrape match id data from ICC website
#'
#' @param Format of the match
#' @param Gender of the team
#'
#' @return data frame with ICC match id data
#' @import
#' rjson
#' dplyr
#' plyr
#'
#' @export
#'
#' @examples
#' iccmatchdata('ODI', 'Men')
#' iccmatchdata('ODI', 'Women')
#'

iccmatchdata <- function(MatchFormat,TeamGender){

  tryCatch(
    {
      if(!all(is.element(toupper(MatchFormat),c("ODI","TEST","T20I","T20","List_A","FIRST_CLASS","OTHER")))){
        print("Match format you entered is not valid!! You have to enter ODI, TEST, T20I, T20, FIRST_CLASS, LIST_A or OTHER")
      }else if(!all(is.element(toupper(TeamGender),c("MEN", "WOMEN")))){
        print("Invalid gender input!! Valid inputs are 'men', 'women'")
      }else{

        teamType = ifelse(toupper(TeamGender) == "MEN", "m","w")
        matchType = toupper(MatchFormat)

        #Make a variable to save match data
        Matches = c()
        print('Scrapping data...')
        for (b in 1:length(teamType)) {
          urlMatch = paste0("https://cricketapi-icc.pulselive.com/fixtures?matchTypes=",matchType,"&tournamentTypes=I%2CWI&teamTypes=",teamType[b],"&matchStates=C&page=1&pageSize=20&sort=desc")

          for (a in 1:length(urlMatch)) {
            urlJdata = fromJSON(paste(readLines(urlMatch[a]), collapse=""))

            pages = 1:urlJdata[["pageInfo"]][["numPages"]]
            matchData = paste0("https://cricketapi-icc.pulselive.com/fixtures?matchTypes=",matchType[a],"&tournamentTypes=I%2CWI&teamTypes=",teamType[b],"&matchStates=C&page=",pages,"&pageSize=20&sort=desc")

            #function to read all links of json file and collapse it to readable way
            jsonViewer = function(i){
              fromJSON(paste(readLines(i), collapse=""))
            }

            #loop for read json data from above function
            read_Jdata = lapply(matchData, jsonViewer)

            #for loop to get tournamentLabel in json_data
            for (i in 1:(length(read_Jdata)-1)) {

              items = length(read_Jdata[[i]][["content"]])
              for (j in 1:items) {

                Team1_Inn1 = tryCatch(
                  {
                    read_Jdata[[i]][["content"]][[j]][["scheduleEntry"]][["team1"]][["innings"]][[1]]
                  },
                  error=function(cond) {
                    NULL
                  })

                Team1_Inn2 = tryCatch(
                  {
                    read_Jdata[[i]][["content"]][[j]][["scheduleEntry"]][["team1"]][["innings"]][[2]]
                  },
                  error=function(cond) {
                    NULL
                  })

                Team2_Inn1 = tryCatch(
                  {
                    read_Jdata[[i]][["content"]][[j]][["scheduleEntry"]][["team2"]][["innings"]][[1]]
                  },
                  error=function(cond) {
                    NULL
                  })

                Team2_Inn2 = tryCatch(
                  {
                    read_Jdata[[i]][["content"]][[j]][["scheduleEntry"]][["team2"]][["innings"]][[2]]
                  },
                  error=function(cond) {
                    NULL
                  })

                TournamentLabel = read_Jdata[[i]][["content"]][[j]][["tournamentLabel"]]
                TournamentId = read_Jdata[[i]][["content"]][[j]][["tournamentId"]][["id"]]
                MatchLabel = read_Jdata[[i]][["content"]][[j]][["label"]]
                MatchId = read_Jdata[[i]][["content"]][[j]][["scheduleEntry"]][["matchId"]][["id"]]
                MatchType = read_Jdata[[i]][["content"]][[j]][["scheduleEntry"]][["matchType"]]
                Gender = toupper(TeamGender[b])
                MatchDate = read_Jdata[[i]][["content"]][[j]][["scheduleEntry"]][["matchDate"]]
                Ground = read_Jdata[[i]][["content"]][[j]][["scheduleEntry"]][["venue"]][["fullName"]]
                Venue = read_Jdata[[i]][["content"]][[j]][["scheduleEntry"]][["venue"]][["country"]]
                MatchStatus = ifelse(is.null(read_Jdata[[i]][["content"]][[j]][["scheduleEntry"]][["matchStatus"]][["text"]]), "NA",
                                     read_Jdata[[i]][["content"]][[j]][["scheduleEntry"]][["matchStatus"]][["text"]])
                Team1 = read_Jdata[[i]][["content"]][[j]][["scheduleEntry"]][["team1"]][["team"]][["fullName"]]
                Team2 = read_Jdata[[i]][["content"]][[j]][["scheduleEntry"]][["team2"]][["team"]][["fullName"]]


                if((is.null(Team1_Inn1) & is.null(Team1_Inn2) & is.null(Team2_Inn1) & is.null(Team2_Inn2))){
                  matchDF = data.frame(TournamentLabel, TournamentId, MatchLabel, MatchId, MatchType, Gender, MatchDate,
                                       Ground, Venue, MatchStatus, Team1, Team2)

                }else if((is.null(Team1_Inn1) & is.null(Team1_Inn2) & !is.null(Team2_Inn1) & is.null(Team2_Inn2))){
                  matchDF = data.frame(TournamentLabel, TournamentId, MatchLabel, MatchId, MatchType, Gender, MatchDate,
                                       Ground, Venue, MatchStatus, Team1, Team2, Team2_Inn1 = c(Team2_Inn1))

                }else if((!is.null(Team1_Inn1) & is.null(Team1_Inn2) & is.null(Team2_Inn1) & is.null(Team2_Inn2))){
                  matchDF = data.frame(TournamentLabel, TournamentId, MatchLabel, MatchId, MatchType, MatchDate,
                                       Ground, Venue, MatchStatus, Team1, Team2, Team1_Inn1 = c(Team1_Inn1)

                  )

                }else if((!is.null(Team1_Inn1) & !is.null(Team1_Inn2) & !is.null(Team2_Inn1) & !is.null(Team2_Inn2))){
                  matchDF = data.frame(TournamentLabel, TournamentId, MatchLabel, MatchId, MatchType, Gender, MatchDate,
                                       Ground, Venue, MatchStatus, Team1, Team2, Team1_Inn1 = c(Team1_Inn1),
                                       Team1_Inn2 = c(Team1_Inn2), Team2_Inn1 = c(Team2_Inn1), Team2_Inn2 = c(Team2_Inn2)

                  )

                }else if((!is.null(Team1_Inn1) & is.null(Team1_Inn2) & !is.null(Team2_Inn1) & !is.null(Team2_Inn2))){
                  matchDF = data.frame(TournamentLabel, TournamentId, MatchLabel, MatchId, MatchType, Gender, MatchDate,
                                       Ground, Venue, MatchStatus, Team1, Team2, Team1_Inn1 = c(Team1_Inn1),
                                       Team2_Inn1 = c(Team2_Inn1), Team2_Inn2 = c(Team2_Inn2)

                  )

                }else if((!is.null(Team1_Inn1) & !is.null(Team1_Inn2) & !is.null(Team2_Inn1) & is.null(Team2_Inn2))){
                  matchDF = data.frame(TournamentLabel, TournamentId, MatchLabel, MatchId, MatchType, Gender, MatchDate,
                                       Ground, Venue, MatchStatus, Team1, Team2, Team1_Inn1 = c(Team1_Inn1), Team1_Inn2 = c(Team1_Inn2),
                                       Team2_Inn1 = c(Team2_Inn1)

                  )

                }else if((!is.null(Team1_Inn1) & is.null(Team1_Inn2) & !is.null(Team2_Inn1) & is.null(Team2_Inn2))){
                  matchDF = data.frame(TournamentLabel, TournamentId, MatchLabel, MatchId, MatchType, Gender, MatchDate,
                                       Ground, Venue, MatchStatus, Team1, Team2, Team1_Inn1 = c(Team1_Inn1),
                                       Team2_Inn1 = c(Team2_Inn1)

                  )

                }else{
                  print(paste0("Error",i," ",j))
                }


                Matches = rbind.fill(Matches, matchDF)
              }
            }
          }
        }
        return(Matches)
      }

    },
    error=function(cond) {
      message("Error: ")
      message(cond)
    })

}

