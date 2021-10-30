
#' Scrape match id data of IPL matches
#'
#' @param Year        IPL tournament year
#' @param TeamGender  gender of the team
#'
#' @return data frame with IPL match id data
#' @import
#' dplyr
#' rvest
#' xml2
#' rjson
#' plyr
#'
#' @export
#' @author Kusal De Silva and Rajitha M. Silva
#'
#' @examples
#' iplmatchdata()
#' iplmatchdata(2008:2012, 'men')
iplmatchdata = function(Year, TeamGender){

  tryCatch(
    {
      if(any(Year<2008 & tolower(TeamGender)=="men" | Year<2018 & tolower(TeamGender)=="women")){
        print("You entered year is not valid!! If you need to get both men and women match details,")
        print("You have to enter 2018 and above. For men only tournament you have to use 2008 or above")
      }else if(!all(is.element(tolower(TeamGender),c("men", "women")))){
        print("Invalid gender input!! Valid inputs are 'men', 'women'")
      }else{
        print('Scrapping data...')
        team = tolower(TeamGender)

        Matches = c()
        for (a in 1:length(team)) {
          url = paste0("https://www.iplt20.com/matches/results/",team[a],"/",Year)

          matchIDs = c()
          for (i in 1:length(url)) {
            readURL = read_html(url[i])%>%
              html_nodes(".result") %>%
              xml_attrs()

            for (j in 1:length(readURL)) {
              matchIDs = append(matchIDs, readURL[[j]][["data-match-id"]])
            }
          }

          matchLink = paste0("https://cricketapi.platform.iplt20.com//fixtures/",matchIDs)

          jsonViewer = function(i){
            fromJSON(paste(readLines(i), collapse=""))
          }

          #loop for read json data from above function
          read_Jdata = lapply(matchLink, jsonViewer)

          for (k in 1:length(read_Jdata)) {

            TournamentLabel = read_Jdata[[k]][["tournamentLabel"]]
            MatchLabel = read_Jdata[[k]][["scheduleEntry"]][["description"]]
            MatchId = read_Jdata[[k]][["scheduleEntry"]][["matchId"]][["id"]]
            Gender = toupper(team[a])
            MatchDate = read_Jdata[[k]][["scheduleEntry"]][["matchDate"]]
            Ground = read_Jdata[[k]][["scheduleEntry"]][["venue"]][["fullName"]]
            Venue = read_Jdata[[k]][["scheduleEntry"]][["venue"]][["city"]]
            MatchStatus = read_Jdata[[k]][["scheduleEntry"]][["matchStatus"]][["text"]]
            Team1 = read_Jdata[[k]][["scheduleEntry"]][["team1"]][["team"]][["fullName"]]
            Team1_Inn = read_Jdata[[k]][["scheduleEntry"]][["team1"]][["innings"]][[1]]
            Team2 = read_Jdata[[k]][["scheduleEntry"]][["team2"]][["team"]][["fullName"]]
            Team2_Inn = read_Jdata[[k]][["scheduleEntry"]][["team2"]][["innings"]][[1]]

            if((is.null(Team1_Inn) & is.null(Team2_Inn))){
              matchDF = data.frame(TournamentLabel, MatchLabel, MatchId, Gender, MatchDate,
                                   Ground, Venue, MatchStatus, Team1, Team2)

            }else if((is.null(Team1_Inn) & !is.null(Team2_Inn))){
              matchDF = data.frame(TournamentLabel, MatchLabel, MatchId, Gender, MatchDate,
                                   Ground, Venue, MatchStatus, Team1, Team2, Team2_Inn = c(Team2_Inn))

            }else if((!is.null(Team1_Inn) & is.null(Team2_Inn))){
              matchDF = data.frame(TournamentLabel, MatchLabel, MatchId, Gender, MatchDate,
                                   Ground, Venue, MatchStatus, Team1, Team1_Inn = c(Team1_Inn), Team2

              )

            }else if((!is.null(Team1_Inn) & !is.null(Team2_Inn))){
              matchDF = data.frame(TournamentLabel, MatchLabel, MatchId, Gender, MatchDate, Ground, Venue,
                                   MatchStatus, Team1, Team1_Inn = c(Team1_Inn), Team2, Team2_Inn = c(Team2_Inn)
              )

            }else{
              print(paste0("Error",i," ",j))
            }

            Matches = rbind.fill(Matches, matchDF)
          }
        }

        return(Matches)
      }

    },
    error=function(cond) {
      message("Error: ")
      message(cond)
    }
  )


}

