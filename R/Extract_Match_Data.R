#' Scrape ball by ball data
#'
#' @param matchIDs insert list of match id
#'
#' @return Ball by ball data frame
#' @import
#' rjson
#' dplyr
#' tidyr
#' @export
#' @author Kusal De Silva and Rajitha M. Silva
#'
#' @examples
#' extractdata(521)
#' extractdata(c(521,533))

extractdata = function(matchIDs){

  tryCatch(
    {
      print('Scrapping data...')
      matchData = paste0("https://cricketapi-icc.pulselive.com/fixtures/",matchIDs,"/uds/stats")
      plyrDetails = paste0("https://cricketapi-icc.pulselive.com/fixtures/",matchIDs,"/uds")

      #function to read all links of json file and collapse it to readable way
      jsonViewer = function(i){
        fromJSON(paste(readLines(i), collapse=""))
      }

      read_match = lapply(matchData, jsonViewer)
      read_Plyrs = lapply(plyrDetails, jsonViewer)

      MatchDF = data.frame()
      for (m in 1:length(read_match)) {
        ballby = as.data.frame(unlist(read_match[[m]][["data"]]))

        colNames = c("Ball", "Striker_ID", "NonStriker_ID","Bowler_ID","BallSpeed_ms", "OutBat_ID", "DismissalDetails","Runs",
                     "BatterRuns","BowlerRuns", "Extras","Unknown","Pitch_X","Pitch_Y","Stumps_X","stumps_Y","Field_X","Field_Y")
        ballbyData = ballby %>%
          separate(`unlist(read_match[[m]][["data"]])`, colNames, sep = "[,]")

        plyrs = data.frame()

        for (i in 1:length(read_Plyrs[[m]][["teams"]])) {
          for (j in 1:length(read_Plyrs[[m]][["teams"]][[i]][["players"]])) {

            plyrData = do.call(rbind, read_Plyrs[[m]][["teams"]][[i]][["players"]])
            plyrData = as.data.frame(plyrData)
          }
          plyrs = rbind(plyrs, plyrData)
        }

        plyrs$id = as.character(plyrs$id)
        for (k in 1:2) {
          names(plyrs)[1] = colNames[k+1]
          names(plyrs)[2] = paste0(colNames[k+1],"Name")
          names(plyrs)[7] = paste0(colNames[k+1],"RightHanded")
          ballbyData = left_join(ballbyData, plyrs[,c(1,2,7)])
        }

        names(plyrs)[1] = colNames[4]
        names(plyrs)[2] = paste0(colNames[4],"Name")
        names(plyrs)[6] = paste0(colNames[4],"RightArmed")
        ballbyData = left_join(ballbyData, plyrs[,c(1,2,6,8)])


        Inning.Over.BallNo = c(row.names(ballby))
        ballbyData = cbind(Inning.Over.BallNo, ballbyData)

        ballbyData$MatchID = c(rep(matchIDs[m], time = nrow(ballbyData)))

        ballbyData = ballbyData[,c(27,1,2,3,20,21,4,22,23,5,24,25,26,6:12,14:19)]
        ballbyData[ballbyData == -1] = ''

        MatchDF = rbind(MatchDF, ballbyData)
      }

      return(MatchDF)
    },
    error=function(cond) {
      message("Error: ")
      message(cond)
    })

}
