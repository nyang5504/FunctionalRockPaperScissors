package please_refactor.rockpaperscissors

import benjamingarrett.rockpaperscissorstools.{AWins, BWins, RPSHistoryBasedPlayer, RPSOutcome, RPSPointsSchema, RPSTournament, RPSTournamentSeason}

object MixedTournamentSeason extends RPSTournamentSeason {
  def handleTournamentSeason(players: List[RPSHistoryBasedPlayer])(tournaments: List[RPSTournament]) =
    tournaments
      .map(event => event.playTournament(players))
      .map(tourneyResult => tourneyResult.flatMap(matchResult => getWinTotals(matchResult)))
      .map(playerWinCount => playerWinCount.toList.sortWith((firstPlayer, secondPlayer) => firstPlayer._2 > secondPlayer._2))
      .flatMap(ranking => applyPoints(ranking))
      .foldLeft(Map[RPSHistoryBasedPlayer, Int]())({
        case (ranking, pair) => updatePlayerRankings(pair._1, pair._2, ranking)
      })

  def handleTournamentSeasonForClassLecture(players: List[RPSHistoryBasedPlayer])(tournaments: List[RPSTournament]): Map[RPSHistoryBasedPlayer, Int] = {

    val tournamentResults: List[Map[(RPSHistoryBasedPlayer, RPSHistoryBasedPlayer), List[RPSOutcome]]] =
      tournaments
        .map((event: RPSTournament) => event.playTournament(players))

    val rosterWithWinTotals: List[Map[RPSHistoryBasedPlayer, Int]] =
      tournamentResults
        .map((tourneyResult: Map[(RPSHistoryBasedPlayer, RPSHistoryBasedPlayer), List[RPSOutcome]]) => tourneyResult
          .flatMap((matchResult: ((RPSHistoryBasedPlayer, RPSHistoryBasedPlayer), List[RPSOutcome])) => getWinTotals(matchResult)))

    val sortedRosterWithWinTotals: List[List[(RPSHistoryBasedPlayer, Int)]] =
      rosterWithWinTotals
        .map((playerWinCount: Map[RPSHistoryBasedPlayer, Int]) => playerWinCount.toList.sortWith((firstPlayer, secondPlayer) => firstPlayer._2 > secondPlayer._2))

    val rosterWithPointsPerTourney: List[(RPSHistoryBasedPlayer, Int)] =
      sortedRosterWithWinTotals
        .flatMap((ranking: List[(RPSHistoryBasedPlayer, Int)]) => applyPoints(ranking))

    val rosterWithOverallPoints: Map[RPSHistoryBasedPlayer, Int] =
      rosterWithPointsPerTourney
        .foldLeft(Map[RPSHistoryBasedPlayer, Int]())({
          case (ranking, pair) => updatePlayerRankings(pair._1, pair._2, ranking)
        })

    rosterWithOverallPoints
  }

  def handle1(allTourneyResults: List[Map[(RPSHistoryBasedPlayer, RPSHistoryBasedPlayer), List[RPSOutcome]]]) = {
    allTourneyResults
      .map(tourneyResult => tourneyResult.flatMap(matchResult => getWinTotals(matchResult)))
      .map(playerWinCount => playerWinCount.toList.sortWith((firstPlayer, secondPlayer) => firstPlayer._2 > secondPlayer._2))
      .flatMap(ranking => applyPoints(ranking))
      .foldLeft(Map[RPSHistoryBasedPlayer, Int]())({
        case (ranking, pair) => updatePlayerRankings(pair._1, pair._2, ranking)
      })
  }

  def handle2(allTourneyResults: List[Map[(RPSHistoryBasedPlayer, RPSHistoryBasedPlayer), List[RPSOutcome]]]) = {
    allTourneyResults
      .map(
        tourneyResult => tourneyResult.flatMap(matchResult => getWinTotals(matchResult))
          .toList.sortWith((firstPlayer, secondPlayer) => firstPlayer._2 > secondPlayer._2)
      )
      .flatMap(ranking => applyPoints(ranking))
      .foldLeft(Map[RPSHistoryBasedPlayer, Int]())({
        case (ranking, pair) => updatePlayerRankings(pair._1, pair._2, ranking)
      })
  }

  def handle3(allTourneyResults: List[Map[(RPSHistoryBasedPlayer, RPSHistoryBasedPlayer), List[RPSOutcome]]]) = {
    val rosterWithWinTotals: List[Map[RPSHistoryBasedPlayer, Int]] =
      allTourneyResults
        .map((tourneyResult: Map[(RPSHistoryBasedPlayer, RPSHistoryBasedPlayer), List[RPSOutcome]]) => tourneyResult
          .flatMap((matchResult: ((RPSHistoryBasedPlayer, RPSHistoryBasedPlayer), List[RPSOutcome])) => getWinTotals(matchResult)))

    val sortedRosterWithWinTotals: List[List[(RPSHistoryBasedPlayer, Int)]] =
      rosterWithWinTotals
        .map((playerWinCount: Map[RPSHistoryBasedPlayer, Int]) => playerWinCount.toList.sortWith((firstPlayer, secondPlayer) => firstPlayer._2 > secondPlayer._2))

    val rosterWithPointsPerTourney: List[(RPSHistoryBasedPlayer, Int)] =
      sortedRosterWithWinTotals
        .flatMap((ranking: List[(RPSHistoryBasedPlayer, Int)]) => applyPoints(ranking))

    val rosterWithOverallPoints: Map[RPSHistoryBasedPlayer, Int] =
      rosterWithPointsPerTourney
        .foldLeft(Map[RPSHistoryBasedPlayer, Int]())({
          case (ranking, pair) => updatePlayerRankings(pair._1, pair._2, ranking)
        })

    rosterWithOverallPoints
  }

  private def getWinTotals(matchResult: ((RPSHistoryBasedPlayer, RPSHistoryBasedPlayer), List[RPSOutcome])): Map[RPSHistoryBasedPlayer, Int] =
    Map(
      matchResult._1._1 -> matchResult._2.count(gameOutcome => gameOutcome == AWins),
      matchResult._1._2 -> matchResult._2.count(gameOutcome => gameOutcome == BWins)
    )

  private def applyPoints(ranking: List[(RPSHistoryBasedPlayer, Int)]): List[(RPSHistoryBasedPlayer, Int)] = {
    def points(level: Int, results: List[(RPSHistoryBasedPlayer, Int)]): List[(RPSHistoryBasedPlayer, Int)] =
      if (level > 4) List.empty
      else results.headOption match {
        case Some(tuple) => (tuple._1, RPSPointsSchema.pointsAwarded(level)) :: points(level + 1, results.tail)
        case None => List.empty
      }
    points(1, ranking)
  }
  private def updatePlayerRankings(player: RPSHistoryBasedPlayer, newPoints: Int, ranking: Map[RPSHistoryBasedPlayer,Int]) = {
    ranking.get(player) match {
      case Some(accumulatedPoints) => ranking.updated(player, accumulatedPoints + newPoints)
      case None => ranking.updated(player, newPoints)
    }
  }
  // was using this for debugging
  private def getRawPoints(tournamentResults: List[Map[(RPSHistoryBasedPlayer, RPSHistoryBasedPlayer), List[RPSOutcome]]]): List[(RPSHistoryBasedPlayer, Int)] =
    tournamentResults.map(
      result => result.flatMap(
        r => Map(
          r._1._1 -> r._2.count(outcome => outcome == AWins),
          r._1._2 -> r._2.count(outcome => outcome == BWins)
        )
      )
    ).map(x => x.toList.sortWith((x, y) => x._2 > y._2)).flatten
}
