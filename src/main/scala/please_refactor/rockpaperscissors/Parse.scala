package please_refactor.rockpaperscissors
import fpinscala.parsing.JSON.{JArray, JBool, JNumber, JObject, JString}
import fpinscala.parsing.{JSON, Location, ParseError}

object Parse {

  val jsonTxt =
    """
{
  "tournaments": 1000,
  "roundsPerMatch": 1000,
  "randomSeed": 12345,
  "players": [
    {
      "name": "Last Losing 1",
      "type": "LastLosingMovePlayer"
    },
    {
      "name": "Last Losing 2",
      "type": "LastLosingMovePlayer",
    },
    {
      "name": "Last Winning 1",
      "type": "LastWinningMovePlayer",
    },
    {
      "name": "Majority Losing 1",
      "type": "MajorityLosingMovePlayer"
    },
    {
      "name": "Majority Winning 1",
      "type": "MajorityWinningMovePlayer"
    },
    {
      "name": "Unbiased Random 1",
      "type": "RandomMovePlayer"
    }
  ]
}
"""

  def go = {
    val P = fpinscala.parsing.Reference
    import fpinscala.parsing.ReferenceTypes.Parser
    val json: Parser[JSON] = JSON.jsonParser(P)
    val resultOfParsing = P.run(json)(jsonTxt) // this parses JSON input into a JSON object
    resultOfParsing.flatMap(j => unpackWithForComprehension(j)).map(dto => println(dto)).map(_ => ())

  }
  case class TemporaryDTO(tournaments: Int, rounds: Int, players: List[Map[String, String]])
  case class SeasonDTO(tournaments: Int, rounds: Int, players: List[Object])

  def unpackWithForComprehension(json: JSON): Either[ParseError, SeasonDTO] =
    json match {
      case jObject: JObject =>
        for {
          tournaments <- unpackNumber(jObject, "tournaments")
          rounds <- unpackNumber(jObject, "roundsPerMatch")
          players <- unpackArray(jObject, "players")
        } yield SeasonDTO(tournaments.toInt, rounds.toInt, players)
      case _ => Left(ParseError(List((Location("Could not unpack JSON contents"),"Could not unpack JSON contents"))))
    }

//  def unpackPlayers(json: JSON): Either[ParseError, Map[String,String]] =
//    json match {
//      case jObject: JObject =>
//        for {
//          name <- unpackString(jObject, "name")
//          playerType <- unpackString(jObject, "type")
//        } yield Map("name" -> name, "type" -> playerType)
//      case _ => Left(ParseError(List((Location("Could not unpack JSON contents"),"Could not unpack JSON contents"))))
//    }


//  def unpackObject(tourney: Int, rounds: Int, players: List[Object]): SeasonDTO = {
//    for(obj <- players){
//      case jObject: JObject =>
//        for {
//          name <- unpackString(jObject, "name")
//          playertype <- unpackString(jObject, "type")
//        } yield SeasonDTO(tourney, rounds, )
//    }
//  }

  def unpack(json: JSON): Either[ParseError,SeasonDTO] = {
    val res = json match {
      case jObject: JObject =>
        for {
          tournaments <- jObject.get("tournaments") match {
            case jNumber: JNumber => Right(jNumber.get)
            case _ => Left(ParseError(List((Location("Could not unpack price"),"price"))))
          }
          rounds <- jObject.get("roundsPerMatch") match {
            case jNumber: JNumber => Right(jNumber.get)
            case _ => Left(ParseError(List((Location("Could not unpack shares"),"shares"))))
          }
          players <- jObject.get("players") match {
            case jArray: JArray => Right(jArray.get)
            case _ => Left(ParseError(List((Location("Could not unpack related"),"related"))))
          }
          related <- unpackList(players.toList, Right(List.empty))
        } yield SeasonDTO(tournaments.toInt,rounds.toInt,related)
      case _ => Left(ParseError(List((Location("Could not unpack JSON contents"),"Could not unpack JSON contents"))))
    }
    res
  }

  def unpackString(jObject: JObject, key: String): Either[ParseError,String] = jObject.get(key) match {
    case jString: JString => Right(jString.get)
    case _ => Left(ParseError(List((Location("Could not unpack ticker"), "ticker"))))
  }

  def unpackBoolean(jObject: JObject, key: String): Either[ParseError, Boolean] = jObject.get(key) match {
    case jBool: JBool => Right(jBool.get)
    case _ => Left(ParseError(List((Location("Could not unpack ticker"), "ticker"))))
  }

  def unpackNumber(jObject: JObject, key: String): Either[ParseError, Double] = jObject.get(key) match {
    case jNumber: JNumber => Right(jNumber.get)
    case _ => Left(ParseError(List((Location("Could not unpack ticker"), "ticker"))))
  }

  def unpackList(c: List[JSON], r: Either[ParseError,List[Object]]): Either[ParseError,List[Object]] =
    c match {
      case ::(head, next) => head match {
        case JString(v) => unpackList(next, r.flatMap(list => Right(v :: list)))
        case p: ParseError => Left(p)
      }
      case Nil => r
    }

  def unpackArray(jObject: JObject, key: String): Either[ParseError, List[Object]] = {
    for {
      relatedPacked <- jObject.get(key) match {
        case jArray: JArray => Right(jArray.get)
        case _ => Left(ParseError(List((Location("Could not unpack related"), "related"))))
      }
      related <- unpackList(relatedPacked.toList, Right(List.empty))
    } yield related
  }

}
