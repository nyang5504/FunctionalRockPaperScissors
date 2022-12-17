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
    },
    {
      "name": "Biased Random 1",
      "type": "BiasedRandomMovePlayer",
      "weights": {
        "rock": 0.5,
        "paper": 0.25,
        "scissors": 0.25
      }
    },
    {
      "name": "Biased Random 2",
      "type": "BiasedRandomMovePlayer",
      "weights": {
        "rock": 0.8,
        "paper": 0.1,
        "scissors": 0.1
      }
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
  case class SeasonDTO(tournaments: Int, rounds: Int, players: List[Map[String, Any]])
  case class DTO1(tournaments: Int, rounds: Int, players: List[String])

  def unpackWithForComprehension(json: JSON): Either[ParseError, DTO1] =
    json match {
      case jObject: JObject =>
        for {
          tournaments <- unpackNumber(jObject, "tournaments")
          rounds <- unpackNumber(jObject, "roundsPerMatch")
          players <- unpackArray(jObject, "players")
        } yield DTO1(tournaments.toInt, rounds.toInt, players)
      case _ => Left(ParseError(List((Location("Could not unpack JSON contents"),"Could not unpack JSON contents"))))
    }

  def unpack2(json: JSON): Either[ParseError, DTO1] =
    json match {
      case jObject: JObject =>
        for {
          name <- unpackString(jObject, "name")
          playerType <- unpackNumber(jObject, "type")
          players <- unpackArray(jObject, "players")
        } yield
      case _ => Left(ParseError(List((Location("Could not unpack JSON contents"),"Could not unpack JSON contents"))))
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

  def unpackList(c: List[JSON], r: Either[ParseError,List[String]]): Either[ParseError,List[String]] =
    c match {
      case ::(head, next) => head match {
        case JString(v) => unpackList(next, r.flatMap(list => Right(v :: list)))
        case p: ParseError => Left(p)
      }
      case Nil => r
    }

  def unpackArray(jObject: JObject, key: String): Either[ParseError, List[String]] = {
    for {
      relatedPacked <- jObject.get(key) match {
        case jArray: JArray => Right(jArray.get)
        case _ => Left(ParseError(List((Location("Could not unpack related"), "related"))))
      }
      related <- unpackList(relatedPacked.toList, Right(List.empty))
    } yield related
  }

}
