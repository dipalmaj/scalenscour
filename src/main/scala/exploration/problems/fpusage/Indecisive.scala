package exploration.problems.fpusage


import scala.annotation.tailrec

case class SurveyPopularity(mostPopular: String, count: Int, customerSuppliedAnswers: Seq[String])

case class UnknownResult(value: String)

case class KnownResult(value: String, cnt: Int)


object ABSCompanySurvey {

  // Either has
  type UnknownOrKnownResult = Either[UnknownResult, KnownResult]

  val suppliedChoices = Seq("monkey","dog","cat")

  def createLabeledResult(responses: Seq[String]): SurveyPopularity = ???

  // Handle Using Folds
  val x: UnknownOrKnownResult = ???

  // Input is Seq of Aggregated Lines
  val input: Seq[String] = Seq(
    "monkey,3", "dog,2", "turkey,5", "cat,12", "goose,5", "aardvark,99"
  )

  def main(args: Array[String]): Unit = {
    val result = createLabeledResult(input)
    println(result)
    assert(result == SurveyPopularity("cat",12,Seq("turkey","goose","aardvark")))
  }

}


