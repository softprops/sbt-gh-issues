package gh

private [gh] trait IssuesApi {
  import dispatch._
  import dispatch.Http._
  import dispatch.liftjson.Js._
  import net.liftweb.json._
  import net.liftweb.json.JsonAST._
  import java.net.URLEncoder

  type One[A] = JValue => Option[A]
  type Many[A] = JValue => List[A]

  val ghRepo: String

  val ghUser: String

  val auth: (String, String)

  private [gh] val github = :/("github.com") / "api" / "v2" / "json" / "issues"

  private [gh] def http = new Http with NoLogging

  def issue[A, B](num: Long)(f: Option[A] => B)(implicit one: One[A]) =
    try {
      http(github / "show" / ghUser / ghRepo / num.toString ># { js =>
        f(one(js))
      })
    } catch { case StatusCode(c, _) => f(None) }

  def issues[A, B](f: List[A] => B)(implicit many: Many[A]) =
    try {
      http(github / "list" / ghUser / ghRepo / "open" ># { js =>
        f(many(js))
      })
    } catch { case StatusCode(c, _) => f(Nil) }

  def closedIssues[A, B](f: List[A] => B)(implicit many: Many[A]) =
    try {
      http(github / "list" / ghUser / ghRepo / "closed" ># { js =>
        f(many(js))
      })
    } catch { case StatusCode(c, _) => f(Nil) }

  def searchOpen[A, B](term: String)(f: List[A] => B)(implicit many: Many[A]) =
    try {
      http(github / "search" / ghUser / ghRepo / "open" / URLEncoder.encode(term, "utf-8") ># { js =>
        f(many(js))
      })
    } catch { case StatusCode(c, _) => f(Nil) }

  def searchClosed[A, B](term: String)(f: List[A] => B)(implicit many: Many[A]) =
    try {
      http(github / "search" / ghUser / ghRepo / "closed" / URLEncoder.encode(term, "utf-8") ># { js =>
        f(many(js))
      })
    } catch { case StatusCode(c, _) => f(Nil) }

  def openIssue[A, B](title: String, body: String)(f: Option[A] => B)(implicit one: One[A]) = {
    val (user, pass) = auth
    try {
      http(github.POST.as_!(user, pass) / "open" / ghUser / ghRepo << Map(
        "title" -> title, "body" -> body
      ) ># { js =>
        f(one(js))
      })
    } catch { case StatusCode(c, _) => f(None) }
  }

  def editIssue[A, B](num: Long, title: String, body: String)(f: Option[A] => B)(implicit one: One[A]) = {
    val (user, pass) = auth
    try {
      http(github.POST.as_!(user, pass) / "edit" / ghUser / ghRepo / num.toString << Map(
        "title" -> title, "body" -> body
      ) ># { js =>
        f(one(js))
      })
    } catch { case StatusCode(c, _) => f(None) }
  }

  def closeIssue[A, B](num: Long)(f: Option[A] => B)(implicit one: One[A]) = {
    val (user, pass) = auth
    try {
      http(github.POST.as_!(user, pass) / "close" / ghUser / ghRepo / num.toString ># { js =>
        f(one(js))
      })
    } catch { case StatusCode(c, _) => f(None) }
  }

  def labels[A, B](f: List[A] => B)(implicit many: Many[A]) =
    try {
      http(github / "labels" / ghUser / ghRepo ># { js =>
        f(many(js))
      })
    } catch { case StatusCode(c, _) => f(Nil) }

  def addLabel[A, B](label: String, num: Long)(f: List[A] => B)(implicit many: Many[A]) = {
    val (user, pass) = auth
    try {
      http(github.POST.as_!(user, pass) / "label" / "add" / ghUser / ghRepo / label / num.toString ># { js =>
        f(many(js))
      })
    } catch { case StatusCode(c, _) => f(Nil) }
  }

  def removeLabel[A, B](label: String, num: Long)(f: List[A] => B)(implicit many: Many[A]) = {
    val (user, pass) = auth
    try {
      http(github.POST.as_!(user, pass) / "label" / "remove" / ghUser / ghRepo / label / num.toString ># { js =>
        f(many(js))
      })
    } catch { case StatusCode(c, _) => f(Nil) }
  }

  def comments[A, B](num: Long)(f: List[A] => B)(implicit many: Many[A]) =
    try {
      http(github  / "comments" / ghUser / ghRepo / num.toString ># { js =>
        f(many(js))
      })
    } catch { case StatusCode(c, _) => f(Nil) }

  def comment[A, B](id: Long, comment: String)(f: Option[A] => B)(implicit one: One[A]) = {
    val (user, pass) = auth
    try {
      http(github.POST.as_!(user, pass) / "comment" / ghUser / ghRepo / id.toString << Map(
        "comment" -> comment
      ) ># { js =>
        f(one(js))
      })
    } catch { case StatusCode(c, _) => f(None) }
  }
}
