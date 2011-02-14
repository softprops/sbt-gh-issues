package gh

import java.util.Date

object GhDate {
  import dispatch.liftjson.Js._
  val time = datestr("yyyy/MM/dd kk:mm:ss Z")
}
import GhDate.time

object Issues {
  import dispatch.liftjson.Js._
  import net.liftweb.json.JsonAST._

  def many(js: JValue) = for(JArray(issue) <- js \ "issues") yield issue
  def one(js: JValue) = for (JField("issue", field) <- js) yield field

  val user = 'user ? str
  val gravatar = 'gravatar_id ? str
  val updatedAt = 'updated_at ? GhDate.time
  val votes = 'votes ? int
  val number = 'number ? int
  val position = 'position ? double
  val title = 'title ? str
  val body = 'body ? str
  val state = 'state ? str
  val createdAt = 'created_at ? time
  val closedAt = 'closed_at ? time
  val labels = 'labels ? ary
}

object Labels {
  import dispatch.liftjson.Js._
  import net.liftweb.json.JsonAST._

  def many(js: JValue) = for(JArray(value) <- js \ "labels"; JString(label) <- value) yield label
}

object Comments {
  import dispatch.liftjson.Js._
  import net.liftweb.json.JsonAST._

  def many(js: JValue) = for(JArray(comment) <- js \ "comments") yield comment
  def one(js: JValue) = for (JField("comment", field) <- js) yield field

  val gravatar = 'gravatar_id ? str
  val createdAt = 'created_at ? time
  val body = 'body ? str
  val updatedAt = 'updated_at ? time
  val id = 'id ? int
  val user = 'user ? str
}

case class Issue(user: String, gravatar: String, updatedAt: Date, votes: BigInt, number: BigInt,
               position: Double, title: String, body: String, state: String,
               createdAt: Date, labels: List[String])

case class Comment(id: BigInt, user: String, gravatar: String, body: String, createdAt: Date, updatedAt: Date)

trait MethodTasks extends sbt.Project {
  /** clean up some brackets with implicit convertions from Option[String] to task */
  implicit def os2t(o: Option[String]) = task { o }
}

trait LabelTasks extends sbt.Project with ColorizedLogging with MethodTasks {
  self: IssuesProvider with GhInfo  =>

  import net.liftweb.json.JsonAST._

  implicit def manyLabels(js: JValue) =
    for(l <- Labels.many(js)) yield l

  lazy val ghLabels = task {
    issues.labels {
      (_: List[String]).foreach(labelListing)
    }
    None
  } describedAs("Lists Labels associated with Github repo")

  lazy val ghAddLabel = task { _ match {
    case Array(label, num) => try {
      val (user, repo) = ghRepository
      issues.addLabel(label, num.toLong) { labels: List[String] =>
        println("""Added label "%s" to Issue %s of %s/%s""" format(label, num, user, repo))
      }
      None
    } catch { case _ => Some("invalid arguments label: %s, num: %s" format(label, num)) }
    case _ => Some("""usage: gh-add-label "<label>" <num>""")
  } } describedAs("Adds a Label to a Github Issue")

  lazy val ghRemoveLabel = task { _ match {
    case Array(label, num) => try {
      val (user, repo) = ghRepository
      issues.removeLabel(label, num.toLong) { labels: List[String] =>
        println("""Removed label "%s" from Issue %s of %s/%s""" format(label, num, user, repo))
      }
      None
    } catch { case _ => Some("invalid arguments label: %s, num:%s" format(label, num)) }
    case _ => Some("usage: gh-remove-label <label> <num>")
  } } describedAs("Removes a Label from a gh issue")

}

trait IssueTasks extends sbt.Project with ColorizedLogging with MethodTasks {
  self: IssuesProvider with GhInfo  =>

  import net.liftweb.json.JsonAST._

  implicit def one(js: JValue) =
    try {
      (for {
        f        <- Issues.one(js)
        grav     <- Issues.gravatar(f)
        position <- Issues.position(f)
        votes    <- Issues.votes(f)
        created  <- Issues.createdAt(f)
        body     <- Issues.body(f)
        title    <- Issues.title(f)
        updated  <- Issues.updatedAt(f)
        state    <- Issues.state(f)
        user     <- Issues.user(f)
        number   <- Issues.number(f)
        labels   <- Some((for(JString(label) <- Issues.labels(f)) yield label): List[String])
      } yield {
        Issue(user, grav, updated, votes, number, position, title, body, state, created, labels)
      }).headOption
    } catch { case dispatch.StatusCode(c, _) => None }

  implicit def many(js: JValue) =
    try {
      for {
        is       <- Issues.many(js)
        f        <- is
        grav     <- Issues.gravatar(f)
        position <- Issues.position(f)
        votes    <- Issues.votes(f)
        created  <- Issues.createdAt(f)
        body     <- Issues.body(f)
        title    <- Issues.title(f)
        updated  <- Issues.updatedAt(f)
        state    <- Issues.state(f)
        user     <- Issues.user(f)
        number   <- Issues.number(f)
        labels   <- Some((for(JString(label) <- Issues.labels(f)) yield label): List[String])
      } yield {
        Issue(user, grav, updated, votes, number, position, title, body, state, created, labels)
      } } catch { case dispatch.StatusCode(c,_) => Nil }

  lazy val ghIssue = task { _ match {
      case Array(num) => issues.issue(num.toLong) { (_: Option[Issue]) match {
        case Some(is) => task {
          issueDetail(is)
          None
        }
        case _ => task { Some("This project has no issue %s" format num) }
      } }
      case _ => task { Some("usage: gh-issue <num>") }
    }
  } describedAs("Shows a Github Issue by number")

  lazy val ghIssues = task {
    issues.issues { (_: List[Issue]) match {
      case Nil => println("This project has no issues (at least no documented issues)")
      case l =>
        val (user, repo) = ghRepository
        println("Open issues for %s/%s" format(user, repo))
        for(is <- l) issueListing(is)
    } }
    None
  } describedAs("Lists open github issues")

  lazy val ghClosedIssues = task {
    issues.closedIssues { (_: List[Issue]) match {
      case Nil => println("This project has no closed issues.")
      case l =>
        val (user, repo) = ghRepository
        println("Closed issues for %s/%s" format(user, repo))
        for(is <- l) issueListing(is)
    } }
    None
  } describedAs("Lists closed Github Issues")

  lazy val ghSearchOpenIssues = task { _ match {
    case Array() => Some("usage: gh-search-open-issues 'terms to search for'")
    case terms =>
      val (user, repo) = ghRepository
      issues.searchOpen(terms.mkString(" ")) { (_: List[Issue]) match {
        case Nil => println("no open issues with the terms %s for %s/%s" format(terms.mkString(" "), user, repo))
        case l =>
          println("%s search results for %s/%s" format(l.size, user, repo))
          for(is <- l) issueListing(is)
      } }
      None
  } } describedAs("Search for open Github Issues by terms")

  lazy val ghSearchClosedIssues = task { _ match {
    case Array() => task { Some("usage: gh-search-closed-issues 'terms to search for'") }
    case terms =>
      val (user, repo) = ghRepository
      issues.searchClosed(terms.mkString(" ")) { (_: List[Issue]) match {
        case Nil => println("no closed issues with the terms %s for %s/%s" format(terms.mkString(" "), user, repo))
        case l =>
          println("%s search results" format l.size)
          for(is <- l) issueListing(is)
      } }
      None
  } } describedAs("Search for closed Github Issues by terms")

  lazy val ghOpen = task { _ match {
    case Array(title, desc) =>
      issues.openIssue(title, desc) { (_: Option[Issue]) match {
        case Some(is) =>
          val (user, repo) = ghRepository
          println("""Opened issue %s "%s" as @%s for %s/%s""" format(is.number, is.title, is.user, user, repo))
          None
        case _ => Some("error creating issue")
      } }
    case _ => Some("""usage: gh-open "<title>" "<description>" """)
  } } describedAs("Opens a new Github Issue")

  lazy val ghEdit = task { _ match {
    case Array(num, title, desc) =>
      try {
        issues.editIssue(num.toLong, title, desc) { (_: Option[Issue]) match {
          case Some(is) =>
            val (user, repo) = ghRepository
            println("""Edited issue %s "%s" as @%s for %s/%s""" format(is.number, is.title, is.user, user, repo))
            None
          case _ => Some("Error editing issue")
        } }
      } catch { case _ => Some("Invalid arguments num: %s, title: %s, desc: %s" format(num, title, desc)) }
    case _ => Some("""usage: gh-edit <num> "<title>" "<description>" """)
  } } describedAs("Edits a Github Issue")

  lazy val ghClose = task { _ match {
    case Array(num) =>
      issues.closeIssue(num.toLong) { (_: Option[Issue]) match {
        case Some(is) =>
          val (user, repo) = ghRepository
          println("""Closed issue %s "%s" as @%s for %s/%s""" format(is.number, is.title, is.user, user, repo))
          None
        case _ => Some("error creating issue")
      } }
    case _ => Some("usage: gh-close <num>")
  } } describedAs("Closes a Github Issue")
}

trait CommentTasks extends sbt.Project with ColorizedLogging with MethodTasks {
  self: IssuesProvider with GhInfo =>
  import net.liftweb.json.JsonAST._

  implicit def oneComment(js: JValue) =
    try {
      (for {
        f        <- Comments.one(js)
        id        <- Comments.id(f)
        user      <- Comments.user(f)
        gravatar  <- Comments.gravatar(f)
        body      <- Comments.body(f)
        createdAt <- Comments.createdAt(f)
        updatedAt <- Comments.updatedAt(f)
      } yield {
        Comment(id, user, gravatar, body, createdAt, updatedAt)
      }).headOption
    } catch { case dispatch.StatusCode(c, _) => None }

  implicit def manyComments(js: JValue) =
   try {
     for {
       c         <- Comments.many(js)
       f         <- c
       id        <- Comments.id(f)
       user      <- Comments.user(f)
       gravatar  <- Comments.gravatar(f)
       body      <- Comments.body(f)
       createdAt <- Comments.createdAt(f)
       updatedAt <- Comments.updatedAt(f)
     } yield {
       Comment(id, user, gravatar, body, createdAt, updatedAt)
     }
   } catch { case dispatch.StatusCode(c, _) => Nil }

  lazy val ghComments = task { _ match {
    case Array() => task { Some("usage: gh-comments <num>") }
    case Array(num) => try {
        val (user, repo) = ghRepository
        println("Comments on issue %s for %s/%s" format(num, user, repo))
        issues.comments(num.toLong) { (_: List[Comment]) match {
          case Nil => println("There were no comments on this issue")
          case l => for (c <-l) commentListing(c)
        } }
        None
      } catch { case _ => Some("Invalid arguments num: %s" format num) }
  } } describedAs("Lists Comments on a Github Issue")

  lazy val ghComment = task { _ match {
    case Array() => task { Some("""usage: gh-comment <num> ""<comment>"" """) }
    case Array(num, comm) => try {
        val (user, repo) = ghRepository
        issues.comment(num.toLong, comm) { (_: Option[Comment]) match {
          case Some(c) => println("""Posted comment "%s" on issue %s as @%s for %s/%s""" format(
            c.body, num, c.user, user, repo)
          )
          case _ => println("Comment not posted")
        } }
        None
      } catch { case _ => Some("Invalid arguments num: %s, comment: %s" format(num, comm)) }
  } } describedAs("Posts a new Comment on a Github Issue")

}

/** Mixin for github.com issue tracking and labeling */
trait Issues extends IssueTasks
     with LabelTasks
     with CommentTasks
     with DefaultIssuesProvider
     with GhInfo

trait IssuesProvider {
  self: GhInfo =>

  def issues: IssuesApi
}

trait GhInfo {
  /** (username, reponame) */
  def ghRepository: (String, String)

  /** (username, password) */
  def ghCredentials: (String, String)
}

/** Preconfigured IssuesProvider */
trait DefaultIssuesProvider extends IssuesProvider {
  self: GhInfo =>

  lazy val issues = new IssuesApi {
    val ghInfo = self
  }
}

private [gh] trait IssuesApi {
  import dispatch._
  import dispatch.Http._
  import dispatch.liftjson.Js._
  import net.liftweb.json._
  import net.liftweb.json.JsonAST._
  import java.net.URLEncoder

  type One[A] = JValue => Option[A]
  type Many[A] = JValue => List[A]

  val ghInfo: GhInfo

  lazy val ghRepo = ghInfo.ghRepository._2

  lazy val ghUser = ghInfo.ghRepository._1

  lazy val auth = ghInfo.ghCredentials

  private [gh] val github = :/("github.com") / "api" / "v2" / "json" / "issues"

  private [gh] def http = new Http {
    /** quiet dispatch request logging */
    override def make_logger = new Logger {
      def info(msg: String, items: Any*) = ()
    }
  }

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
