package gh

import sbt._
import Keys._

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
  def many(js: JValue) =
    for(JArray(value) <- js \ "labels"; JString(label) <- value) yield label
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

case class Issue(user: String, gravatar: String, updatedAt: Date, votes: BigInt,
                 number: BigInt, position: Double, title: String, body: String,
                 state: String, createdAt: Date, labels: List[String])

case class Comment(id: BigInt, user: String, gravatar: String, body: String,
                   createdAt: Date, updatedAt: Date)


// sbt should resolve this as the plugin
object SbtIssues extends LabelTasks with IssueTasks with CommentTasks

trait LabelTasks extends Plugin with ColorizedLogging {
  import net.liftweb.json.JsonAST._
  implicit def manyLabels(js: JValue) = for(l <- Labels.many(js)) yield l

  val ghLabels = InputKey[Unit]("gh-labels", "Lists Labels associated with Github repo")
  val ghAddLabel = InputKey[Unit]("gh-add-label", "Adds a Label to a Github Issue")
  val ghRemoveLabel = InputKey[Unit]("gh-remove-label", "Removes a Label from a gh issue")

  override def settings = Github.settings ++ Seq(
    ghLabels <<= inputTask { (argstask: TaskKey[Seq[String]]) =>
      (argstask, Github.api) map { (args, api) =>
        api.labels {
          (_: List[String]).foreach(labelListing)
        }
      }
    },

    ghAddLabel <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
      (argTask, Github.api, Github.repository, streams) map { (args, api, repository, out) =>
         args match {
          case label :: num :: Nil => try {
            val (user, repo) = repository
            api.addLabel(label, num.toLong) { labels: List[String] =>
              out.log.info("""Added label "%s" to Issue %s of %s/%s""" format(label, num, user, repo))
            }
          } catch { case _ => out.log.warn("invalid arguments label: %s, num: %s" format(label, num)) }
          case _ => out.log.warn("""usage: gh-add-label "<label>" <num>""")
        }
      }
    },

    ghRemoveLabel <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
      (argTask, Github.api, Github.repository, streams) map { (args, api, repository, out) =>
        args match {
          case label :: num :: Nil=> try {
            val (user, repo) = repository
            api.removeLabel(label, num.toLong) { labels: List[String] =>
              out.log.info("""Removed label "%s" from Issue %s of %s/%s""" format(label, num, user, repo))
            }
          } catch { case _ => out.log.warn("invalid arguments label: %s, num:%s" format(label, num)) }
          case _ => out.log.warn("usage: gh-remove-label <label> <num>")
        }
      }
    })

}

trait IssueTasks extends Plugin with ColorizedLogging {
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

  val ghIssue = InputKey[Unit]("gh-issue", "Shows a Github Issue by number")
  val ghIssues = InputKey[Unit]("gh-issues", "Lists open github issues")
  val ghClosedIssues = InputKey[Unit]("gh-closed-issues", "Lists closed Github Issues")
  val ghSearchOpenIssues = InputKey[Unit]("gh-search-open-issues","Search for open Github Issues by terms")
  val ghSearchClosedIssues = InputKey[Unit]("gh-search-closed-issues", "Search for closed Github Issues by terms")
  val ghOpen = InputKey[Unit]("gh-open", "Opens a new Github Issue")
  val ghEdit = InputKey[Unit]("gh-edit", "Edits a Github Issue")
  val ghClose = InputKey[Unit]("gh-close", "Closes a Github Issue")

  override def settings = Github.settings ++ Seq(
    ghIssue <<= inputTask {  (argTask: TaskKey[Seq[String]]) =>
      (argTask, Github.api, streams) map { (args, api, out) =>
        args match {
         case num :: Nil =>
           api.issue(num.toLong) { (_: Option[Issue]) match {
             case Some(is) => issueDetail(is)
             case _ => out.log.info("This project has no issue %s" format num)
           } }
         case _ => task { Some("usage: gh-issue <num>") }
        }
      }
    },

    ghIssues <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
       (argTask, Github.api, Github.repository, streams) map { (args, api, repository, out) =>
         api.issues { (_: List[Issue]) match {
           case Nil => out.log.info("This project has no issues (at least no documented issues)")
           case l =>
             val (user, repo) = repository
             out.log.info("Open issues for %s/%s" format(user, repo))
             for(is <- l) issueListing(is)
         } }
      }
    },

    ghClosedIssues <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
      (argTask, Github.api, Github.repository, streams) map { (args, api, repository, out) =>
        api.closedIssues { (_: List[Issue]) match {
          case Nil => out.log.info("This project has no closed issues.")
          case l =>
            val (user, repo) = repository
            out.log.info("Closed issues for %s/%s" format(user, repo))
            for(is <- l) issueListing(is)
        } }
      }
    },

    ghSearchOpenIssues <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
      (argTask, Github.api, Github.repository, streams) map { (args, api, repository, out) =>
        args match {
          case Nil => out.log.warn("usage: gh-search-open-issues 'terms to search for'")
          case terms =>
            val (user, repo) = repository
            api.searchOpen(terms.mkString(" ")) { (_: List[Issue]) match {
              case Nil => out.log.info("no open issues with the terms %s for %s/%s" format(terms.mkString(" "), user, repo))
              case l =>
                out.log.info("%s search results for %s/%s" format(l.size, user, repo))
                for(is <- l) issueListing(is)
           } }
        }
      }
    },

    ghSearchClosedIssues <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
      (argTask, Github.api, Github.repository, streams) map { (args, api, repository, out) =>
        args match {
          case Nil => out.log.warn("usage: gh-search-closed-issues 'terms to search for'")
          case terms =>
            val (user, repo) = repository
            api.searchClosed(terms.mkString(" ")) { (_: List[Issue]) match {
              case Nil => out.log.info("no closed issues with the terms %s for %s/%s" format(terms.mkString(" "), user, repo))
              case l =>
                out.log.info("%s search results" format l.size)
                for(is <- l) issueListing(is)
            } }
        }
      }
    },

    ghOpen <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
      (argTask, Github.api, Github.repository, streams) map { (args, api, repository, out) =>
        args match {
          case title :: desc :: Nil =>
            api.openIssue(title, desc) { (_: Option[Issue]) match {
              case Some(is) =>
                val (user, repo) = repository
                out.log.info("""Opened issue %s "%s" as @%s for %s/%s""" format(is.number, is.title, is.user, user, repo))
              case _ => out.log.warn("error creating issue")
            } }
          case _ => out.log.warn("""usage: gh-open "<title>" "<description>" """)
        }
      }
    },

    ghEdit <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
      (argTask, Github.api, Github.repository, streams) map { (args, api, repository, out) =>
        args match {
          case num :: title :: desc :: Nil => try {
            api.editIssue(num.toLong, title, desc) { (_: Option[Issue]) match {
              case Some(is) =>
                val (user, repo) = repository
                out.log.info("""Edited issue %s "%s" as @%s for %s/%s""" format(is.number, is.title, is.user, user, repo))
             case _ => out.log.warn("Error editing issue")
            } }
          } catch { case _ => out.log.warn("Invalid arguments num: %s, title: %s, desc: %s" format(num, title, desc)) }
          case _ => out.log.warn("""usage: gh-edit <num> "<title>" "<description>" """)
        }
      }
    },

    ghClose <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
      (argTask, Github.api, Github.repository, streams) map { (args, api, repository, out) =>
        args match {
          case num :: Nil =>
            api.closeIssue(num.toLong) { (_: Option[Issue]) match {
              case Some(is) =>
                val (user, repo) = repository
                out.log.info("""Closed issue %s "%s" as @%s for %s/%s""" format(is.number, is.title, is.user, user, repo))
              case _ => out.log.warn("error creating issue")
            } }
          case _ => out.log.warn("usage: gh-close <num>")
        }
      }
    })
}

trait CommentTasks extends Plugin with ColorizedLogging {
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

  val ghComments = InputKey[Unit]("gh-comments", "Lists Comments on a Github Issue")
  val ghComment = InputKey[Unit]("gh-comment", "Posts a new Comment on a Github Issue")

  override def settings = Github.settings ++ Seq(
    ghComments <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
      (argTask, Github.api, Github.repository, streams) map { (args, api, repository, out) =>
        args match {
          case num :: Nil => try {
            val (user, repo) = repository
            out.log.info("Comments on issue %s for %s/%s" format(num, user, repo))
            api.comments(num.toLong) { (_: List[Comment]) match {
              case Nil => out.log.info("There were no comments on this issue")
              case l => for (c <-l) commentListing(c)
            } }
          } catch { case _ => out.log.warn("Invalid arguments num: %s" format num) }
          case _ => out.log.warn("usage: gh-comments <num>")
        }
      }
    },

    ghComment <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
      (argTask, Github.api, Github.repository, streams) map { (args, api, repository, out) =>
        args match {
          case num :: comm :: Nil => try {
            val (user, repo) = repository
            api.comment(num.toLong, comm) { (_: Option[Comment]) match {
              case Some(c) => out.log.info("""Posted comment "%s" on issue %s as @%s for %s/%s""" format(
                c.body, num, c.user, user, repo)
              )
              case _ => out.log.warn("Comment not posted")
            } }
          } catch { case _ => out.log.warn("Invalid arguments num: %s, comment: %s" format(num, comm)) }
          case _ => out.log.warn("""usage: gh-comment <num> ""<comment>"" """)
        }
      }
    })
}

object Github {
  val repository = SettingKey[(String, String)]("gh-repository", "A tuple of (user,repository) hosted on github")
  val credentials = SettingKey[(String, String)]("gh-credentials", "A tuple of (user, password) to authenticate with the github api")
  val api = SettingKey[gh.IssuesApi]("gh-api")

  def settings = Defaults.defaultSettings ++ Seq(
    repository := ("<user>", "<repo>"),
    credentials := ("<user>", "<password>"),
    api <<= (repository, credentials) { (repo, creds) =>
      new IssuesApi {
        val auth = creds
        val ghUser = repo._1
        val ghRepo = repo._2
      }
    }
  )
}
