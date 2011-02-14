package gh

private [gh] trait GhLogging {
  def issueListing(issue: Issue): Unit
  def issueDetail(issue: Issue): Unit
  def commentListing(comment: Comment): Unit
  def labelListing(label: String): Unit
  def repoListing(user: String, repo: String): Unit
}

trait Colors {
  def resetColor[T](f: => T) = {
    print(Console.RESET)
    val r = f
    print(Console.RESET)
    r
  }

  def color[T](cols: String*)(f: => T) {
    cols.foreach(print)
    val r = f
    print(Console.RESET)
    r
  }
}

trait ColorizedLogging extends GhLogging with Colors {
  import RelativeTime._

  def ghUserColor = Console.BOLD

  def ghIssueTitleColor = Console.BOLD

  def ghIssueNumColor = Console.MAGENTA

  def ghLabelColor = Console.BOLD

  def userRepoColor = Console.MAGENTA

  private def colorizeLabels(l: List[String]) = l match {
    case Nil => "\n"
    case _ => l.mkString("[" + ghLabelColor, Console.RESET + "," + ghLabelColor, Console.RESET + "]\n")
  }

  def issueListing(i: Issue) =
    resetColor {
      color(Console.BOLD, ghIssueNumColor) {
        print(i.number)
      }
      color(Console.BOLD, ghIssueTitleColor) {
        print(" %s " format i.title)
      }
      print(i.createdAt.relativeTime)
      print(" (@")
      color(Console.BOLD, ghUserColor) {
        print(i.user)
      }
      print(") %s" format(colorizeLabels(i.labels)))
    }

  def issueDetail(i: Issue) =
    resetColor {
      color(Console.BOLD, ghIssueNumColor) {
        print(i.number)
      }
      color(Console.BOLD, ghIssueTitleColor) {
        print(" %s " format i.title)
      }
      print("(@")
      color(Console.BOLD, ghUserColor) {
        print(i.user)
      }
      print(") %s" format(colorizeLabels(i.labels)))
      println("\t%s" format i.createdAt.relativeTime)
      println("\t%s\n" format i.body)
    }

  def commentListing(c: Comment) =
    resetColor {
      print("@")
      color(Console.BOLD, ghUserColor) {
        print(c.user)
      }
      print(" %s \n\t" format c.createdAt.relativeTime)
      println(c.body)
    }

  def labelListing(l: String) =
    resetColor {
      color(Console.BOLD, ghLabelColor) {
        println(l)
      }
    }

  def repoListing(user: String, repo: String) =
    resetColor {
      print("using ")
      color(userRepoColor) {
        print(user)
      }
      print("/")
      color(userRepoColor) {
        println(repo)
      }
    }
}
