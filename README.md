# sbt gh issues

plugin for [github](http://github.com) [issues](http://develop.github.com/p/issues.html) for those `wired into` [sbt](http://code.google.com/p/simple-build-tool/)

![gh issues](https://github.com/downloads/softprops/sbt-gh-issues/sbt-gh-issues.jpg)

## usage

This plugin adds several Github Issues related tasks to your project.

These tasks are broken down into three traits

### gh.IssueTasks

    gh-issues # lists open issues
    gh-closed-issues # lists closed issues
    gh-issue <num> # shows the details of a gh issue by number
    gh-open <title> <desc> # opens a new gh issue
    gh-close <num> # closes a gh issue by number
    gh-edit <num> <new_title> <new_desc> # edits a gh issue
    gh-search-closed-issues <term> # Search for closed gh issues by terms
    gh-search-open-issues <term> # Search for open gh issues by terms

### gh.LabelTasks

    gh-labels # Lists a labels for the current repo
    gh-add-label <label> <num> # Adds a label to a gh issue
    gh-remove-label <label> <num> # Removes a label from a gh issue

### gh. CommentTasks

    gh-comments <num> # lists all comments on a gh issue
    gh-comment <num> <comment> # adds a comment on a gh issue

To get a the full list available in the sbt prompt type

     > gh<tab>
Some tasks listed above take arguments. Those are annotated with `<arg1> <arg2>`.
If you forget what arguments a task takes, just execute the task with no args. This will reveal a `usage` statement to help. For arguments that are strings, be sure to double-quote their values ( `gh-open "something's wrong" "oh something is definitely wrong!"` )

For simplicity, you can can just mix `gh.Issues` (includes all three) into your `ProjectDefinition`.

Declare the gh issues plugin in your `PluginDefinition`.

    > cat project/plugins/plugins.scala
    import sbt._

    class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
      val lessis = "less is repo" at "http://repo.lessis.me"
      val ghIssues = "me.lessis" % "sbt-gh-issues" % "0.1.0"
    }

The only dependency this plugin requires is a Github user to authenticate as an a Github repository to point at, both defined as 2 item tuples in your `ProjectDefinition`.

    > cat project/build/project.scala
    import sbt._

    class Project(info: ProjectInfo) extends DefaultProject(info) with gh.Issues {
       def ghCredentials = (<your-gh-username>, <your-gh-password>)
       def ghRepository = (<gh-user>, <gh-repo>)
    }

Obviously, you will not want to check in your github username and password with your source code and push that to github :). To supply
the `ghCredentials` tuple. You can use the `LocalGhCreds` function which assumes you have an externally defined `.gh`
file containing your github username and password. This should follow the standard Java [Properties](http://download.oracle.com/javase/6/docs/api/java/util/Properties.html) file format.

    > echo "username=your-gh-username
    password=your-gh-password" > ~/.gh

    > cat ~/.gh
    username=your-gh-username
    password=your-gh-password

Your `ProjectDefinition` would then become

    > cat project/build/project.scala
    import sbt._

    class Project(info: ProjectInfo) extends DefaultProject(info) with gh.Issues {
       def ghCredentials = gh.LocalGhCreds(log)
       def ghRepository = (<gh-user>, <gh-repo>)
    }

### Logging

Some logging is colorized. The default options (which are overridable in your `ProjectDefinition`) are.

    def ghUserColor = Console.BOLD
    def ghIssueTitleColor = Console.BOLD
    def ghIssueNumColor = Console.MAGENTA
    def ghLabelColor = Console.BOLD

I you would like to write out the logging yourself, override one of these logging methods.

    def issueListing(issue: Issue): Unit
    def issueDetail(issue: Issue): Unit
    def commentListing(comment: Comment): Unit
    def labelListing(label: String): Unit

Doug Tangren (softprops) 2011
