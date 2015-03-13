# Introduction #

The Netbeans plugin for Scala works, but there are some quirks to keep in mind.


# Details #
Documentation Page
http://wiki.netbeans.org/Scala


Know Issues of Scala plugin Beta

  * Formatting for multiple lines expression is not supported yet.
  * Goto-Declaration for types in "import" statements is not supported
  * Auto-completion in "import" statements does not support types defined in project's source folder (will be fixed soon)
  * Debugging into scala's standard library is not supported yet.
  * Mixing Scala + Java sources in same project is only supported for Scala-2.7.2 or higher
  * When add a Java project in Scala project libraries, you'll need to restart NetBeans to get Scala project knowing these Java classes
  * For Windows Vista users: NullPointerException at org.openide.filesystems.FileUtil.normalizeFileOnWindows (related to #135547 which may have been fixed in trunk but not for NetBeans 6.5 Beta). If you are Vista user and like to have a try on Scala plugins, you may need to download a recent nightly build version of NetBeans.
  * Due to recent underlying changes of NetBeans' "extexecution" module, Interactive Console is wrongly redirected to output window instead of "Scala Shell" window, this has been fixed in NetBeans trunk

  * If java classes are not being recognized in a scala file, clean and build the project, close the files, then open the files again and the syntax errors should disappear.


How to Set Main Class and Run

Scala Plugin does not support run/debug single Scala file yet, but, you can change and set project's Main class to run/debug it:

  1. Set project's Main class in project's properties dialog,input the full qualified name of this class, for example: scalaproject1.RunMe
> 2. Make sure the main method return UNIT type

Documentation for Scala JUnit Integration is at http://blogtrader.net/page/dcaoyuan/entry/scala_for_netbeans_screenshot_121