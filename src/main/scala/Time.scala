package gh

import java.util.Date

object RelativeTime {
  implicit val realtivelyNow = 1000l
  implicit def dt2rt(d: Date) = RelativeTime(d)
}
case class RelativeTime(d: Date) {
  private val conversions = ('millisecond, 1) :: ('second, 1000) ::
                            ('minute, 60) :: ('hour, 60) ::
                            ('day, 24) :: ('month, 30) ::
                            ('year, 12) :: Nil

  def relativeTime(implicit relativelyNow: Long) = {
    //@annotation.tailrec
    def rel(delt: Long, prev: (Symbol, Int), list: List[(Symbol,Int)]):
            (Long, (Symbol,Int), List[(Symbol,Int)]) =
     list match {
       case head :: tail =>
         if(delt < head._2.longValue) (delt, prev, list) else rel(delt / head._2, head, tail)
       case _ => (delt, prev, list)
     }

    def describe(delt: Long, unit: Symbol, suffix: String) =
      "%s %s %s" format(delt, if(delt != 1) unit.name + "s" else unit.name, suffix)

    (new Date().getTime - d.getTime) match {
      case l if(l < 0) =>
        val (delta, units, _) = rel(l * -1, conversions.head, conversions)
        describe(delta, units._1, "later")
      case l if(l <= relativelyNow) => "Just now"
      case l =>
       val (delta, units, _) =  rel(l, conversions.head, conversions)
       describe(delta, units._1, "ago")
    }
  }
}
