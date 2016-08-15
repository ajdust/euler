/**
  * Created by aaron on 8/14/2016.
  */
package Problems

object Sequences {

  private def fibonacciTuple = Stream.iterate((0,1))(t => (t._2, t._1 + t._2))

  def fibonacci = fibonacciTuple.map(x => x._1)

}
