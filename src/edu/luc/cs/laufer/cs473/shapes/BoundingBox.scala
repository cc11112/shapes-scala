package edu.luc.cs.laufer.cs473.shapes

import scala.collection.mutable

object BoundingBox {
  def boundingBox(s: Shape): Location = s match {
    case Rectangle(_, _) =>
      new Location(0, 0, s)
    case Location(x, y, shape) => {
      val b = boundingBox(shape)
      Location(x + b.x, y + b.y, b.shape)
    }

    //TODO add missing cases (see test fixtures)
    case Ellipse(x, y) => new Location(-x, -y, new Rectangle(2 * x, 2 * y))

    // must use map and reduceLeft (or foldLeft) for Group (no mutable variables!)
    case Group(shapes @ _*) => {

      // use flatMap, such as map to List(x, x+width), List(y, y+height))
      // then we can use first/last to get max/min
      //

      val x: List[Int] = shapes.map(boundingBox(_)).map(
        s => List(s.x, s.x + s.shape.asInstanceOf[Rectangle].width))
        .flatMap(x => x).toList.sort(_ < _)

      val y: List[Int] = shapes.map(boundingBox(_)).map(
        s => List(s.y, s.y + s.shape.asInstanceOf[Rectangle].height))
        .flatMap(y => y).toList.sort(_ < _)

      new Location(
        x.first,
        y.first,
        new Rectangle(
          x.last - x.first,
          y.last - y.first))

      //        
      //    val locations = shapes.map(boundingBox(_))
      //
      //    val x = locations.map(s => s.x)
      //      .reduceLeft((a, b) => if (a < b) a else b)
      //
      //    val y = locations.map(s => s.y)
      //      .reduceLeft((a, b) => if (a < b) a else b)
      //
      //    val width = locations.map(s => s.x + s.shape.asInstanceOf[Rectangle].width)
      //      .reduceLeft((a, b) => if (a > b) a else b) - x
      //
      //    val height = locations.map(s => s.y + s.shape.asInstanceOf[Rectangle].height)
      //      .reduceLeft((a, b) => if (a > b) a else b) - y
      //
      //    new Location(x, y, new Rectangle(width, height))
      //
    }

    case _ => error("Can not find such shape mapping")
  }

  // implement size function here

  def sizeOf(s: Shape): Int = s match {
    case Rectangle(_, _) => 1

    case Ellipse(_, _) => 1

    case Location(_, _, shape) => sizeOf(shape)

    case Group(shapes @ _*) => shapes.map(sizeOf(_)).foldLeft(0)(_ + _)

    case _ => error("Can not find such shape mapping")
  }

}
