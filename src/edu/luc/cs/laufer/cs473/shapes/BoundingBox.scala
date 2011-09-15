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

    case Group(shapes @ _*) => GroupLocation(shapes.toList)

    case _ => error("Can not find such shape mapping")
  }

  // must use map and reduceLeft (or foldLeft) for Group (no mutable variables!)
  def GroupLocation(shapes: List[Shape]): Location = {

    //TODO: 
    // How to improve this code?
    // use flatMap, such as map to List(x, x+width), List(y, y+height))
    // then we can use reduceLeft to get max/min value
    //

    val locations = shapes.map(boundingBox(_))

    val x = locations.map(s => s.x)
      .reduceLeft((a, b) => if (a < b) a else b)

    val y = locations.map(s => s.y)
      .reduceLeft((a, b) => if (a < b) a else b)

    val width = locations.map(s => s.x + s.shape.asInstanceOf[Rectangle].width)
      .reduceLeft((a, b) => if (a > b) a else b) - x

    val height = locations.map(s => s.y + s.shape.asInstanceOf[Rectangle].height)
      .reduceLeft((a, b) => if (a > b) a else b) - y

    new Location(x, y, new Rectangle(width, height))

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
