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
    case Ellipse(x, y) => {
      new Location(-x, -y, new Rectangle(2 * x, 2 * y))
    }

    /*
    case Group(shapes : Shape, _*)  => {
      
      val array = Array.apply(shapes)
      
      Groups(array)
    }
    */

    case Group(shape1: Shape, shape2: Shape) => {

      Groups(Array.apply(shape1, shape2))
    }

    case Group(shape1: Shape, shape2: Shape, shape3: Shape) => {
      Groups(Array.apply(shape1, shape2, shape3))
    }

    case _ => error("Can not find such shape mapping")
  }

  // must use map and reduceLeft (or foldLeft) for Group (no mutable variables!)
  def Groups(shapes: Array[Shape]): Location = {

    val locations = shapes.map(boundingBox(_));

    val x = locations.reduceLeft(
      (a, b) => if (a.x < b.x) a else b).x

    val y = locations.reduceLeft(
      (a, b) => if (a.y < b.y) a else b).y

    val a = locations.reduceLeft(
      (a, b) => if (a.x + a.shape.asInstanceOf[Rectangle].width
        > b.x + b.shape.asInstanceOf[Rectangle].width) a else b)

    val right = a.x + a.shape.asInstanceOf[Rectangle].width

    val b = locations.reduceLeft(
      (a, b) => if (a.y + a.shape.asInstanceOf[Rectangle].height
        > b.y + b.shape.asInstanceOf[Rectangle].height) a else b)

    val top = b.y + b.shape.asInstanceOf[Rectangle].height

    new Location(x, y, new Rectangle(right - x, top - y))

  }
  
/*
  def sizeOf(s: Shape): Int = {
    val b = boundingBox(s).shape.asInstanceOf[Rectangle]
    b.width * b.height
  }
*/
  
  def countShapes(s: Shape): Int = s match {
    case Rectangle(_, _) => 1

    case Location(_, _, shape) => {

      if (shape.isInstanceOf[Rectangle]
        || shape.isInstanceOf[Ellipse]) 1
      else if (shape.isInstanceOf[Group]) countArray(Array.apply(shape))
      else 0
    }

    case Ellipse(_, _) => 1

    case Group(shape) => {
      countArray(Array.apply(shape))
    }

    case Group(shape1, shape2) => {
      countArray(Array.apply(shape1, shape2))
    }

    case Group(shape1, shape2, shape3) => {
      countArray(Array.apply(shape1, shape2, shape3))
    }

    case _ => error("Can not find such shape mapping")
  }

  def countArray(shapes: Array[Shape]): Int = {
    shapes.map(countShapes(_)).foldLeft(0)(_ + _)
  }
}
