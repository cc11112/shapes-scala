package edu.luc.cs.laufer.cs473.shapes

abstract class Shape

case class Rectangle(width: Int, height: Int) extends Shape {
  require(width >= 0)
  require(height >= 0)
}

case class Location(x: Int, y: Int, shape: Shape) extends Shape {
  if (shape == null) {
    throw new IllegalArgumentException("null shape in location")
  }
}

// TODO add missing case classes (see test fixtures)
// TODO must include validity checking for constructor arguments
case class Ellipse(width: Int, height: Int) extends Shape {
  require(width >= 0)
  require(height >= 0)
}

case class Group(shapes: Shape*) extends Shape {
  require(shapes != null)
  require(shapes.length > 0)
  for (shape <- shapes)
    require(shape != null)
}

