package edu.luc.cs.laufer.cs473.shapes

object BoundingBox {
  def boundingBox(s: Shape): Location = s match {
    case Rectangle(_, _) =>
      new Location(0, 0, s)
    case Location(x, y, shape) => {
      val b = boundingBox(shape)
      Location(x + b.x, y + b.y, b.shape)
    }
    //TODO add missing cases (see test fixtures)
    case Ellipse(x , y) => {
       new Location(-x, -y, new Rectangle( 2 * x, 2 * y))
    }
    
    // must use map and reduceLeft (or foldLeft) for Group (no mutable variables!)
    case Group(shape1: Shape, shape2: Shape) =>{
      
      val shapes:Array[Shape] = Array.apply(shape1, shape2)
      
      val locations = shapes.map(boundingBox(_));
      
      val x = locations.reduceLeft(
          (a, b) => if (a.x < b.x) a else b 
          ).x
          
      val y = locations.reduceLeft(
          (a, b) => if (a.y < b.y) a else b 
          ).y
      
      val a = locations.reduceLeft(
          (a, b) => if (a.x + a.shape.asInstanceOf[Rectangle].width 
              > b.x + b.shape.asInstanceOf[Rectangle].width) a else b 
          )
       
      val right = a.x + a.shape.asInstanceOf[Rectangle].width
      
      val b = locations.reduceLeft(
          (a, b) => if (a.y + a.shape.asInstanceOf[Rectangle].height 
              > b.y + b.shape.asInstanceOf[Rectangle].height ) a else b 
          )
      
      val top = b.y + b.shape.asInstanceOf[Rectangle].height
      
      new Location(x, y, new Rectangle(right - x, top - y))
    	
    }
  }
  
  def sizeOf(s: Shape) : Int = {
    val b = boundingBox(s).shape.asInstanceOf[Rectangle]
    b.width * b.height
  }
}
