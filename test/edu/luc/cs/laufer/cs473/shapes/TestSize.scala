package edu.luc.cs.laufer.cs473.shapes;

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import BoundingBox._
import TestFixtures._

@RunWith(classOf[JUnitRunner])
class TestSize extends FunSuite {

  def testSizeOfShape(description: String, s: Shape, count: Int) = {
    test(description) {
      assert(count === sizeOf(s))
    }
  }

  testSizeOfShape("simple ellipse", simpleEllipse, 1)
  testSizeOfShape("simple rectangle", simpleRectangle, 1)
  testSizeOfShape("simple location", simpleLocation, 1)
  testSizeOfShape("basic group", basicGroup, 2)
  testSizeOfShape("simple group", simpleGroup, 2)
  testSizeOfShape("complex group", complexGroup, 5)

}
