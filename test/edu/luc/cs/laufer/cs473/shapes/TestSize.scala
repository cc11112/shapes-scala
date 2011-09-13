package edu.luc.cs.laufer.cs473.shapes;

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import BoundingBox._
import TestFixtures._

@RunWith(classOf[JUnitRunner])
class TestSize extends FunSuite {
	
 	def testSizeOfShape(description: String, s: Shape, size: Int) = {
	    test(description) {
		  assert(size === sizeOf(s))
	    }
	  }


	  testSizeOfShape("simple ellipse", simpleEllipse, 6000)
	  testSizeOfShape("simple rectangle", simpleRectangle, 9600)
	  testSizeOfShape("simple location", simpleLocation, 9600)
	 // testBoundingBox("basic group", basicGroup,  7000) 
	 // testBoundingBox("simple group", simpleGroup,  350 * 280)
	 // testBoundingBox("complex group", complexGroup,  470 * 320)
	
}
