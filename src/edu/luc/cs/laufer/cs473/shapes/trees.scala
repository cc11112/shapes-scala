package edu.luc.cs.laufer.cs473.shapes

abstract class Tree 

abstract class Fruit(weight: Float) extends Tree

case class Apple(weight: Float) extends Fruit(weight)

case class Peach(weight: Float) extends Fruit(weight)

case class Mango(weight: Float) extends Fruit(weight)

case class Branch(children: Seq[Tree]) extends Tree {
  require(children != null)
  require(!children.contains(null))
}

case class Cluster(multiplier: Int, child: Tree) extends Tree {
  require(child != null)
}

trait TreeAlgebra[T] {
  def visitMango(m: Mango): T
  def visitPeach(p: Peach): T
  def visitBranch(rs: Seq[T], b: Branch): T
  def visitCluster(r: T, c: Cluster): T
  
  def fold: Tree => T = {
    case m: Mango => visitMango(m)
    case p: Peach => visitPeach(p)
    case b: Branch => visitBranch(b.children.map(fold(_)), b)
    case c: Cluster => visitCluster(fold(c.child), c)
  }
}

object Weight extends TreeAlgebra[Float] {
  override def visitMango(m : Mango) = m.weight
  override def visitPeach(p: Peach) = p.weight
  override def visitBranch(rs: Seq[Float], b :Branch) = rs.reduceLeft(_ + _)
  override def visitCluster(r: Float, c: Cluster) = c.multiplier * r
}

class Grow(factor: Int) extends TreeAlgebra[Tree] {
  override def visitMango(m: Mango) = Mango(m.weight * factor)
  override def visitPeach(p: Peach) = Peach(p.weight * factor)
  override def visitBranch(rs: Seq[Tree], b: Branch) = Branch(rs)
  override def visitCluster(r: Tree, c: Cluster) = Cluster(c.multiplier, r)
}
