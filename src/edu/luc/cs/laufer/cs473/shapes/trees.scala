package edu.luc.cs.laufer.cs473.shapes

class Tree {

}

case class Apple(weight: Float) extends Tree

case class Peach(weight: Float) extends Tree

case class Mango(weight: Float) extends Tree

case class Branch(ts: Seq[Tree]) extends Tree {
  require(ts != null)
  require(!ts.contains(null))
}

case class Cluster(multiplier: Int, t: Tree) extends Tree {
  require(t != null)
}

trait TreeAlgebra[T] {
  def visitMango(m: Mango): T
  def visitPeach(p: Peach): T
  def visitBrach(rs: Seq[T], b: Branch): T
  def visitCluster(r: T, c: Cluster): T
}

class Grow(factor: Int) extends TreeAlgebra[Tree] {
  override def visitMango(m: Mango) = Mango(m.weight * factor)
  override def visitPeach(p: Peach) = Peach(p.weight * factor)
  override def visitBrach(rs: Seq[Tree], b: Branch) = Branch(rs)
  override def visitCluster(r: Tree, c: Cluster) = Cluster(c.multiplier, r)
}
