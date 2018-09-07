package org.renci.translator.ctd

import scala.xml.Elem
import scala.xml.Node

object Util {

  def printCombosCounts(root: Elem): Unit = {

    def ixnTuple(node: Node): Seq[String] = {
      val codes = (node \ "axn").map(_ \ "@code").map(_.text) //.mkString(",")
      val actors = (node \ "actor")
      val tuples = codes.map { code =>
        val actorTypes = actors.flatMap(actorTuple).mkString(",")
        s"[`$code` $actorTypes]"
      }
      tuples
      //actors.flatMap(actorTuple).toSet ++ tuples
    }

    def actorTuple(node: Node): Seq[String] = (node \ "@type").head.text match {
      case "ixn" => ixnTuple(node)
      case other => Seq(other)
    }

    (root \ "ixn").flatMap(ixnTuple).groupBy(identity).mapValues(_.size).toSeq.sortBy(-_._2).foreach { case (entry, count) => println(s"$count\t$entry") }
  }

}