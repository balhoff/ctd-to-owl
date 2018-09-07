package org.renci.translator.ctd

import scala.xml.Node
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLAxiom
import org.phenoscape.scowl._
import Vocab._
import org.semanticweb.owlapi.model.OWLClass

object Model {

  sealed trait Actor

  final case class Interaction(codes: List[String], node: Node, actors: List[Actor]) extends Actor

  sealed trait AtomicActor extends Actor {

    def node: Node

    def nodeType: OWLClass

    def owl(taxonIndex: Int): (OWLNamedIndividual, Set[OWLAxiom]) = {
      val actorClass = actorType((node \ "@id").head.text, (node \ "@type").head.text)
      val position = (node \ "@position").head.text
      val parentID = (node \ "@parentid").head.text
      val ixnID = s"$CTDIXN$parentID#$taxonIndex"
      val actorInd = Individual(s"$ixnID-$position")
      val label = s"${node.text}#$parentID-$position"
      val formAnnotations = (node \ "@form").map(_.text).map(form => actorInd Annotation (HasForm, form))
      (actorInd, Set(actorInd Type actorClass, actorInd Type nodeType, actorInd Annotation (RDFSLabel, label)) ++ formAnnotations)
    }

  }

  final case class Chemical(node: Node) extends AtomicActor {

    def nodeType: OWLClass = ChemicalEntity

  }

  final case class Gene(node: Node) extends AtomicActor {

    def nodeType: OWLClass = Vocab.Gene

  }

  def actorType(id: String, tpe: String): OWLClass = tpe match {
    case "chemical" => Class(id.replaceAllLiterally("MESH:", s"$MESH/"))
    case "gene"     => Class(id.replaceAllLiterally("GENE", NCBIGENE))
  }

}