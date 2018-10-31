package org.renci.translator.ctd

import org.phenoscape.scowl._
import org.renci.translator.ctd.Vocab._
import org.semanticweb.owlapi.model.{OWLAxiom, OWLClass, OWLNamedIndividual}

import scala.xml.Node

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
      val typeLabel = node.text
      val label = s"$typeLabel#$parentID-$position"
      val formAnnotations = (node \ "@form").map(_.text).map(form => actorInd Annotation(HasForm, form))
      (actorInd, Set(
        actorInd Type actorClass,
        actorClass Annotation(RDFSLabel, typeLabel),
        actorInd Type nodeType,
        actorInd Annotation(RDFSLabel, label)
      ) ++ formAnnotations)
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