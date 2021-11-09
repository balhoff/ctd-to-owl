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

    def owl(taxonIndex: Int, meshToCHEBI: Map[String, String], includeLabels: Boolean): (OWLNamedIndividual, Set[OWLAxiom]) = {
      val actorClass = actorType((node \ "@id").head.text, (node \ "@type").head.text, meshToCHEBI)
      val position = (node \ "@position").head.text
      val parentID = (node \ "@parentid").head.text
      val ixnID = s"$CTDIXN$parentID#$taxonIndex"
      val actorInd = Individual(s"$ixnID-$position")
      val labelAxioms = if (includeLabels) {
        val typeLabel = node.text
        val label = s"$typeLabel#$parentID-$position"
        Set(actorClass Annotation(RDFSLabel, typeLabel), actorInd Annotation(RDFSLabel, label))
      } else Set.empty
      val formAnnotations = (node \ "@form").map(_.text).map(form => actorInd Annotation(HasForm, form))
      (actorInd, Set(
        actorInd Type actorClass,
        actorInd Type nodeType
      ) ++ labelAxioms ++ formAnnotations)
    }

  }

  final case class Chemical(node: Node) extends AtomicActor {

    def nodeType: OWLClass = ChemicalEntity

  }

  final case class Gene(node: Node) extends AtomicActor {

    def nodeType: OWLClass = Vocab.Gene

  }

  def actorType(id: String, tpe: String, meshToCHEBI: Map[String, String]): OWLClass = tpe match {
    case "chemical" =>
      val iri = meshToCHEBI.get(id).map(_.replaceAllLiterally("CHEBI:", s"$CHEBI")).getOrElse {
        scribe.warn(s"No mapping for MESH: $id")
        id.replaceAllLiterally("MESH:", s"$MESH")
      }
      Class(iri)
    case "gene"     => Class(id.replaceAllLiterally("GENE:", NCBIGENE))
  }

}
