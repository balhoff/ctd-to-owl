package org.renci.translator.ctd

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.geneontology.whelk.{AtomicConcept, Bridge, ConceptAssertion, ConceptInclusion, Reasoner, ReasonerState, Role, RoleAssertion, Individual => WIndividual}
import scala.collection.JavaConverters._
import scala.xml.Elem
import scala.xml.Node
import scala.xml.XML

import org.geneontology.whelk.AtomicConcept
import org.geneontology.whelk.Bridge
import org.geneontology.whelk.ConceptAssertion
import org.geneontology.whelk.ConceptInclusion
import org.geneontology.whelk.{Individual => WIndividual}
import org.geneontology.whelk.Reasoner
import org.geneontology.whelk.ReasonerState
import org.geneontology.whelk.Role
import org.geneontology.whelk.RoleAssertion
import org.phenoscape.scowl._
import org.renci.translator.ctd.Model._
import org.renci.translator.ctd.Model.Gene
import org.renci.translator.ctd.Vocab._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.TurtleDocumentFormat
import org.semanticweb.owlapi.io.FileDocumentTarget
import org.semanticweb.owlapi.model.{IRI, OWLAxiom, OWLNamedIndividual}

import scala.collection.JavaConverters._
import scala.xml.{Elem, Node, XML}

object Main extends App with LazyLogging {

  //TODO handle multiple actions

  def process(ixn: Elem, reasoner: ReasonerState): Set[OWLAxiom] = {
    val id = (ixn \ "@id").head.text
    val pmids = (ixn \ "reference").flatMap(_ \ "@pmid").map(_.text).map(p => IRI.create(s"$PMID/$p"))
    val axioms = (ixn \ "taxon").zipWithIndex.flatMap {
      case (taxonNode, taxonIndex) =>
        interaction(ixn, taxonIndex).map {
          case (ixnInd, _, ixnAxioms) =>
            val pubmedLinks = pmids.map(ixnInd Annotation(DCSource, _))
            val taxon = Class(s"$OBO/NCBITaxon_${(taxonNode \ "@id").head.text}")
            val label = s"${taxonNode.text}#$id"
            val organismID = s"${ixnInd.getIRI}#organism"
            val organism = Individual(organismID)
            ixnAxioms ++ pubmedLinks ++ Set(
              organism Type taxon,
              organism Annotation(RDFSLabel, label),
              ixnInd Fact(OccursIn, organism))
        }.toSet.flatten
    }.toSet
    val inferences = Reasoner.assert(axioms.flatMap(Bridge.convertAxiom).collect { case ci: ConceptInclusion => ci }, reasoner)
    val inferredAxioms = inferences.classAssertions.map { case ConceptAssertion(AtomicConcept(classIRI), WIndividual(indIRI)) => ClassAssertion(Class(classIRI), Individual(indIRI)) } ++
      inferences.roleAssertions.map { case RoleAssertion(Role(roleIRI), WIndividual(subjectIRI), WIndividual(targetIRI)) => ObjectPropertyAssertion(ObjectProperty(roleIRI), Individual(subjectIRI), Individual(targetIRI)) }
    axioms ++ inferredAxioms
  }

  def interaction(ixnNode: Node, taxonIndex: Int): Option[(OWLNamedIndividual, OWLNamedIndividual, Set[OWLAxiom])] = {

    val id = (ixnNode \ "@id").head.text
    val ixnID = s"$CTDIXN$id#$taxonIndex"
    val ixnInd = Individual(ixnID)

    val maybeAffectorAndAxioms = handleIxn(ixnNode) match {

      case Interaction("exp" :: Nil, _, (chem: Chemical) :: (gene: Gene) :: Nil) =>
        val (chemInd, chemAxioms) = chem.owl(taxonIndex)
        val chemProcess = Individual(s"${chemInd.getIRI.toString}-process")
        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
        val axn = (ixnNode \ "axn").head
        val relation = processToProcess((axn \ "@degreecode").head.text)
        Some(chemProcess, chemAxioms ++ geneAxioms ++ Set(
          ixnInd Type GeneExpression,
          chemProcess Type Process,
          chemProcess Fact(EnabledBy, chemInd),
          chemProcess Fact(relation, ixnInd),
          ixnInd Fact(HasInput, geneInd)))

      case Interaction("exp" :: Nil, _, (affectingGene: Gene) :: (gene: Gene) :: Nil) =>
        val (affectingGeneInd, affectingGeneAxioms) = affectingGene.owl(taxonIndex)
        val affectingGeneFunction = Individual(s"${affectingGeneInd.getIRI.toString}-function")
        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
        val axn = (ixnNode \ "axn").head
        val relation = processToProcess((axn \ "@degreecode").head.text)
        Some(affectingGeneFunction, affectingGeneAxioms ++ geneAxioms ++ Set(
          ixnInd Type GeneExpression,
          affectingGeneFunction Type MolecularFunction,
          affectingGeneFunction Fact(EnabledBy, affectingGeneInd),
          affectingGeneFunction Fact(relation, ixnInd),
          ixnInd Fact(HasInput, geneInd)))

      case Interaction("exp" :: Nil, _, Interaction("w" :: Nil, cotreatmentNode, chemicals) :: (gene: Gene) :: Nil) if chemicals.forall(_.isInstanceOf[AtomicActor]) =>
        val (inputs, inputsAxioms) = chemicals.collect { case actor: AtomicActor => actor.owl(taxonIndex) }.unzip
        val cotreatmentID = (cotreatmentNode \ "@id").head.text
        val cotreatmentIRI = s"$CTDIXN$cotreatmentID"
        val cotreatmentInd = Individual(cotreatmentIRI)
        val cotreatmentAxioms = inputs.map(cotreatmentInd Fact(HasInput, _))
        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
        val axn = (ixnNode \ "axn").head
        val relation = processToProcess((axn \ "@degreecode").head.text)
        Some(cotreatmentInd, inputsAxioms.toSet.flatten ++ geneAxioms ++ cotreatmentAxioms ++ Set(
          ixnInd Type GeneExpression,
          cotreatmentInd Type CoTreatment,
          cotreatmentInd Fact(relation, ixnInd),
          ixnInd Fact(HasInput, geneInd)))

      case Interaction("rxn" :: Nil, _, (chem: Chemical) :: Interaction(_, innerNode, _) :: Nil) =>
        interaction(innerNode, taxonIndex).map {
          case (_, innerAffector, innerAxioms) =>
            val (chemInd, chemAxioms) = chem.owl(taxonIndex)
            val axn = (ixnNode \ "axn").head
            val relation = processToProcess((axn \ "@degreecode").head.text)
            (ixnInd, innerAxioms ++ chemAxioms ++ Set(
              ixnInd Type Process,
              ixnInd Fact(EnabledBy, chemInd),
              ixnInd Fact(relation, innerAffector)))
        }

      case Interaction("myl" :: Nil, _, (chem: Chemical) :: (gene: Gene) :: Nil) =>
        val (chemInd, chemAxioms) = chem.owl(taxonIndex)
        val chemProcess = Individual(s"${chemInd.getIRI.toString}-process")
        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
        val axn = (ixnNode \ "axn").head
        val relation = processToProcess((axn \ "@degreecode").head.text)
        Some(chemProcess, chemAxioms ++ geneAxioms ++ Set(
          ixnInd Type Methylation,
          chemProcess Type Process,
          chemProcess Fact(EnabledBy, chemInd),
          chemProcess Fact(relation, ixnInd),
          ixnInd Fact(HasInput, geneInd)))

      case Interaction("act" :: Nil, _, (chem: Chemical) :: (gene: Gene) :: Nil) =>
        val (chemInd, chemAxioms) = chem.owl(taxonIndex)
        val chemProcess = Individual(s"${chemInd.getIRI.toString}-process")
        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
        val axn = (ixnNode \ "axn").head
        val relation = processToProcess((axn \ "@degreecode").head.text)
        Some(chemProcess, chemAxioms ++ geneAxioms ++ Set(
          ixnInd Type MolecularFunction,
          chemProcess Type Process,
          chemProcess Fact(EnabledBy, chemInd),
          chemProcess Fact(relation, ixnInd),
          ixnInd Fact(EnabledBy, geneInd)))

      case Interaction("rxn" :: Nil, _, (gene: Gene) :: Interaction(_, expressionNode, _) :: Nil) =>
        interaction(expressionNode, taxonIndex).map {
          case (_, innerAffector, innerAxioms) =>
            val (geneInd, geneAxioms) = gene.owl(taxonIndex)
            val axn = (ixnNode \ "axn").head
            val relation = processToProcess((axn \ "@degreecode").head.text)
            (ixnInd, innerAxioms ++ geneAxioms ++ Set(
              ixnInd Type MolecularFunction,
              ixnInd Fact(EnabledBy, geneInd),
              ixnInd Fact(relation, innerAffector)))
        }

      case Interaction("rec" :: Nil, _, (gene: Gene) :: (chem: Chemical) :: Nil) =>
        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
        val (chemInd, chemAxioms) = chem.owl(taxonIndex)
        val geneFunction = Individual(s"${geneInd.getIRI.toString}-function")
        val axn = (ixnNode \ "axn").head
        val relation = processToProcess((axn \ "@degreecode").head.text)
        Some(geneFunction, chemAxioms ++ geneAxioms ++ Set(
          ixnInd Type ResponseToChemical,
          geneFunction Type MolecularFunction,
          geneFunction Fact(EnabledBy, geneInd),
          geneFunction Fact(relation, ixnInd),
          ixnInd Fact(HasInput, chemInd)))

      case Interaction("pho" :: Nil, _, (chem: Chemical) :: (gene: Gene) :: Nil) =>
        val (chemInd, chemAxioms) = chem.owl(taxonIndex)
        val chemProcess = Individual(s"${chemInd.getIRI.toString}-process")
        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
        val axn = (ixnNode \ "axn").head
        val relation = processToProcess((axn \ "@degreecode").head.text)
        Some(chemProcess, chemAxioms ++ geneAxioms ++ Set(
          ixnInd Type Phosphorylation,
          chemProcess Type Process,
          chemProcess Fact(EnabledBy, chemInd),
          chemProcess Fact(relation, ixnInd),
          ixnInd Fact(HasInput, geneInd)))

      case Interaction("b" :: Nil, _, (chem: Chemical) :: (gene: Gene) :: Nil) =>
        val (chemInd, chemAxioms) = chem.owl(taxonIndex)
        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
        Some(ixnInd, chemAxioms ++ geneAxioms ++ Set(
          ixnInd Type Binding,
          ixnInd Fact(EnabledBy, chemInd), //FIXME GO molecular functions (binding) shouldn't be enabled by chemicals
          ixnInd Fact(HasInput, geneInd)))

      case Interaction("rxn" :: Nil, _, (chem: Chemical) :: Interaction("exp" :: Nil, expressionNode, (_: Gene) :: (_: Gene) :: Nil) :: Nil) =>
        interaction(expressionNode, taxonIndex).map {
          case (_, innerAffector, innerAxioms) =>
            val (chemInd, chemAxioms) = chem.owl(taxonIndex)
            val axn = (ixnNode \ "axn").head
            val relation = processToProcess((axn \ "@degreecode").head.text)
            (ixnInd, innerAxioms ++ chemAxioms ++ Set(
              ixnInd Type Process,
              ixnInd Fact(EnabledBy, chemInd),
              ixnInd Fact(relation, innerAffector)))
        }

      case Interaction(itype :: Nil, _, (chem: Chemical) :: (gene: Gene) :: Nil)
        if Set("sec", "loc", "clv", "mut", "deg", "spl", "rec", "sta", "met", "oxd", "ubq", "nit", "upt", "red", "alk", "sum", "gyc")(itype) =>
        val (chemInd, chemAxioms) = chem.owl(taxonIndex)
        val chemProcess = Individual(s"${chemInd.getIRI.toString}-process")
        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
        val axn = (ixnNode \ "axn").head
        val relation = processToProcess((axn \ "@degreecode").head.text)
        val ixnType = IxnTypes(itype)
        Some(chemProcess, chemAxioms ++ geneAxioms ++ Set(
          ixnInd Type ixnType,
          chemProcess Type Process,
          chemProcess Fact(EnabledBy, chemInd),
          chemProcess Fact(relation, ixnInd),
          ixnInd Fact(HasInput, geneInd)))

      case Interaction(itype :: Nil, _, (gene: Gene) :: (chem: Chemical) :: Nil)
        if Set("met", "trt", "glc", "csy", "upt", "oxd", "sec", "red", "hdx")(itype) =>
        //TODO no "abu", "b", "act" for now - different model?
        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
        val (chemInd, chemAxioms) = chem.owl(taxonIndex)
        val geneFunction = Individual(s"${geneInd.getIRI.toString}-function")
        val axn = (ixnNode \ "axn").head
        val relation = processToProcess((axn \ "@degreecode").head.text)
        val ixnType = IxnTypes(itype)
        Some(geneFunction, chemAxioms ++ geneAxioms ++ Set(
          ixnInd Type ixnType,
          geneFunction Type MolecularFunction,
          geneFunction Fact(EnabledBy, geneInd),
          geneFunction Fact(relation, ixnInd),
          ixnInd Fact(HasInput, chemInd)))

      case _ => None //FIXME

    }

    maybeAffectorAndAxioms.map {
      case (affector, axioms) => (ixnInd, affector, axioms)
    }

  }

  def handleIxn(node: Node): Interaction = {
    val codes = (node \ "axn").map(_ \ "@code").map(_.text).toList
    val actors = (node \ "actor").map(handleActor).toList
    Interaction(codes, node, actors)
  }

  def handleActor(node: Node): Actor = (node \ "@type").head.text match {
    case "ixn"      => handleIxn(node)
    case "chemical" => Chemical(node)
    case "gene"     => Gene(node)
  }

  val root = XML.loadFile("CTD_chem_gene_ixns_structured.xml")
  //val root = XML.loadFile("testowl.xml")
  val ro = OWLManager.createOWLOntologyManager().loadOntology(IRI.create("http://purl.obolibrary.org/obo/ro.owl"))
  val reasoner = Reasoner.assert(Bridge.ontologyToAxioms(ro))
  val ixns = root \ "ixn"
  val chunks = ixns.grouped(10000).zipWithIndex

  for {
    (chunk, index) <- chunks
  } {
    val axioms = chunk.flatMap {
      case elem: Elem => process(elem, reasoner)
    }.toSet
    val manager = OWLManager.createOWLOntologyManager()
    val ontology = manager.createOntology(axioms.asJava)
    manager.saveOntology(ontology, new TurtleDocumentFormat(), new FileDocumentTarget(new File(s"ctdcams/ctd-cam-$index.ttl")))
  }

}