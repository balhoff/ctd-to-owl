package org.renci.translator.ctd

import java.io.File

import org.geneontology.whelk.{AtomicConcept, Bridge, BuiltIn, ConceptAssertion, ConceptInclusion, Reasoner, ReasonerState, Role, RoleAssertion, Individual => WIndividual}
import org.phenoscape.scowl._
import org.renci.translator.ctd.Model.{Gene, _}
import org.renci.translator.ctd.Vocab._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.TurtleDocumentFormat
import org.semanticweb.owlapi.io.FileDocumentTarget
import org.semanticweb.owlapi.model.{IRI, OWLAxiom, OWLNamedIndividual}

import scala.jdk.CollectionConverters._
import scala.xml.{Elem, Node, XML}

object Main extends App {

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
    if (inferences.classAssertions.exists(_.concept == BuiltIn.Bottom)) {
      println(ixn)
      axioms.foreach(println)
      System.exit(1)
    }
    val inferredAxioms = inferences.classAssertions.map { case ConceptAssertion(AtomicConcept(classIRI), WIndividual(indIRI)) => ClassAssertion(Class(classIRI), Individual(indIRI)) } ++
      inferences.roleAssertions.map { case RoleAssertion(Role(roleIRI), WIndividual(subjectIRI), WIndividual(targetIRI)) => ObjectPropertyAssertion(ObjectProperty(roleIRI), Individual(subjectIRI), Individual(targetIRI)) }
    axioms ++ inferredAxioms
  }

  def interaction(ixnNode: Node, taxonIndex: Int): Option[(OWLNamedIndividual, OWLNamedIndividual, Set[OWLAxiom])] = {

    val id = (ixnNode \ "@id").head.text
    val ixnID = s"$CTDIXN$id#$taxonIndex"
    val ixnInd = Individual(ixnID)

    val maybeAffectorAndAxioms = handleIxn(ixnNode) match {

      //first check if binding or cotreatment

      case Interaction("w" :: Nil, _, actors) if actors.forall(_.isInstanceOf[AtomicActor]) =>
        val (inputs, inputsAxioms) = actors.collect {
          case actor: AtomicActor => actor.owl(taxonIndex)
        }.unzip
        val cotreatmentAxioms = inputs.map(ixnInd Fact(HasInput, _))
        Some(ixnInd, inputsAxioms.toSet.flatten ++ cotreatmentAxioms ++ Set(ixnInd Type CoTreatment))

      case Interaction("b" :: Nil, _, actors) if actors.forall(_.isInstanceOf[AtomicActor]) =>
        val (inputs, inputsAxioms) = actors.collect {
          case actor: AtomicActor => actor.owl(taxonIndex)
        }.unzip
        val bindingAxioms = inputs.map(ixnInd Fact(HasInput, _))
        Some(ixnInd, inputsAxioms.toSet.flatten ++ bindingAxioms ++ Set(ixnInd Type Binding))

      case Interaction("rxn" :: Nil, _, subject :: Interaction(_, innerNode, _) :: Nil) =>
        val subjectStuff = subject match {
          case atomic: AtomicActor =>
            val (subjInd, subjAxioms) = atomic.owl(taxonIndex)
            val subjProcess = Individual(s"${subjInd.getIRI.toString}-process")
            Some((subjProcess, subjAxioms ++ Set(subjProcess Type Process,
              subjProcess Fact(HasParticipant, subjInd))))
          case ixn: Interaction    => interaction(ixn.node, taxonIndex).map(res => (res._1, res._3))
        }
        for {
          (subjectProcess, subjectAxioms) <- subjectStuff
          (_, innerAffector, innerAxioms) <- interaction(innerNode, taxonIndex)
        } yield {
          val axn = (ixnNode \ "axn").head
          val relation = processToProcess((axn \ "@degreecode").head.text)
          (ixnInd, innerAxioms ++ subjectAxioms ++ Set(
            ixnInd Type Process,
            innerAffector Fact(PartOf, ixnInd),
            subjectProcess Fact(relation, innerAffector)))
        }

      case Interaction(itypes, _, subject :: (target: AtomicActor) :: Nil)
        if itypes.forall(Set("act", "pho", "exp", "myl", "sec", "loc", "clv", "mut", "deg", "spl", "rec", "sta", "met", "oxd", "ubq", "nit", "upt", "red", "alk", "sum", "gyc", "trt", "glc", "csy", "upt", "red", "hdx")) =>
        val subjectStuff = subject match {
          case atomic: AtomicActor =>
            val (subjInd, subjAxioms) = atomic.owl(taxonIndex)
            val subjProcess = Individual(s"${subjInd.getIRI.toString}-process")
            Some((subjProcess, subjAxioms ++ Set(subjProcess Type Process,
              subjProcess Fact(HasParticipant, subjInd))))
          case ixn: Interaction    => interaction(ixn.node, taxonIndex).map(res => (res._1, res._3))
        }
        for {
          (subjectProcess, subjectAxioms) <- subjectStuff
        } yield {
          val axn = (ixnNode \ "axn").head
          val relation = processToProcess((axn \ "@degreecode").head.text)
          val (targetInd, targetAxioms) = target.owl(taxonIndex)
          val axioms = itypes.zipWithIndex.flatMap { case (itype, index) =>
            val ixnType = IxnTypes(itype)
            val localIxnID = s"$CTDIXN$id#$taxonIndex-target-$index"
            val localIxnInd = Individual(localIxnID)
            List(localIxnInd Type ixnType,
              localIxnInd Fact(HasParticipant, targetInd),
              subjectProcess Fact(relation, localIxnInd),
              localIxnInd Fact(PartOf, ixnInd)
            )
          }.toSet
          (ixnInd, axioms ++ subjectAxioms ++ targetAxioms)
        }

      case _ => {
        scribe.warn(s"Not handling:\n$ixnNode")
        None
      } //FIXME

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