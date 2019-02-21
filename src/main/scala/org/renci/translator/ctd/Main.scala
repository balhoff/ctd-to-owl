package org.renci.translator.ctd

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.geneontology.whelk.{AtomicConcept, Bridge, ConceptAssertion, ConceptInclusion, Reasoner, ReasonerState, Role, RoleAssertion, Individual => WIndividual}
import org.phenoscape.scowl._
import org.renci.translator.ctd.Model.{Gene, _}
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
          case atomic: AtomicActor => Some(atomic.owl(taxonIndex))
          case ixn: Interaction    => interaction(ixn.node, taxonIndex).map(res => (res._1, res._3))
        }
        for {
          (subjectInd, subjectAxioms) <- subjectStuff
          (_, innerAffector, innerAxioms) <- interaction(innerNode, taxonIndex)
        } yield {
          val axn = (ixnNode \ "axn").head
          val relation = processToProcess((axn \ "@degreecode").head.text)
          (ixnInd, innerAxioms ++ subjectAxioms ++ Set(
            ixnInd Type Process,
            ixnInd Fact(HasParticipant, subjectInd),
            ixnInd Fact(relation, innerAffector)))
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
            val localIxnID = s"$CTDIXN$id#$taxonIndex-$index"
            val localIxnInd = Individual(localIxnID)
            List(localIxnInd Type ixnType,
              localIxnInd Fact(HasParticipant, targetInd),
              subjectProcess Fact(relation, localIxnInd),
              localIxnInd Fact(PartOf, ixnInd)
            )
          }.toSet
          (ixnInd, axioms ++ subjectAxioms ++ targetAxioms)
        }


      //      case Interaction(itype :: Nil, _, Interaction("w" :: Nil, cotreatmentNode, chemicals) :: (targetActor: AtomicActor) :: Nil)
      //        if chemicals.forall(_.isInstanceOf[AtomicActor]) &&
      //          Set("act", "pho", "exp", "myl", "sec", "loc", "clv", "mut", "deg", "spl", "rec", "sta", "met", "oxd", "ubq", "nit", "upt", "red", "alk", "sum", "gyc", "trt", "glc", "csy", "upt", "red", "hdx")(itype) =>
      //        val (inputs, inputsAxioms) = chemicals.collect {
      //          case actor: AtomicActor => actor.owl(taxonIndex)
      //        }.unzip
      //        val cotreatmentID = (cotreatmentNode \ "@id").head.text
      //        val cotreatmentIRI = s"$CTDIXN$cotreatmentID"
      //        val cotreatmentInd = Individual(cotreatmentIRI)
      //        val cotreatmentAxioms = inputs.map(cotreatmentInd Fact(HasInput, _))
      //        val (targetInd, targetAxioms) = targetActor.owl(taxonIndex)
      //        val axn = (ixnNode \ "axn").head
      //        val relation = processToProcess((axn \ "@degreecode").head.text)
      //        val ixnType = IxnTypes(itype)
      //        Some(cotreatmentInd, inputsAxioms.toSet.flatten ++ targetAxioms ++ cotreatmentAxioms ++ Set(
      //          ixnInd Type ixnType,
      //          cotreatmentInd Type CoTreatment,
      //          cotreatmentInd Fact(relation, ixnInd),
      //          ixnInd Fact(HasParticipant, targetInd)))
      //
      //      case Interaction(itype :: Nil, _, Interaction("b" :: Nil, bindingNode, boundThings) :: (targetActor: AtomicActor) :: Nil)
      //        if boundThings.forall(_.isInstanceOf[AtomicActor]) &&
      //          Set("act", "pho", "exp", "myl", "sec", "loc", "clv", "mut", "deg", "spl", "rec", "sta", "met", "oxd", "ubq", "nit", "upt", "red", "alk", "sum", "gyc", "trt", "glc", "csy", "upt", "red", "hdx")(itype) =>
      //        val (inputs, inputsAxioms) = boundThings.collect {
      //          case actor: AtomicActor => actor.owl(taxonIndex)
      //        }.unzip
      //        val bindingID = (bindingNode \ "@id").head.text
      //        val bindingIRI = s"$CTDIXN$bindingID"
      //        val bindingInd = Individual(bindingIRI)
      //        val bindingAxioms = inputs.map(bindingInd Fact(HasInput, _))
      //        val (targetInd, targetAxioms) = targetActor.owl(taxonIndex)
      //        val axn = (ixnNode \ "axn").head
      //        val relation = processToProcess((axn \ "@degreecode").head.text)
      //        val ixnType = IxnTypes(itype)
      //        Some(bindingInd, inputsAxioms.toSet.flatten ++ targetAxioms ++ bindingAxioms ++ Set(
      //          ixnInd Type ixnType,
      //          bindingInd Type Binding,
      //          bindingInd Fact(relation, ixnInd),
      //          ixnInd Fact(HasParticipant, targetInd)))
      //
      //      case Interaction("exp" :: Nil, _, (chem: Chemical) :: (gene: Gene) :: Nil) =>
      //        val (chemInd, chemAxioms) = chem.owl(taxonIndex)
      //        val chemProcess = Individual(s"${chemInd.getIRI.toString}-process")
      //        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
      //        val axn = (ixnNode \ "axn").head
      //        val relation = processToProcess((axn \ "@degreecode").head.text)
      //        Some(chemProcess, chemAxioms ++ geneAxioms ++ Set(
      //          ixnInd Type GeneExpression,
      //          chemProcess Type Process,
      //          chemProcess Fact(EnabledBy, chemInd),
      //          chemProcess Fact(relation, ixnInd),
      //          ixnInd Fact(HasInput, geneInd)))
      //
      //      case Interaction("exp" :: Nil, _, (affectingGene: Gene) :: (gene: Gene) :: Nil) =>
      //        val (affectingGeneInd, affectingGeneAxioms) = affectingGene.owl(taxonIndex)
      //        val affectingGeneFunction = Individual(s"${affectingGeneInd.getIRI.toString}-function")
      //        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
      //        val axn = (ixnNode \ "axn").head
      //        val relation = processToProcess((axn \ "@degreecode").head.text)
      //        Some(affectingGeneFunction, affectingGeneAxioms ++ geneAxioms ++ Set(
      //          ixnInd Type GeneExpression,
      //          affectingGeneFunction Type Process,
      //          affectingGeneFunction Fact(EnabledBy, affectingGeneInd),
      //          affectingGeneFunction Fact(relation, ixnInd),
      //          ixnInd Fact(HasInput, geneInd)))
      //
      //      case Interaction("myl" :: Nil, _, (chem: Chemical) :: (gene: Gene) :: Nil) =>
      //        val (chemInd, chemAxioms) = chem.owl(taxonIndex)
      //        val chemProcess = Individual(s"${chemInd.getIRI.toString}-process")
      //        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
      //        val axn = (ixnNode \ "axn").head
      //        val relation = processToProcess((axn \ "@degreecode").head.text)
      //        Some(chemProcess, chemAxioms ++ geneAxioms ++ Set(
      //          ixnInd Type Methylation,
      //          chemProcess Type Process,
      //          chemProcess Fact(EnabledBy, chemInd),
      //          chemProcess Fact(relation, ixnInd),
      //          ixnInd Fact(HasInput, geneInd)))
      //
      //      case Interaction("act" :: Nil, _, (chem: Chemical) :: (gene: Gene) :: Nil) =>
      //        val (chemInd, chemAxioms) = chem.owl(taxonIndex)
      //        val chemProcess = Individual(s"${chemInd.getIRI.toString}-process")
      //        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
      //        val axn = (ixnNode \ "axn").head
      //        val relation = processToProcess((axn \ "@degreecode").head.text)
      //        Some(chemProcess, chemAxioms ++ geneAxioms ++ Set(
      //          ixnInd Type MolecularFunction,
      //          chemProcess Type Process,
      //          chemProcess Fact(EnabledBy, chemInd),
      //          chemProcess Fact(relation, ixnInd),
      //          ixnInd Fact(EnabledBy, geneInd)))
      //
      //      case Interaction("rec" :: Nil, _, (gene: Gene) :: (chem: Chemical) :: Nil) =>
      //        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
      //        val (chemInd, chemAxioms) = chem.owl(taxonIndex)
      //        val geneFunction = Individual(s"${geneInd.getIRI.toString}-function")
      //        val axn = (ixnNode \ "axn").head
      //        val relation = processToProcess((axn \ "@degreecode").head.text)
      //        Some(geneFunction, chemAxioms ++ geneAxioms ++ Set(
      //          ixnInd Type ResponseToChemical,
      //          geneFunction Type MolecularFunction,
      //          geneFunction Fact(EnabledBy, geneInd),
      //          geneFunction Fact(relation, ixnInd),
      //          ixnInd Fact(HasInput, chemInd)))
      //
      //      case Interaction("pho" :: Nil, _, (chem: Chemical) :: (gene: Gene) :: Nil) =>
      //        val (chemInd, chemAxioms) = chem.owl(taxonIndex)
      //        val chemProcess = Individual(s"${chemInd.getIRI.toString}-process")
      //        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
      //        val axn = (ixnNode \ "axn").head
      //        val relation = processToProcess((axn \ "@degreecode").head.text)
      //        Some(chemProcess, chemAxioms ++ geneAxioms ++ Set(
      //          ixnInd Type Phosphorylation,
      //          chemProcess Type Process,
      //          chemProcess Fact(EnabledBy, chemInd),
      //          chemProcess Fact(relation, ixnInd),
      //          ixnInd Fact(HasInput, geneInd)))
      //
      //      case Interaction("b" :: Nil, _, (chem: Chemical) :: (gene: Gene) :: Nil) =>
      //        val (chemInd, chemAxioms) = chem.owl(taxonIndex)
      //        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
      //        Some(ixnInd, chemAxioms ++ geneAxioms ++ Set(
      //          ixnInd Type Binding,
      //          ixnInd Fact(EnabledBy, chemInd), //FIXME GO molecular functions (binding) shouldn't be enabled by chemicals
      //          ixnInd Fact(HasInput, geneInd)))
      //
      //      case Interaction(itype :: Nil, _, (chem: Chemical) :: (gene: Gene) :: Nil)
      //        if Set("sec", "loc", "clv", "mut", "deg", "spl", "rec", "sta", "met", "oxd", "ubq", "nit", "upt", "red", "alk", "sum", "gyc")(itype) =>
      //        val (chemInd, chemAxioms) = chem.owl(taxonIndex)
      //        val chemProcess = Individual(s"${chemInd.getIRI.toString}-process")
      //        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
      //        val axn = (ixnNode \ "axn").head
      //        val relation = processToProcess((axn \ "@degreecode").head.text)
      //        val ixnType = IxnTypes(itype)
      //        Some(chemProcess, chemAxioms ++ geneAxioms ++ Set(
      //          ixnInd Type ixnType,
      //          chemProcess Type Process,
      //          chemProcess Fact(EnabledBy, chemInd),
      //          chemProcess Fact(relation, ixnInd),
      //          ixnInd Fact(HasInput, geneInd)))
      //
      //      case Interaction(itype :: Nil, _, (gene: Gene) :: (chem: Chemical) :: Nil)
      //        if Set("met", "trt", "glc", "csy", "upt", "oxd", "sec", "red", "hdx")(itype) =>
      //        //TODO no "abu", "b", "act" for now - different model?
      //        val (geneInd, geneAxioms) = gene.owl(taxonIndex)
      //        val (chemInd, chemAxioms) = chem.owl(taxonIndex)
      //        val geneFunction = Individual(s"${geneInd.getIRI.toString}-function")
      //        val axn = (ixnNode \ "axn").head
      //        val relation = processToProcess((axn \ "@degreecode").head.text)
      //        val ixnType = IxnTypes(itype)
      //        Some(geneFunction, chemAxioms ++ geneAxioms ++ Set(
      //          ixnInd Type ixnType,
      //          geneFunction Type MolecularFunction,
      //          geneFunction Fact(EnabledBy, geneInd),
      //          geneFunction Fact(relation, ixnInd),
      //          ixnInd Fact(HasInput, chemInd)))

      case _ => {
        logger.warn(s"Not handling:\n$ixnNode")
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