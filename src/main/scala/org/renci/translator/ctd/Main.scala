package org.renci.translator.ctd

import java.io.{File, FileInputStream}
import java.util.Properties

import com.bigdata.journal.Options
import com.bigdata.rdf.sail.{BigdataSail, BigdataSailRepository}
import org.openrdf.model.impl.URIImpl
import org.openrdf.rio.helpers.StatementCollector
import org.phenoscape.scowl._
import org.renci.translator.ctd.Model.{Gene, _}
import org.renci.translator.ctd.Vocab._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{IRI, OWLAxiom, OWLNamedIndividual, OWLOntology}
import org.semanticweb.owlapi.rio.RioRenderer

import scala.jdk.CollectionConverters._
import scala.xml.{Elem, Node, XML}

object Main extends App {

  //TODO handle multiple actions

  def process(ixn: Elem): OWLOntology = {
    val id = (ixn \ "@id").head.text
    val graphIRI = IRI.create(s"$CTDIXN$id")
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
    val manager = OWLManager.createOWLOntologyManager()
    manager.createOntology(axioms.asJava, graphIRI)
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

  val root = XML.loadFile(args(0))
  val journalFile = new File(args(1))
  val inputProperties = new File(args(2))
  val blazegraphProperties = new Properties()
  val propertiesStream = new FileInputStream(inputProperties)
  blazegraphProperties.load(propertiesStream)
  propertiesStream.close()
  blazegraphProperties.setProperty(Options.FILE, journalFile.getAbsolutePath)
  val sail = new BigdataSail(blazegraphProperties)
  val repository = new BigdataSailRepository(sail)
  repository.initialize()
  val blazegraph = repository.getUnisolatedConnection()
  val ixns = root \ "ixn"
  blazegraph.begin()
  ixns.foreach { case ixn: Elem =>
    val ont = process(ixn)

    val graph = new URIImpl(ont.getOntologyID.getOntologyIRI.get().toString)
    val collector = new StatementCollector()
    val renderer = new RioRenderer(ont, collector, null)
    renderer.render()
    blazegraph.add(collector.getStatements, graph)
  }
  blazegraph.commit()
  blazegraph.close()
  repository.shutDown()

}