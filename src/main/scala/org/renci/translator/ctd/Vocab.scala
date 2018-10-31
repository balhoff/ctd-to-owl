package org.renci.translator.ctd

import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLObjectProperty
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary

object Vocab {

  private val factory = OWLManager.getOWLDataFactory
  val TopProperty = factory.getOWLTopObjectProperty
  val HasForm = AnnotationProperty("http://ctd.example.org/has_form")
  val DCSource = AnnotationProperty(DublinCoreVocabulary.SOURCE.getIRI)

  val OBO = "http://purl.obolibrary.org/obo"
  val CTDIXN = "http://ctdbase.org/detail.go?type=relationship&ixnId="
  val MESH = "http://id.nlm.nih.gov/mesh"
  val NCBIGENE = "http://identifiers.org/ncbigene"
  val PMID = "https://www.ncbi.nlm.nih.gov/pubmed"
  val ChemicalEntity = Class(s"$OBO/CHEBI_24431")
  val Gene = Class(s"$OBO/SO_0000704")
  val ActsUpstreamOf = ObjectProperty(s"$OBO/RO_0002263") // material entity to process
  val ActsUpstreamOfPositiveEffect = ObjectProperty(s"$OBO/RO_0004034") // material entity to process
  val ActsUpstreamOfNegativeEffect = ObjectProperty(s"$OBO/RO_0004035") // material entity to process
  val CausallyUpstreamOf = ObjectProperty(s"$OBO/RO_0002411") // process to process
  val CausallyUpstreamOfPositiveEffect = ObjectProperty(s"$OBO/RO_0002304") // process to process
  val CausallyUpstreamOfNegativeEffect = ObjectProperty(s"$OBO/RO_0002305") // process to process
  val OccursIn = ObjectProperty(s"$OBO/BFO_0000066")
  val HasInput = ObjectProperty(s"$OBO/RO_0002233")
  val InputOf = ObjectProperty(s"$OBO/RO_0002352")
  val Enables = ObjectProperty(s"$OBO/RO_0002327")
  val EnabledBy = ObjectProperty(s"$OBO/RO_0002333")
  val TransportsOrMaintainsLocalizationOf = ObjectProperty(s"$OBO/RO_0002313")
  val MolecularFunction = Class(s"$OBO/GO_0003674")
  val Transport = Class(s"$OBO/GO_0006810")
  val ImportIntoCell = Class(s"$OBO/GO_0098657")
  val Binding = Class(s"$OBO/GO_0005488")
  val Cleavage = Class(s"$OBO/CTDI_8")
  val Oxidation = Class(s"$OBO/CTDI_20")
  val ChemicalSynthesis = Class(s"$OBO/CTDI_10")
  val Phosphorylation = Class(s"$OBO/GO_0016310")
  val Localization = Class(s"$OBO/GO_0051179")
  val Secretion = Class(s"$OBO/GO_0046903")
  val RNASplicing = Class(s"$OBO/GO_0008380")
  val ProteinOLinkedGlycosylation = Class(s"$OBO/GO_0006493")
  val GeneExpression = Class(s"$OBO/GO_0010467")
  val CoTreatment = Class(s"$OBO/CTDI_26")
  val ResponseToChemical = Class(s"$OBO/GO_0042221")
  val MetabolicProcess = Class(s"$OBO/GO_0008152")
  val Methylation = Class(s"$OBO/GO_0032259")
  val Abundance = Class(s"$OBO/CTDI_1")
  val Process = Class(s"$OBO/BFO_0000015")

  val IxnTypes = Map(
    "exp" -> GeneExpression,
    "w" -> CoTreatment,
    "rec" -> ResponseToChemical,
    "met" -> MetabolicProcess,
    //"rxn" -> MolecularFunction,
    "act" -> MolecularFunction,
    "myl" -> Methylation,
    "upt" -> Transport,
    "imt" -> ImportIntoCell,
    "b" -> Binding,
    "clv" -> Cleavage,
    "oxd" -> Oxidation,
    "csy" -> ChemicalSynthesis,
    "pho" -> Phosphorylation,
    "loc" -> Localization,
    "sec" -> Secretion,
    "spl" -> RNASplicing,
    "ogl" -> ProteinOLinkedGlycosylation)

  def materialToProcess(degree: String): OWLObjectProperty = degree match {
    case "1" => ActsUpstreamOf
    case "0" => TopProperty //TODO
    case "+" => ActsUpstreamOfPositiveEffect
    case "-" => ActsUpstreamOfNegativeEffect
  }

  def processToProcess(degree: String): OWLObjectProperty = degree match {
    case "1" => CausallyUpstreamOf
    case "0" => TopProperty //TODO
    case "+" => CausallyUpstreamOfPositiveEffect
    case "-" => CausallyUpstreamOfNegativeEffect
  }
}