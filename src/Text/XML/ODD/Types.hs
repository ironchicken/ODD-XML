-- ODD-XML - An ODD Parser for Haskell
--
-- Copyright (C) 2015-2017 Richard Lewis
-- Author: Richard Lewis <richard@rjlewis.me.uk>

-- This file is part of ODD-XML

-- ODD-XML is free software: you can redistribute it and/or modify it
-- under the terms of the GNU Lesser General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- ODD-XML is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with ODD-XML.  If not, see <http://www.gnu.org/licenses/>.

module Text.XML.ODD.Types where

import Text.XML.ODD.BootstrapTEI

-- See <http://www.tei-c.org/release/doc/tei-p5-doc/en/html/REF-ELEMENTS.html#tagdocs>

data AltIdent = AltIdent
  { altIdentGlobalAttrs :: GlobalAttrs
  , altIdentGlobalRenditionAttrs :: GlobalRenditionAttrs
  , altIdentGlobalLinkingAttrs :: GlobalLinkingAttrs
  , altIdentGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , altIdentGlobalFacsAttrs :: GlobalFacsAttrs
  , altIdentGlobalChangeAttrs :: GlobalChangeAttrs
  , altIdentGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , altIdentTyped :: TypedAttrs
  }

data Atlernate = Alternate
  { alternateGlobalAttrs :: GlobalAttrs
  , alternateGlobalRenditionAttrs :: GlobalRenditionAttrs
  , alternateGlobalLinkingAttrs :: GlobalLinkingAttrs
  , alternateGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , alternateGlobalFacsAttrs :: GlobalFacsAttrs
  , alternateGlobalChangeAttrs :: GlobalChangeAttrs
  , alternateGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , alternateRepeatableAttrs :: RepeatableAttrs
  }

data Att = Att
  { attGlobalAttrs :: GlobalAttrs
  , attGlobalRenditionAttrs :: GlobalRenditionAttrs
  , attGlobalLinkingAttrs :: GlobalLinkingAttrs
  , attGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , attGlobalFacsAttrs :: GlobalFacsAttrs
  , attGlobalChangeAttrs :: GlobalChangeAttrs
  , attGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , attScheme :: Maybe DataEnumerated
  }

data AttDefUsage
  = Required
  | Recommended
  | Optional
  deriving (Eq, Enum)

data AttDef = AttDef
  { attDefGlobalAttrs :: GlobalAttrs
  , attDefGlobalRenditionAttrs :: GlobalRenditionAttrs
  , attDefGlobalLinkingAttrs :: GlobalLinkingAttrs
  , attDefGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , attDefGlobalFacsAttrs :: GlobalFacsAttrs
  , attDefGlobalChangeAttrs :: GlobalChangeAttrs
  , attDefGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , attDefGlobalIdentifiedAttrs :: GlobalIdentifiedAttrs
  , attDefCombinableAttrs :: CombinableAttrs
  , attDefDeprecatedAttrs :: DeprecatedAttrs
  , attDef_usage :: Maybe AttDefUsage
  , attDef_ns :: Maybe DataNamespace
  }

data AttListOrg
  = Group
  | Choice
  deriving (Eq, Enum)

data AttList = AttList
  { attListGlobalAttrs :: GlobalAttrs
  , attListGlobalRenditionAttrs :: GlobalRenditionAttrs
  , attListGlobalLinkingAttrs :: GlobalLinkingAttrs
  , attListGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , attListGlobalFacsAttrs :: GlobalFacsAttrs
  , attListGlobalChangeAttrs :: GlobalChangeAttrs
  , attListGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , attList_org :: Maybe AttListOrg
  }

data AttRef = AttRef
  { attRefGlobalAttrs :: GlobalAttrs
  , attRefGlobalRenditionAttrs :: GlobalRenditionAttrs
  , attRefGlobalLinkingAttrs :: GlobalLinkingAttrs
  , attRefGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , attRefGlobalFacsAttrs :: GlobalFacsAttrs
  , attRefGlobalChangeAttrs :: GlobalChangeAttrs
  , attRefGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , attRef_class :: Maybe DataToken
  , attRef_name :: Maybe DataText
  }

data ClassRefExpand
  = ExpAlternate
  | ExpSequence
  | ExpSequenceOptional
  | ExpSequenceOptionalRepeatable
  | ExpSequenceRepeatable
  deriving (Eq, Enum)

data ClassRef = ClassRef
  { classRefGlobalAttrs :: GlobalAttrs
  , classRefGlobalRenditionAttrs :: GlobalRenditionAttrs
  , classRefGlobalLinkingAttrs :: GlobalLinkingAttrs
  , classRefGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , classRefGlobalFacsAttrs :: GlobalFacsAttrs
  , classRefGlobalChangeAttrs :: GlobalChangeAttrs
  , classRefGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , classRefRepeatableAttrs :: RepeatableAttrs
  , classRefReadFromAttrs :: ReadFromAttrs
  , classRef_key :: DataXMLName
  , classRef_expand :: Maybe ClassRefExpand
  , classRef_include :: Maybe DataXMLName
  , classRef_except :: Maybe DataXMLName
  }

data ClassSpecType
  = ClassTypeModel
  | ClassTypeAttrs
  deriving (Eq, Enum)

data ClassSpecGenerate
  = GenAlternation
  | GenSequence
  | GenSequenceOptional
  | GenSequenceOptionalRepeatable
  | GenSequenceRepeatable
  deriving (Eq, Enum)

data ClassSpec = ClassSpec
  { classSpecGlobalAttrs :: GlobalAttrs
  , classSpecGlobalRenditionAttrs :: GlobalRenditionAttrs
  , classSpecGlobalLinkingAttrs :: GlobalLinkingAttrs
  , classSpecGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , classSpecGlobalFacsAttrs :: GlobalFacsAttrs
  , classSpecGlobalChangeAttrs :: GlobalChangeAttrs
  , classSpecGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , classSpecGlobalIdentifiedAttrs :: GlobalIdentifiedAttrs
  , classSpecCombinableAttrs :: CombinableAttrs
  , classSpecDeprecatedAttrs :: DeprecatedAttrs
  , classSpec_type :: ClassSpecType
  , classSpec_generate :: Maybe ClassSpecGenerate
  }

data ClassesMode
  = Change
  | Replace
  deriving (Eq, Enum)

data Classes = Classes
  { classesGlobalAttrs :: GlobalAttrs
  , classesGlobalRenditionAttrs :: GlobalRenditionAttrs
  , classesGlobalLinkingAttrs :: GlobalLinkingAttrs
  , classesGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , classesGlobalFacsAttrs :: GlobalFacsAttrs
  , classesGlobalChangeAttrs :: GlobalChangeAttrs
  , classesGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , classesMode :: Maybe ClassesMode
  }

data Code = Code
  { codeGlobalAttrs :: GlobalAttrs
  , codeGlobalRenditionAttrs :: GlobalRenditionAttrs
  , codeGlobalLinkingAttrs :: GlobalLinkingAttrs
  , codeGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , codeGlobalFacsAttrs :: GlobalFacsAttrs
  , codeGlobalChangeAttrs :: GlobalChangeAttrs
  , codeGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , codeLang :: Maybe DataToken
  }

data Constraint = Constraint
  { constraintGlobalAttrs :: GlobalAttrs
  , constraintGlobalRenditionAttrs :: GlobalRenditionAttrs
  , constraintGlobalLinkingAttrs :: GlobalLinkingAttrs
  , constraintGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , constraintGlobalFacsAttrs :: GlobalFacsAttrs
  , constraintGlobalChangeAttrs :: GlobalChangeAttrs
  , constraintGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  }

data ConstraintSpec = ConstraintSpec
  { constraintSpecGlobalAttrs :: GlobalAttrs
  , constraintSpecGlobalRenditionAttrs :: GlobalRenditionAttrs
  , constraintSpecGlobalLinkingAttrs :: GlobalLinkingAttrs
  , constraintSpecGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , constraintSpecGlobalFacsAttrs :: GlobalFacsAttrs
  , constraintSpecGlobalChangeAttrs :: GlobalChangeAttrs
  , constraintSpecGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , constraintSpecGlobalIdentifiedAttrs :: GlobalIdentifiedAttrs
  , constraintSpecCombinableAttrs :: CombinableAttrs
  , constraintSpecDeprecatedAttrs :: DeprecatedAttrs
  , constraintSpecTyped :: TypedAttrs
  , constraintSpec_scheme :: DataEnumerated
  }

data Content = Content
  { contentGlobalAttrs :: GlobalAttrs
  , contentGlobalRenditionAttrs :: GlobalRenditionAttrs
  , contentGlobalLinkingAttrs :: GlobalLinkingAttrs
  , contentGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , contentGlobalFacsAttrs :: GlobalFacsAttrs
  , contentGlobalChangeAttrs :: GlobalChangeAttrs
  , contentGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , content_autoPrefix :: Maybe DataTruth
  }

data DataRef = DataRef
  { dataRefGlobalAttrs :: GlobalAttrs
  , dataRefGlobalRenditionAttrs :: GlobalRenditionAttrs
  , dataRefGlobalLinkingAttrs :: GlobalLinkingAttrs
  , dataRefGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , dataRefGlobalFacsAttrs :: GlobalFacsAttrs
  , dataRefGlobalChangeAttrs :: GlobalChangeAttrs
  , dataRefGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , dataRefReadFromAttrs :: ReadFromAttrs
  , dataRef_key :: Maybe DataXMLName
  , dataRef_name :: Maybe DataXMLName
  , dataRef_ref :: Maybe DataXMLName
  , dataRef_restriction :: Maybe DataRegex
  }

data DataSpec = DataSpec
  { dataSpecGlobalAttrs :: GlobalAttrs
  , dataSpecGlobalRenditionAttrs :: GlobalRenditionAttrs
  , dataSpecGlobalLinkingAttrs :: GlobalLinkingAttrs
  , dataSpecGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , dataSpecGlobalFacsAttrs :: GlobalFacsAttrs
  , dataSpecGlobalChangeAttrs :: GlobalChangeAttrs
  , dataSpecGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , dataSpecGlobalIdentifiedAttrs :: GlobalIdentifiedAttrs
  , dataSpecCombinableAttrs :: CombinableAttrs
  , dataSpecDeprecatedAttrs :: DeprecatedAttrs
  }

data DataType = DataType
  { dataTypeGlobalAttrs :: GlobalAttrs
  , dataTypeGlobalRenditionAttrs :: GlobalRenditionAttrs
  , dataTypeGlobalLinkingAttrs :: GlobalLinkingAttrs
  , dataTypeGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , dataTypeGlobalFacsAttrs :: GlobalFacsAttrs
  , dataTypeGlobalChangeAttrs :: GlobalChangeAttrs
  , dataTypeGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , dataType_minOccurs :: Maybe DataCount
  , dataType_maxOccurs :: Maybe DataUnboundedInt
  }

data DefaultVal = DefaultVal
  { defaultValGlobalAttrs :: GlobalAttrs
  , defaultValGlobalRenditionAttrs :: GlobalRenditionAttrs
  , defaultValGlobalLinkingAttrs :: GlobalLinkingAttrs
  , defaultValGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , defaultValGlobalFacsAttrs :: GlobalFacsAttrs
  , defaultValGlobalChangeAttrs :: GlobalChangeAttrs
  , defaultValGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  }

data Eg = Eg
  { egGlobalAttrs :: GlobalAttrs
  , egGlobalRenditionAttrs :: GlobalRenditionAttrs
  , egGlobalLinkingAttrs :: GlobalLinkingAttrs
  , egGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , egGlobalFacsAttrs :: GlobalFacsAttrs
  , egGlobalChangeAttrs :: GlobalChangeAttrs
  , egGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  }

data EgXMLValid
  = EgXMLValidTrue
  | EgXMLValidFeasible
  | EgXMLValidFalse
  deriving (Eq, Enum)

data EgXML = EgXML
  { egXMLGlobalAttrs :: GlobalAttrs
  , egXMLGlobalRenditionAttrs :: GlobalRenditionAttrs
  , egXMLGlobalLinkingAttrs :: GlobalLinkingAttrs
  , egXMLGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , egXMLGlobalFacsAttrs :: GlobalFacsAttrs
  , egXMLGlobalChangeAttrs :: GlobalChangeAttrs
  , egXMLGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , egXMLSourceAttrs :: SourceAttrs
  , egXML_valid :: Maybe EgXMLValid
  }

data ElementRef = ElementRef
  { elementRefGlobalAttrs :: GlobalAttrs
  , elementRefGlobalRenditionAttrs :: GlobalRenditionAttrs
  , elementRefGlobalLinkingAttrs :: GlobalLinkingAttrs
  , elementRefGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , elementRefGlobalFacsAttrs :: GlobalFacsAttrs
  , elementRefGlobalChangeAttrs :: GlobalChangeAttrs
  , elementRefGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , elementRefRepeatableAttrs :: RepeatableAttrs
  , elementRefReadFromAttrs :: ReadFromAttrs
  , elementRef_key :: DataXMLName
  }

data ElementSpec = ElementSpec
  { elementSpecGlobalAttrs :: GlobalAttrs
  , elementSpecGlobalRenditionAttrs :: GlobalRenditionAttrs
  , elementSpecGlobalLinkingAttrs :: GlobalLinkingAttrs
  , elementSpecGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , elementSpecGlobalFacsAttrs :: GlobalFacsAttrs
  , elementSpecGlobalChangeAttrs :: GlobalChangeAttrs
  , elementSpecGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , elementSpecGlobalIdentifiedAttrs :: GlobalIdentifiedAttrs
  , elementSpecCombinableAttrs :: CombinableAttrs
  , elementSpecDeprecatedAttrs :: DeprecatedAttrs
  , elementSpecGlobalNamespaceableAttrs :: GlobalNamespaceableAttrs
  , elementSpec_prefix :: DataXMLName
  }

data Equiv = Equiv
  { equivGlobalAttrs :: GlobalAttrs
  , equivGlobalRenditionAttrs :: GlobalRenditionAttrs
  , equivGlobalLinkingAttrs :: GlobalLinkingAttrs
  , equivGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , equivGlobalFacsAttrs :: GlobalFacsAttrs
  , equivGlobalChangeAttrs :: GlobalChangeAttrs
  , equivGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , equiv_name :: DataName
  , equiv_uri :: DataPointer
  , equiv_filter :: DataPointer
  }

data Exemplum = Exemplum
  { exemplumGlobalAttrs :: GlobalAttrs
  , exemplumGlobalRenditionAttrs :: GlobalRenditionAttrs
  , exemplumGlobalLinkingAttrs :: GlobalLinkingAttrs
  , exemplumGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , exemplumGlobalFacsAttrs :: GlobalFacsAttrs
  , exemplumGlobalChangeAttrs :: GlobalChangeAttrs
  , exemplumGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , exemplumTyped :: TypedAttrs
  , exemplumTranslatable :: TranslatableAttrs
  }

data Gi = Gi
  { giGlobalAttrs :: GlobalAttrs
  , giGlobalRenditionAttrs :: GlobalRenditionAttrs
  , giGlobalLinkingAttrs :: GlobalLinkingAttrs
  , giGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , giGlobalFacsAttrs :: GlobalFacsAttrs
  , giGlobalChangeAttrs :: GlobalChangeAttrs
  , giGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , gi_scheme :: DataEnumerated
  }

data Ident = Ident
  { identGlobalAttrs :: GlobalAttrs
  , identGlobalRenditionAttrs :: GlobalRenditionAttrs
  , identGlobalLinkingAttrs :: GlobalLinkingAttrs
  , identGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , identGlobalFacsAttrs :: GlobalFacsAttrs
  , identGlobalChangeAttrs :: GlobalChangeAttrs
  , identGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , identTyped :: TypedAttrs
  }

data ListRef = ListRef
  { listRefGlobalAttrs :: GlobalAttrs
  , listRefGlobalRenditionAttrs :: GlobalRenditionAttrs
  , listRefGlobalLinkingAttrs :: GlobalLinkingAttrs
  , listRefGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , listRefGlobalFacsAttrs :: GlobalFacsAttrs
  , listRefGlobalChangeAttrs :: GlobalChangeAttrs
  , listRefGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  }

data MacroRef = MacroRef
  { macroRefGlobalAttrs :: GlobalAttrs
  , macroRefGlobalRenditionAttrs :: GlobalRenditionAttrs
  , macroRefGlobalLinkingAttrs :: GlobalLinkingAttrs
  , macroRefGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , macroRefGlobalFacsAttrs :: GlobalFacsAttrs
  , macroRefGlobalChangeAttrs :: GlobalChangeAttrs
  , macroRefGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , macroRefReadFromAttrs :: ReadFromAttrs
  , macroRef_key :: DataXMLName
  }

data MacroSpecType
  = ParameterEntity
  | DataTypeEntity
  deriving (Eq, Enum)

data MacroSpec = MacroSpec
  { macroSpecGlobalAttrs :: GlobalAttrs
  , macroSpecGlobalRenditionAttrs :: GlobalRenditionAttrs
  , macroSpecGlobalLinkingAttrs :: GlobalLinkingAttrs
  , macroSpecGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , macroSpecGlobalFacsAttrs :: GlobalFacsAttrs
  , macroSpecGlobalChangeAttrs :: GlobalChangeAttrs
  , macroSpecGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , macroSpecGlobalIdentifiedAttrs :: GlobalIdentifiedAttrs
  , macroSpecCombinableAttrs :: CombinableAttrs
  , macroSpecDeprecatedAttrs :: DeprecatedAttrs
  , macroSpec_type :: Maybe MacroSpecType
  }

data MemberOfMode
  = MemberOfAdd
  | MemberOfDelete
  deriving (Eq, Enum)

data MemberOf = MemberOf
  { memberOfGlobalAttrs :: GlobalAttrs
  , memberOfGlobalRenditionAttrs :: GlobalRenditionAttrs
  , memberOfGlobalLinkingAttrs :: GlobalLinkingAttrs
  , memberOfGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , memberOfGlobalFacsAttrs :: GlobalFacsAttrs
  , memberOfGlobalChangeAttrs :: GlobalChangeAttrs
  , memberOfGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , memberOf_key :: Maybe DataName
  , memberOf_mode :: Maybe MemberOfMode
  , memberOf_max :: Maybe DataNumeric
  , memberOF_min :: Maybe DataNumeric
  }

data Model = Model
  { modelGlobalAttrs :: GlobalAttrs
  , modelGlobalRenditionAttrs :: GlobalRenditionAttrs
  , modelGlobalLinkingAttrs :: GlobalLinkingAttrs
  , modelGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , modelGlobalFacsAttrs :: GlobalFacsAttrs
  , modelGlobalChangeAttrs :: GlobalChangeAttrs
  , modelGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , model_behaviour :: DataEnumerated
  , model_predicate :: Maybe DataXPath
  , model_useSourceRendition :: Maybe DataTruth
  , model_output :: Maybe DataEnumerated
  , model_cssClass :: Maybe [DataName]
  }

data ModelGrp = ModelGrp
  { modelGrpGlobalAttrs :: GlobalAttrs
  , modelGrpGlobalRenditionAttrs :: GlobalRenditionAttrs
  , modelGrpGlobalLinkingAttrs :: GlobalLinkingAttrs
  , modelGrpGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , modelGrpGlobalFacsAttrs :: GlobalFacsAttrs
  , modelGrpGlobalChangeAttrs :: GlobalChangeAttrs
  , modelGrpGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , modelGrp_useSourceRendition :: Maybe DataTruth
  , modelGrp_output :: Maybe DataEnumerated
  }

data ModelSequence = ModelSequence
  { modelSequenceGrpGlobalAttrs :: GlobalAttrs
  , modelSequenceGrpGlobalRenditionAttrs :: GlobalRenditionAttrs
  , modelSequenceGrpGlobalLinkingAttrs :: GlobalLinkingAttrs
  , modelSequenceGrpGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , modelSequenceGrpGlobalFacsAttrs :: GlobalFacsAttrs
  , modelSequenceGrpGlobalChangeAttrs :: GlobalChangeAttrs
  , modelSequenceGrpGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , modelSequence_predicate :: Maybe DataXPath
  , modelSequence_useSourceRendition :: Maybe DataTruth
  , modelSequence_output :: Maybe DataEnumerated
  }

data ModuleRef = ModuleRef
  { moduleRefGlobalAttrs :: GlobalAttrs
  , moduleRefGlobalRenditionAttrs :: GlobalRenditionAttrs
  , moduleRefGlobalLinkingAttrs :: GlobalLinkingAttrs
  , moduleRefGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , moduleRefGlobalFacsAttrs :: GlobalFacsAttrs
  , moduleRefGlobalChangeAttrs :: GlobalChangeAttrs
  , moduleRefGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , moduleRefReadFromAttrs :: ReadFromAttrs
  , moduleRef_prefix :: Maybe DataXMLName
  , moduleRef_include :: Maybe [DataXMLName]
  , moduleRef_except :: Maybe [DataXMLName]
  , moduleRef_key :: Maybe DataXMLName
  , moduleRef_url :: Maybe DataPointer
  }

data ModuleSpec = ModuleSpec
  { moduleSpecGlobalAttrs :: GlobalAttrs
  , moduleSpecGlobalRenditionAttrs :: GlobalRenditionAttrs
  , moduleSpecGlobalLinkingAttrs :: GlobalLinkingAttrs
  , moduleSpecGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , moduleSpecGlobalFacsAttrs :: GlobalFacsAttrs
  , moduleSpecGlobalChangeAttrs :: GlobalChangeAttrs
  , moduleSpecGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , moduleSpecGlobalIdentifiedAttrs :: GlobalIdentifiedAttrs
  , moduleSpecCombinableAttrs :: CombinableAttrs
  , moduleSpecDeprecatedAttrs :: DeprecatedAttrs
  , moduleSpecTypedAttrs :: TypedAttrs
  }

data OutputRendition = OutputRendition
  { outputRenditionGlobalAttrs :: GlobalAttrs
  , outputRenditionGlobalRenditionAttrs :: GlobalRenditionAttrs
  , outputRenditionGlobalLinkingAttrs :: GlobalLinkingAttrs
  , outputRenditionGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , outputRenditionGlobalFacsAttrs :: GlobalFacsAttrs
  , outputRenditionGlobalChangeAttrs :: GlobalChangeAttrs
  , outputRenditionGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , outputRendition_scope :: Maybe DataText
  }

data Param = Param
  { paramGlobalAttrs :: GlobalAttrs
  , paramGlobalRenditionAttrs :: GlobalRenditionAttrs
  , paramGlobalLinkingAttrs :: GlobalLinkingAttrs
  , paramGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , paramGlobalFacsAttrs :: GlobalFacsAttrs
  , paramGlobalChangeAttrs :: GlobalChangeAttrs
  , paramGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , param_name :: DataXPath -- FIXME This is the documented type, but it's probably wrong
  , param_value :: DataXPath
  }

data ParamList = ParamList
  { paramListGlobalAttrs :: GlobalAttrs
  , paramListGlobalRenditionAttrs :: GlobalRenditionAttrs
  , paramListGlobalLinkingAttrs :: GlobalLinkingAttrs
  , paramListGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , paramListGlobalFacsAttrs :: GlobalFacsAttrs
  , paramListGlobalChangeAttrs :: GlobalChangeAttrs
  , paramListGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  }

data ParamSpec = ParamSpec
  { paramSpecGlobalAttrs :: GlobalAttrs
  , paramSpecGlobalRenditionAttrs :: GlobalRenditionAttrs
  , paramSpecGlobalLinkingAttrs :: GlobalLinkingAttrs
  , paramSpecGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , paramSpecGlobalFacsAttrs :: GlobalFacsAttrs
  , paramSpecGlobalChangeAttrs :: GlobalChangeAttrs
  , paramSpecGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , paramSpecGlobalIdentifiedAttrs :: GlobalIdentifiedAttrs
  , paramSpecCombinableAttrs :: CombinableAttrs
  , paramSpecDeprecatedAttrs :: DeprecatedAttrs
  }

data Remarks = Remarks
  { remarksGlobalAttrs :: GlobalAttrs
  , remarksGlobalRenditionAttrs :: GlobalRenditionAttrs
  , remarksGlobalLinkingAttrs :: GlobalLinkingAttrs
  , remarksGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , remarksGlobalFacsAttrs :: GlobalFacsAttrs
  , remarksGlobalChangeAttrs :: GlobalChangeAttrs
  , remarksGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , remarksTranslatable :: TranslatableAttrs
  }

data SchemaSpec = SchemaSpec
  { schemaSpecGlobalAttrs :: GlobalAttrs
  , schemaSpecGlobalRenditionAttrs :: GlobalRenditionAttrs
  , schemaSpecGlobalLinkingAttrs :: GlobalLinkingAttrs
  , schemaSpecGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , schemaSpecGlobalFacsAttrs :: GlobalFacsAttrs
  , schemaSpecGlobalChangeAttrs :: GlobalChangeAttrs
  , schemaSpecGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , schemaSpecGlobalIdentifiedAttrs :: GlobalIdentifiedAttrs
  , schemaSpecCombinableAttrs :: CombinableAttrs
  , schemaSpecDeprecatedAttrs :: DeprecatedAttrs
  , schemaSpecGlobalNamespaceableAttrs :: GlobalNamespaceableAttrs
  , schemaSpecReadFromAttrs :: ReadFromAttrs
  , schemaSpec_start :: DataName
  , schemaSpec_prefix :: DataXMLName
  , schemaSpec_targetLang :: DataLanguage
  , schemaSpec_docLang :: DataLanguage
  }

data Sequence = Sequence
  { sequenceGlobalAttrs :: GlobalAttrs
  , sequenceGlobalRenditionAttrs :: GlobalRenditionAttrs
  , sequenceGlobalLinkingAttrs :: GlobalLinkingAttrs
  , sequenceGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , sequenceGlobalFacsAttrs :: GlobalFacsAttrs
  , sequenceGlobalChangeAttrs :: GlobalChangeAttrs
  , sequenceGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , sequenceRepeatableAttrs :: RepeatableAttrs
  , sequence_preserveOrder :: Maybe DataTruth
  }

data SpecDesc = SpecDesc
  { specDescGlobalAttrs :: GlobalAttrs
  , specDescGlobalRenditionAttrs :: GlobalRenditionAttrs
  , specDescGlobalLinkingAttrs :: GlobalLinkingAttrs
  , specDescGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , specDescGlobalFacsAttrs :: GlobalFacsAttrs
  , specDescGlobalChangeAttrs :: GlobalChangeAttrs
  , specDescGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , specDesc_key :: Maybe DataName
  , specDesc_atts :: Maybe [DataName]
  }

data SpecGrp = SpecGrp
  { specGrpGlobalAttrs :: GlobalAttrs
  , specGrpGlobalRenditionAttrs :: GlobalRenditionAttrs
  , specGrpGlobalLinkingAttrs :: GlobalLinkingAttrs
  , specGrpGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , specGrpGlobalFacsAttrs :: GlobalFacsAttrs
  , specGrpGlobalChangeAttrs :: GlobalChangeAttrs
  , specGrpGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  }

data SpecGrpRef = SpecGrpRef
  { specGrpRefGlobalAttrs :: GlobalAttrs
  , specGrpRefGlobalRenditionAttrs :: GlobalRenditionAttrs
  , specGrpRefGlobalLinkingAttrs :: GlobalLinkingAttrs
  , specGrpRefGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , specGrpRefGlobalFacsAttrs :: GlobalFacsAttrs
  , specGrpRefGlobalChangeAttrs :: GlobalChangeAttrs
  , specGrpRefGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , specGrpRef_target :: DataPointer
  }

data SpecList = SpecList
  { specListRefGlobalAttrs :: GlobalAttrs
  , specListRefGlobalRenditionAttrs :: GlobalRenditionAttrs
  , specListRefGlobalLinkingAttrs :: GlobalLinkingAttrs
  , specListRefGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , specListRefGlobalFacsAttrs :: GlobalFacsAttrs
  , specListRefGlobalChangeAttrs :: GlobalChangeAttrs
  , specListRefGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  }

data TagType
  = TagStart
  | TagEnd
  | TagEmpty
  | TagPI
  | TagComment
  | TagMS
  deriving (Eq, Enum)

data Tag = Tag
  { tagRefGlobalAttrs :: GlobalAttrs
  , tagRefGlobalRenditionAttrs :: GlobalRenditionAttrs
  , tagRefGlobalLinkingAttrs :: GlobalLinkingAttrs
  , tagRefGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , tagRefGlobalFacsAttrs :: GlobalFacsAttrs
  , tagRefGlobalChangeAttrs :: GlobalChangeAttrs
  , tagRefGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , tag_type :: Maybe TagType
  , tag_scheme :: Maybe DataEnumerated
  }

data TextNode = TextNode
  { textNodeGlobalAttrs :: GlobalAttrs
  , textNodeGlobalRenditionAttrs :: GlobalRenditionAttrs
  , textNodeGlobalLinkingAttrs :: GlobalLinkingAttrs
  , textNodeGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , textNodeGlobalFacsAttrs :: GlobalFacsAttrs
  , textNodeGlobalChangeAttrs :: GlobalChangeAttrs
  , textNodeGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  }

data Val = Val
  { valGlobalAttrs :: GlobalAttrs
  , valGlobalRenditionAttrs :: GlobalRenditionAttrs
  , valGlobalLinkingAttrs :: GlobalLinkingAttrs
  , valGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , valGlobalFacsAttrs :: GlobalFacsAttrs
  , valGlobalChangeAttrs :: GlobalChangeAttrs
  , valTranslatable :: TranslatableAttrs
  }

data ValDesc = ValDesc
  { valDescGlobalAttrs :: GlobalAttrs
  , valDescGlobalRenditionAttrs :: GlobalRenditionAttrs
  , valDescGlobalLinkingAttrs :: GlobalLinkingAttrs
  , valDescGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , valDescGlobalFacsAttrs :: GlobalFacsAttrs
  , valDescGlobalChangeAttrs :: GlobalChangeAttrs
  , valDescCombinableAttrs :: CombinableAttrs
  , valDescTranslatable :: TranslatableAttrs
  , valDescDeprecatedAttrs :: DeprecatedAttrs
  , valDesc_usage :: Maybe AttDefUsage
  }

data ValItem = ValItem
  { valItemGlobalAttrs :: GlobalAttrs
  , valItemGlobalRenditionAttrs :: GlobalRenditionAttrs
  , valItemGlobalLinkingAttrs :: GlobalLinkingAttrs
  , valItemGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , valItemGlobalFacsAttrs :: GlobalFacsAttrs
  , valItemGlobalChangeAttrs :: GlobalChangeAttrs
  , valItemGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , valItemCombinableAttrs :: CombinableAttrs
  , valItemDeprecatedAttrs :: DeprecatedAttrs
  }

data ValList = ValList
  { valListGlobalAttrs :: GlobalAttrs
  , valListGlobalRenditionAttrs :: GlobalRenditionAttrs
  , valListGlobalLinkingAttrs :: GlobalLinkingAttrs
  , valListGlobalAnalyticAttrs :: GlobalAnalyticAttrs
  , valListGlobalFacsAttrs :: GlobalFacsAttrs
  , valListGlobalChangeAttrs :: GlobalChangeAttrs
  , valListGlobalResponsibilityAttrs :: GlobalResponsibilityAttrs
  , valListCombinableAttrs :: CombinableAttrs
  , valListDeprecatedAttrs :: DeprecatedAttrs
  }
