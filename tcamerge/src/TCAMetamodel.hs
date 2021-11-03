{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module TCAMetamodel
  ( module TCAMetamodel
  ) where

import qualified GHC.Generics as GHCG

import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import Ecore

 
deriving instance GHCG.Generic Xsd.XsdString
deriving instance GHCG.Generic Xsd.AnyURI
deriving instance GHCG.Generic Xsd.Float
deriving instance GHCG.Generic Xsd.Double

-- Some hs-boot imports are required, for fwd-declaring types.
 
elementIIdentifiable :: XMLParser IIdentifiable
elementIIdentifiable = parseSchemaType "IIdentifiable"
elementToXMLIIdentifiable :: IIdentifiable -> [Content ()]
elementToXMLIIdentifiable = schemaTypeToXML "IIdentifiable"
 
elementIAssumable :: XMLParser IAssumable
elementIAssumable = parseSchemaType "IAssumable"
elementToXMLIAssumable :: IAssumable -> [Content ()]
elementToXMLIAssumable = schemaTypeToXML "IAssumable"
 
elementIVirtualizable :: XMLParser IVirtualizable
elementIVirtualizable = parseSchemaType "IVirtualizable"
elementToXMLIVirtualizable :: IVirtualizable -> [Content ()]
elementToXMLIVirtualizable = schemaTypeToXML "IVirtualizable"
 
elementIDeactivatable :: XMLParser IDeactivatable
elementIDeactivatable = parseSchemaType "IDeactivatable"
elementToXMLIDeactivatable :: IDeactivatable -> [Content ()]
elementToXMLIDeactivatable = schemaTypeToXML "IDeactivatable"
 
elementIImpactable :: XMLParser IImpactable
elementIImpactable = parseSchemaType "IImpactable"
elementToXMLIImpactable :: IImpactable -> [Content ()]
elementToXMLIImpactable = schemaTypeToXML "IImpactable"
 
elementIToolChainElement :: XMLParser IToolChainElement
elementIToolChainElement = parseSchemaType "IToolChainElement"
elementToXMLIToolChainElement :: IToolChainElement -> [Content ()]
elementToXMLIToolChainElement = schemaTypeToXML "IToolChainElement"
 
elementTool :: XMLParser Tool
elementTool = parseSchemaType "Tool"
elementToXMLTool :: Tool -> [Content ()]
elementToXMLTool = schemaTypeToXML "Tool"
 
elementUseCase :: XMLParser UseCase
elementUseCase = parseSchemaType "UseCase"
elementToXMLUseCase :: UseCase -> [Content ()]
elementToXMLUseCase = schemaTypeToXML "UseCase"
 
elementArtifact :: XMLParser Artifact
elementArtifact = parseSchemaType "Artifact"
elementToXMLArtifact :: Artifact -> [Content ()]
elementToXMLArtifact = schemaTypeToXML "Artifact"
 
elementError :: XMLParser Error
elementError = parseSchemaType "Error"
elementToXMLError :: Error -> [Content ()]
elementToXMLError = schemaTypeToXML "Error"
 
elementCheck :: XMLParser Check
elementCheck = parseSchemaType "Check"
elementToXMLCheck :: Check -> [Content ()]
elementToXMLCheck = schemaTypeToXML "Check"
 
elementToolChain :: XMLParser ToolChain
elementToolChain = parseSchemaType "ToolChain"
elementToXMLToolChain :: ToolChain -> [Content ()]
elementToXMLToolChain = schemaTypeToXML "ToolChain"
 
elementRestriction :: XMLParser Restriction
elementRestriction = parseSchemaType "Restriction"
elementToXMLRestriction :: Restriction -> [Content ()]
elementToXMLRestriction = schemaTypeToXML "Restriction"
 
elementCosts :: XMLParser Costs
elementCosts = parseSchemaType "Costs"
elementToXMLCosts :: Costs -> [Content ()]
elementToXMLCosts = schemaTypeToXML "Costs"
 
elementToolCosts :: XMLParser ToolCosts
elementToolCosts = parseSchemaType "ToolCosts"
elementToXMLToolCosts :: ToolCosts -> [Content ()]
elementToXMLToolCosts = schemaTypeToXML "ToolCosts"
 
elementUseCaseCosts :: XMLParser UseCaseCosts
elementUseCaseCosts = parseSchemaType "UseCaseCosts"
elementToXMLUseCaseCosts :: UseCaseCosts -> [Content ()]
elementToXMLUseCaseCosts = schemaTypeToXML "UseCaseCosts"
 
elementQualification :: XMLParser Qualification
elementQualification = parseSchemaType "Qualification"
elementToXMLQualification :: Qualification -> [Content ()]
elementToXMLQualification = schemaTypeToXML "Qualification"
 
elementFeature :: XMLParser Feature
elementFeature = parseSchemaType "Feature"
elementToXMLFeature :: Feature -> [Content ()]
elementToXMLFeature = schemaTypeToXML "Feature"
 
elementInferredRestriction :: XMLParser InferredRestriction
elementInferredRestriction = parseSchemaType "InferredRestriction"
elementToXMLInferredRestriction :: InferredRestriction -> [Content ()]
elementToXMLInferredRestriction = schemaTypeToXML "InferredRestriction"
 
elementInferredError :: XMLParser InferredError
elementInferredError = parseSchemaType "InferredError"
elementToXMLInferredError :: InferredError -> [Content ()]
elementToXMLInferredError = schemaTypeToXML "InferredError"
 
elementInferredCheck :: XMLParser InferredCheck
elementInferredCheck = parseSchemaType "InferredCheck"
elementToXMLInferredCheck :: InferredCheck -> [Content ()]
elementToXMLInferredCheck = schemaTypeToXML "InferredCheck"
 
elementAttribute :: XMLParser Attribute
elementAttribute = parseSchemaType "Attribute"
elementToXMLAttribute :: Attribute -> [Content ()]
elementToXMLAttribute = schemaTypeToXML "Attribute"
 
elementDefaultErrorAttributes :: XMLParser DefaultErrorAttributes
elementDefaultErrorAttributes = parseSchemaType "DefaultErrorAttributes"
elementToXMLDefaultErrorAttributes :: DefaultErrorAttributes -> [Content ()]
elementToXMLDefaultErrorAttributes = schemaTypeToXML "DefaultErrorAttributes"
 
elementToolAttribute :: XMLParser ToolAttribute
elementToolAttribute = parseSchemaType "ToolAttribute"
elementToXMLToolAttribute :: ToolAttribute -> [Content ()]
elementToXMLToolAttribute = schemaTypeToXML "ToolAttribute"
 
elementArtifactAttribute :: XMLParser ArtifactAttribute
elementArtifactAttribute = parseSchemaType "ArtifactAttribute"
elementToXMLArtifactAttribute :: ArtifactAttribute -> [Content ()]
elementToXMLArtifactAttribute = schemaTypeToXML "ArtifactAttribute"
 
elementErrorOccurrence :: XMLParser ErrorOccurrence
elementErrorOccurrence = parseSchemaType "ErrorOccurrence"
elementToXMLErrorOccurrence :: ErrorOccurrence -> [Content ()]
elementToXMLErrorOccurrence = schemaTypeToXML "ErrorOccurrence"
 
elementCheckOccurrence :: XMLParser CheckOccurrence
elementCheckOccurrence = parseSchemaType "CheckOccurrence"
elementToXMLCheckOccurrence :: CheckOccurrence -> [Content ()]
elementToXMLCheckOccurrence = schemaTypeToXML "CheckOccurrence"
 
elementRestrictionOccurrence :: XMLParser RestrictionOccurrence
elementRestrictionOccurrence = parseSchemaType "RestrictionOccurrence"
elementToXMLRestrictionOccurrence :: RestrictionOccurrence -> [Content ()]
elementToXMLRestrictionOccurrence = schemaTypeToXML "RestrictionOccurrence"
 
elementVariant :: XMLParser Variant
elementVariant = parseSchemaType "Variant"
elementToXMLVariant :: Variant -> [Content ()]
elementToXMLVariant = schemaTypeToXML "Variant"
 
elementORVariant :: XMLParser ORVariant
elementORVariant = parseSchemaType "ORVariant"
elementToXMLORVariant :: ORVariant -> [Content ()]
elementToXMLORVariant = schemaTypeToXML "ORVariant"
 
elementOneOfVariant :: XMLParser OneOfVariant
elementOneOfVariant = parseSchemaType "OneOfVariant"
elementToXMLOneOfVariant :: OneOfVariant -> [Content ()]
elementToXMLOneOfVariant = schemaTypeToXML "OneOfVariant"
 
elementANDVariant :: XMLParser ANDVariant
elementANDVariant = parseSchemaType "ANDVariant"
elementToXMLANDVariant :: ANDVariant -> [Content ()]
elementToXMLANDVariant = schemaTypeToXML "ANDVariant"
 
elementTest :: XMLParser Test
elementTest = parseSchemaType "Test"
elementToXMLTest :: Test -> [Content ()]
elementToXMLTest = schemaTypeToXML "Test"
 
elementIdentification :: XMLParser Identification
elementIdentification = parseSchemaType "Identification"
elementToXMLIdentification :: Identification -> [Content ()]
elementToXMLIdentification = schemaTypeToXML "Identification"
 
elementKnownBug :: XMLParser KnownBug
elementKnownBug = parseSchemaType "KnownBug"
elementToXMLKnownBug :: KnownBug -> [Content ()]
elementToXMLKnownBug = schemaTypeToXML "KnownBug"
 
elementQualificationProject :: XMLParser QualificationProject
elementQualificationProject = parseSchemaType "QualificationProject"
elementToXMLQualificationProject :: QualificationProject -> [Content ()]
elementToXMLQualificationProject = schemaTypeToXML "QualificationProject"
 
elementQualificationRole :: XMLParser QualificationRole
elementQualificationRole = parseSchemaType "QualificationRole"
elementToXMLQualificationRole :: QualificationRole -> [Content ()]
elementToXMLQualificationRole = schemaTypeToXML "QualificationRole"
 
elementQualificationStep :: XMLParser QualificationStep
elementQualificationStep = parseSchemaType "QualificationStep"
elementToXMLQualificationStep :: QualificationStep -> [Content ()]
elementToXMLQualificationStep = schemaTypeToXML "QualificationStep"
 
elementQualificationArtifact :: XMLParser QualificationArtifact
elementQualificationArtifact = parseSchemaType "QualificationArtifact"
elementToXMLQualificationArtifact :: QualificationArtifact -> [Content ()]
elementToXMLQualificationArtifact = schemaTypeToXML "QualificationArtifact"
 
elementMethod :: XMLParser Method
elementMethod = parseSchemaType "Method"
elementToXMLMethod :: Method -> [Content ()]
elementToXMLMethod = schemaTypeToXML "Method"
 
elementStandardMethods :: XMLParser StandardMethods
elementStandardMethods = parseSchemaType "StandardMethods"
elementToXMLStandardMethods :: StandardMethods -> [Content ()]
elementToXMLStandardMethods = schemaTypeToXML "StandardMethods"
 
elementReview :: XMLParser Review
elementReview = parseSchemaType "Review"
elementToXMLReview :: Review -> [Content ()]
elementToXMLReview = schemaTypeToXML "Review"
 
elementIBug :: XMLParser IBug
elementIBug = parseSchemaType "IBug"
elementToXMLIBug :: IBug -> [Content ()]
elementToXMLIBug = schemaTypeToXML "IBug"
 
elementIDerivable :: XMLParser IDerivable
elementIDerivable = parseSchemaType "IDerivable"
elementToXMLIDerivable :: IDerivable -> [Content ()]
elementToXMLIDerivable = schemaTypeToXML "IDerivable"
 
elementTestRun :: XMLParser TestRun
elementTestRun = parseSchemaType "TestRun"
elementToXMLTestRun :: TestRun -> [Content ()]
elementToXMLTestRun = schemaTypeToXML "TestRun"
 
elementTestResult :: XMLParser TestResult
elementTestResult = parseSchemaType "TestResult"
elementToXMLTestResult :: TestResult -> [Content ()]
elementToXMLTestResult = schemaTypeToXML "TestResult"
 
elementLibrary :: XMLParser Library
elementLibrary = parseSchemaType "Library"
elementToXMLLibrary :: Library -> [Content ()]
elementToXMLLibrary = schemaTypeToXML "Library"
 
elementAnomalousOpCond :: XMLParser AnomalousOpCond
elementAnomalousOpCond = parseSchemaType "AnomalousOpCond"
elementToXMLAnomalousOpCond :: AnomalousOpCond -> [Content ()]
elementToXMLAnomalousOpCond = schemaTypeToXML "AnomalousOpCond"
 
elementNOTVariant :: XMLParser NOTVariant
elementNOTVariant = parseSchemaType "NOTVariant"
elementToXMLNOTVariant :: NOTVariant -> [Content ()]
elementToXMLNOTVariant = schemaTypeToXML "NOTVariant"
 
elementParameter :: XMLParser Parameter
elementParameter = parseSchemaType "Parameter"
elementToXMLParameter :: Parameter -> [Content ()]
elementToXMLParameter = schemaTypeToXML "Parameter"
 
elementEnumValue :: XMLParser EnumValue
elementEnumValue = parseSchemaType "EnumValue"
elementToXMLEnumValue :: EnumValue -> [Content ()]
elementToXMLEnumValue = schemaTypeToXML "EnumValue"
 
elementToolQualification :: XMLParser ToolQualification
elementToolQualification = parseSchemaType "ToolQualification"
elementToXMLToolQualification :: ToolQualification -> [Content ()]
elementToXMLToolQualification = schemaTypeToXML "ToolQualification"
 
elementLibraryQualification :: XMLParser LibraryQualification
elementLibraryQualification = parseSchemaType "LibraryQualification"
elementToXMLLibraryQualification :: LibraryQualification -> [Content ()]
elementToXMLLibraryQualification = schemaTypeToXML "LibraryQualification"
 
elementProgressTrackingState :: XMLParser ProgressTrackingState
elementProgressTrackingState = parseSchemaType "ProgressTrackingState"
elementToXMLProgressTrackingState :: ProgressTrackingState -> [Content ()]
elementToXMLProgressTrackingState = schemaTypeToXML "ProgressTrackingState"
 
elementCommandLineParameter :: XMLParser CommandLineParameter
elementCommandLineParameter = parseSchemaType "CommandLineParameter"
elementToXMLCommandLineParameter :: CommandLineParameter -> [Content ()]
elementToXMLCommandLineParameter = schemaTypeToXML "CommandLineParameter"
 
elementWithNameAndPath :: XMLParser WithNameAndPath
elementWithNameAndPath = parseSchemaType "WithNameAndPath"
elementToXMLWithNameAndPath :: WithNameAndPath -> [Content ()]
elementToXMLWithNameAndPath = schemaTypeToXML "WithNameAndPath"
 
data IIdentifiable
        = IIdentifiable_ProgressTrackingState ProgressTrackingState
        | IIdentifiable_EnumValue EnumValue
        | IIdentifiable_Parameter Parameter
        | IIdentifiable_AnomalousOpCond AnomalousOpCond
        | IIdentifiable_TestResult TestResult
        | IIdentifiable_TestRun TestRun
        | IIdentifiable_Review Review
        | IIdentifiable_Method Method
        | IIdentifiable_QualificationArtifact QualificationArtifact
        | IIdentifiable_QualificationStep QualificationStep
        | IIdentifiable_QualificationRole QualificationRole
        | IIdentifiable_QualificationProject QualificationProject
        | IIdentifiable_KnownBug KnownBug
        | IIdentifiable_Identification Identification
        | IIdentifiable_Variant Variant
        | IIdentifiable_Attribute Attribute
        | IIdentifiable_Costs Costs
        | IIdentifiable_IToolChainElement IToolChainElement
        
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType IIdentifiable where
    parseSchemaType s = do
        (fmap IIdentifiable_ProgressTrackingState $ parseSchemaType s)
        `onFail`
        (fmap IIdentifiable_EnumValue $ parseSchemaType s)
        `onFail`
        (fmap IIdentifiable_Parameter $ parseSchemaType s)
        `onFail`
        (fmap IIdentifiable_AnomalousOpCond $ parseSchemaType s)
        `onFail`
        (fmap IIdentifiable_TestResult $ parseSchemaType s)
        `onFail`
        (fmap IIdentifiable_TestRun $ parseSchemaType s)
        `onFail`
        (fmap IIdentifiable_Review $ parseSchemaType s)
        `onFail`
        (fmap IIdentifiable_Method $ parseSchemaType s)
        `onFail`
        (fmap IIdentifiable_QualificationArtifact $ parseSchemaType s)
        `onFail`
        (fmap IIdentifiable_QualificationStep $ parseSchemaType s)
        `onFail`
        (fmap IIdentifiable_QualificationRole $ parseSchemaType s)
        `onFail`
        (fmap IIdentifiable_QualificationProject $ parseSchemaType s)
        `onFail`
        (fmap IIdentifiable_KnownBug $ parseSchemaType s)
        `onFail`
        (fmap IIdentifiable_Identification $ parseSchemaType s)
        `onFail`
        (fmap IIdentifiable_Variant $ parseSchemaType s)
        `onFail`
        (fmap IIdentifiable_Attribute $ parseSchemaType s)
        `onFail`
        (fmap IIdentifiable_Costs $ parseSchemaType s)
        `onFail`
        (fmap IIdentifiable_IToolChainElement $ parseSchemaType s)
        `onFail` fail "Parse failed when expecting an extension type of IIdentifiable,\n\
\  namely one of:\n\
\ProgressTrackingState,EnumValue,Parameter,AnomalousOpCond,TestResult,TestRun,Review,Method,QualificationArtifact,QualificationStep,QualificationRole,QualificationProject,KnownBug,Identification,Variant,Attribute,Costs,IToolChainElement"
    schemaTypeToXML _s (IIdentifiable_ProgressTrackingState x) = schemaTypeToXML "progressTrackingState" x
    schemaTypeToXML _s (IIdentifiable_EnumValue x) = schemaTypeToXML "enumValue" x
    schemaTypeToXML _s (IIdentifiable_Parameter x) = schemaTypeToXML "parameter" x
    schemaTypeToXML _s (IIdentifiable_AnomalousOpCond x) = schemaTypeToXML "anomalousOpCond" x
    schemaTypeToXML _s (IIdentifiable_TestResult x) = schemaTypeToXML "testResult" x
    schemaTypeToXML _s (IIdentifiable_TestRun x) = schemaTypeToXML "testRun" x
    schemaTypeToXML _s (IIdentifiable_Review x) = schemaTypeToXML "review" x
    schemaTypeToXML _s (IIdentifiable_Method x) = schemaTypeToXML "method" x
    schemaTypeToXML _s (IIdentifiable_QualificationArtifact x) = schemaTypeToXML "qualificationArtifact" x
    schemaTypeToXML _s (IIdentifiable_QualificationStep x) = schemaTypeToXML "qualificationStep" x
    schemaTypeToXML _s (IIdentifiable_QualificationRole x) = schemaTypeToXML "qualificationRole" x
    schemaTypeToXML _s (IIdentifiable_QualificationProject x) = schemaTypeToXML "qualificationProject" x
    schemaTypeToXML _s (IIdentifiable_KnownBug x) = schemaTypeToXML "knownBug" x
    schemaTypeToXML _s (IIdentifiable_Identification x) = schemaTypeToXML "identification" x
    schemaTypeToXML _s (IIdentifiable_Variant x) = schemaTypeToXML "variant" x
    schemaTypeToXML _s (IIdentifiable_Attribute x) = schemaTypeToXML "attribute" x
    schemaTypeToXML _s (IIdentifiable_Costs x) = schemaTypeToXML "costs" x
    schemaTypeToXML _s (IIdentifiable_IToolChainElement x) = schemaTypeToXML "iToolChainElement" x
 
--  (There are no subtypes defined for this abstract type.)
data IAssumable = IAssumable deriving (GHCG.Generic,Eq,Show)
instance SchemaType IAssumable where
    parseSchemaType s = fail "Parse failed when expecting an extension type of IAssumable:\n  No extension types are known."
    schemaTypeToXML s _ = toXMLElement s [] []
 
--  (There are no subtypes defined for this abstract type.)
data IVirtualizable = IVirtualizable deriving (GHCG.Generic,Eq,Show)
instance SchemaType IVirtualizable where
    parseSchemaType s = fail "Parse failed when expecting an extension type of IVirtualizable:\n  No extension types are known."
    schemaTypeToXML s _ = toXMLElement s [] []
 
--  (There are no subtypes defined for this abstract type.)
data IDeactivatable = IDeactivatable deriving (GHCG.Generic,Eq,Show)
instance SchemaType IDeactivatable where
    parseSchemaType s = fail "Parse failed when expecting an extension type of IDeactivatable:\n  No extension types are known."
    schemaTypeToXML s _ = toXMLElement s [] []
 
--  (There are no subtypes defined for this abstract type.)
data IImpactable = IImpactable deriving (GHCG.Generic,Eq,Show)
instance SchemaType IImpactable where
    parseSchemaType s = fail "Parse failed when expecting an extension type of IImpactable:\n  No extension types are known."
    schemaTypeToXML s _ = toXMLElement s [] []
 
data IToolChainElement
        = IToolChainElement_IBug IBug
        | IToolChainElement_Test Test
        | IToolChainElement_Qualification Qualification
        | IToolChainElement_Restriction Restriction
        | IToolChainElement_Check Check
        | IToolChainElement_Artifact Artifact
        | IToolChainElement_UseCase UseCase
        | IToolChainElement_Tool Tool
        
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType IToolChainElement where
    parseSchemaType s = do
        (fmap IToolChainElement_IBug $ parseSchemaType s)
        `onFail`
        (fmap IToolChainElement_Test $ parseSchemaType s)
        `onFail`
        (fmap IToolChainElement_Qualification $ parseSchemaType s)
        `onFail`
        (fmap IToolChainElement_Restriction $ parseSchemaType s)
        `onFail`
        (fmap IToolChainElement_Check $ parseSchemaType s)
        `onFail`
        (fmap IToolChainElement_Artifact $ parseSchemaType s)
        `onFail`
        (fmap IToolChainElement_UseCase $ parseSchemaType s)
        `onFail`
        (fmap IToolChainElement_Tool $ parseSchemaType s)
        `onFail` fail "Parse failed when expecting an extension type of IToolChainElement,\n\
\  namely one of:\n\
\IBug,Test,Qualification,Restriction,Check,Artifact,UseCase,Tool"
    schemaTypeToXML _s (IToolChainElement_IBug x) = schemaTypeToXML "iBug" x
    schemaTypeToXML _s (IToolChainElement_Test x) = schemaTypeToXML "test" x
    schemaTypeToXML _s (IToolChainElement_Qualification x) = schemaTypeToXML "qualification" x
    schemaTypeToXML _s (IToolChainElement_Restriction x) = schemaTypeToXML "restriction" x
    schemaTypeToXML _s (IToolChainElement_Check x) = schemaTypeToXML "check" x
    schemaTypeToXML _s (IToolChainElement_Artifact x) = schemaTypeToXML "artifact" x
    schemaTypeToXML _s (IToolChainElement_UseCase x) = schemaTypeToXML "useCase" x
    schemaTypeToXML _s (IToolChainElement_Tool x) = schemaTypeToXML "tool" x
instance Extension IToolChainElement IIdentifiable where
    supertype v = IIdentifiable_IToolChainElement v
 
data Tool = Tool
        { tool_iD :: Maybe Ecore.EString
        , tool_name :: Maybe Ecore.EString
        , tool_description :: Maybe Ecore.EString
        , tool_longDescription :: Maybe Ecore.EString
        , tool_comment :: Maybe Ecore.EString
        , tool_partOf :: Maybe Xsd.AnyURI
        , tool_toolAttributes :: Maybe Xsd.XsdString
        , tool_inputs :: Maybe Xsd.XsdString
        , tool_outputs :: Maybe Xsd.XsdString
        , tool_inputsOutputs :: Maybe Xsd.XsdString
        , tool_calledTools :: Maybe Xsd.XsdString
        , tool_callingTools :: Maybe Xsd.XsdString
        , tool_variant :: Maybe Xsd.XsdString
        , tool_knownBugManagement :: Maybe Ecore.EString
        , tool_knownBugDate :: Maybe Ecore.EString
        , tool_numberOfExecutions :: Maybe Ecore.EInt
        , tool_internalReference :: Maybe Ecore.EString
        , tool_numberOfSetups :: Maybe Ecore.EInt
        , tool_supportedMethods :: Maybe Xsd.XsdString
        , tool_superTool :: Maybe Xsd.AnyURI
        , tool_anomalousOpCondHandling :: Maybe Ecore.EString
        , tool_toolOwner :: Maybe Ecore.EString
        , tool_toolProvider :: Maybe Ecore.EString
        , tool_path :: Maybe Ecore.EString
        , tool_useCases :: [UseCase]
        , tool_costs :: [ToolCosts]
        , tool_qualifications :: [Qualification]
        , tool_tests :: [Test]
        , tool_identifications :: [Identification]
        , tool_knownBugs :: [KnownBug]
        , tool_qualificationProjects :: [QualificationProject]
        , tool_reviews :: [Review]
        , tool_testRuns :: [TestRun]
        , tool_artifacts :: [Artifact]
        , tool_anomalousOpConds :: [AnomalousOpCond]
        , tool_parameters :: [Parameter]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType Tool where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "PartOf" e pos
        a6 <- optional $ getAttribute "ToolAttributes" e pos
        a7 <- optional $ getAttribute "Inputs" e pos
        a8 <- optional $ getAttribute "Outputs" e pos
        a9 <- optional $ getAttribute "InputsOutputs" e pos
        a10 <- optional $ getAttribute "CalledTools" e pos
        a11 <- optional $ getAttribute "CallingTools" e pos
        a12 <- optional $ getAttribute "Variant" e pos
        a13 <- optional $ getAttribute "KnownBugManagement" e pos
        a14 <- optional $ getAttribute "KnownBugDate" e pos
        a15 <- optional $ getAttribute "NumberOfExecutions" e pos
        a16 <- optional $ getAttribute "InternalReference" e pos
        a17 <- optional $ getAttribute "NumberOfSetups" e pos
        a18 <- optional $ getAttribute "SupportedMethods" e pos
        a19 <- optional $ getAttribute "SuperTool" e pos
        a20 <- optional $ getAttribute "AnomalousOpCondHandling" e pos
        a21 <- optional $ getAttribute "ToolOwner" e pos
        a22 <- optional $ getAttribute "ToolProvider" e pos
        a23 <- optional $ getAttribute "Path" e pos
        commit $ interior e $ return (Tool a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23)
            `apply` many (parseSchemaType "UseCases")
            `apply` many (parseSchemaType "Costs")
            `apply` many (parseSchemaType "Qualifications")
            `apply` many (parseSchemaType "Tests")
            `apply` many (parseSchemaType "Identifications")
            `apply` many (parseSchemaType "KnownBugs")
            `apply` many (parseSchemaType "QualificationProjects")
            `apply` many (parseSchemaType "Reviews")
            `apply` many (parseSchemaType "TestRuns")
            `apply` many (parseSchemaType "Artifacts")
            `apply` many (parseSchemaType "AnomalousOpConds")
            `apply` many (parseSchemaType "Parameters")
    schemaTypeToXML s x@Tool{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ tool_iD x
                       , maybe [] (toXMLAttribute "Name") $ tool_name x
                       , maybe [] (toXMLAttribute "Description") $ tool_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ tool_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ tool_comment x
                       , maybe [] (toXMLAttribute "PartOf") $ tool_partOf x
                       , maybe [] (toXMLAttribute "ToolAttributes") $ tool_toolAttributes x
                       , maybe [] (toXMLAttribute "Inputs") $ tool_inputs x
                       , maybe [] (toXMLAttribute "Outputs") $ tool_outputs x
                       , maybe [] (toXMLAttribute "InputsOutputs") $ tool_inputsOutputs x
                       , maybe [] (toXMLAttribute "CalledTools") $ tool_calledTools x
                       , maybe [] (toXMLAttribute "CallingTools") $ tool_callingTools x
                       , maybe [] (toXMLAttribute "Variant") $ tool_variant x
                       , maybe [] (toXMLAttribute "KnownBugManagement") $ tool_knownBugManagement x
                       , maybe [] (toXMLAttribute "KnownBugDate") $ tool_knownBugDate x
                       , maybe [] (toXMLAttribute "NumberOfExecutions") $ tool_numberOfExecutions x
                       , maybe [] (toXMLAttribute "InternalReference") $ tool_internalReference x
                       , maybe [] (toXMLAttribute "NumberOfSetups") $ tool_numberOfSetups x
                       , maybe [] (toXMLAttribute "SupportedMethods") $ tool_supportedMethods x
                       , maybe [] (toXMLAttribute "SuperTool") $ tool_superTool x
                       , maybe [] (toXMLAttribute "AnomalousOpCondHandling") $ tool_anomalousOpCondHandling x
                       , maybe [] (toXMLAttribute "ToolOwner") $ tool_toolOwner x
                       , maybe [] (toXMLAttribute "ToolProvider") $ tool_toolProvider x
                       , maybe [] (toXMLAttribute "Path") $ tool_path x
                       ]
            [ concatMap (schemaTypeToXML "UseCases") $ tool_useCases x
            , concatMap (schemaTypeToXML "Costs") $ tool_costs x
            , concatMap (schemaTypeToXML "Qualifications") $ tool_qualifications x
            , concatMap (schemaTypeToXML "Tests") $ tool_tests x
            , concatMap (schemaTypeToXML "Identifications") $ tool_identifications x
            , concatMap (schemaTypeToXML "KnownBugs") $ tool_knownBugs x
            , concatMap (schemaTypeToXML "QualificationProjects") $ tool_qualificationProjects x
            , concatMap (schemaTypeToXML "Reviews") $ tool_reviews x
            , concatMap (schemaTypeToXML "TestRuns") $ tool_testRuns x
            , concatMap (schemaTypeToXML "Artifacts") $ tool_artifacts x
            , concatMap (schemaTypeToXML "AnomalousOpConds") $ tool_anomalousOpConds x
            , concatMap (schemaTypeToXML "Parameters") $ tool_parameters x
            ]
instance Extension Tool IToolChainElement where
    supertype v = IToolChainElement_Tool v
instance Extension Tool IIdentifiable where
    supertype = (supertype :: IToolChainElement -> IIdentifiable)
              . (supertype :: Tool -> IToolChainElement)
              
 
data UseCase = UseCase
        { useCase_iD :: Maybe Ecore.EString
        , useCase_name :: Maybe Ecore.EString
        , useCase_description :: Maybe Ecore.EString
        , useCase_longDescription :: Maybe Ecore.EString
        , useCase_comment :: Maybe Ecore.EString
        , useCase_tool :: Maybe Xsd.AnyURI
        , useCase_inputs :: Maybe Xsd.XsdString
        , useCase_outputs :: Maybe Xsd.XsdString
        , useCase_inputsOutputs :: Maybe Xsd.XsdString
        , useCase_qualifications :: Maybe Xsd.XsdString
        , useCase_mandatoryFeatures :: Maybe Xsd.XsdString
        , useCase_excludedFeatures :: Maybe Xsd.XsdString
        , useCase_requiredFeatures :: Maybe Xsd.XsdString
        , useCase_expandedAttributes :: Maybe Xsd.XsdString
        , useCase_partOf :: Maybe Xsd.AnyURI
        , useCase_calledUseCases :: Maybe Xsd.XsdString
        , useCase_callingUseCases :: Maybe Xsd.XsdString
        , useCase_showPartsErrors :: Maybe Ecore.EBoolean
        , useCase_showPartOfErrors :: Maybe Ecore.EBoolean
        , useCase_variant :: Maybe Xsd.XsdString
        , useCase_tests :: Maybe Xsd.XsdString
        , useCase_numberOfExecutions :: Maybe Ecore.EInt
        , useCase_internalReference :: Maybe Ecore.EString
        , useCase_numberOfSetups :: Maybe Ecore.EInt
        , useCase_attributes :: Maybe Xsd.XsdString
          -- ^ This is called Attribues instead of ToolAttributes for 
          --   compatibily reasons with older versions
        , useCase_knownBugs :: Maybe Xsd.XsdString
        , useCase_commandLineArgument :: Maybe Ecore.EString
        , useCase_testRuns :: Maybe Xsd.XsdString
        , useCase_qualificationResult :: Maybe QualificationResult
        , useCase_testResults :: Maybe Xsd.XsdString
        , useCase_path :: Maybe Ecore.EString
        , useCase_errors :: [Error]
        , useCase_checks :: [Check]
        , useCase_restrictions :: [Restriction]
        , useCase_costs :: [UseCaseCosts]
        , useCase_parts :: [UseCase]
        , useCase_inferredErrors :: [InferredError]
        , useCase_inferredChecks :: [InferredCheck]
        , useCase_inferredRestrictions :: [InferredRestriction]
        , useCase_reviews :: [Review]
        , useCase_parameters :: [Parameter]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType UseCase where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "Tool" e pos
        a6 <- optional $ getAttribute "Inputs" e pos
        a7 <- optional $ getAttribute "Outputs" e pos
        a8 <- optional $ getAttribute "InputsOutputs" e pos
        a9 <- optional $ getAttribute "Qualifications" e pos
        a10 <- optional $ getAttribute "MandatoryFeatures" e pos
        a11 <- optional $ getAttribute "ExcludedFeatures" e pos
        a12 <- optional $ getAttribute "RequiredFeatures" e pos
        a13 <- optional $ getAttribute "ExpandedAttributes" e pos
        a14 <- optional $ getAttribute "PartOf" e pos
        a15 <- optional $ getAttribute "CalledUseCases" e pos
        a16 <- optional $ getAttribute "CallingUseCases" e pos
        a17 <- optional $ getAttribute "ShowPartsErrors" e pos
        a18 <- optional $ getAttribute "ShowPartOfErrors" e pos
        a19 <- optional $ getAttribute "Variant" e pos
        a20 <- optional $ getAttribute "Tests" e pos
        a21 <- optional $ getAttribute "NumberOfExecutions" e pos
        a22 <- optional $ getAttribute "InternalReference" e pos
        a23 <- optional $ getAttribute "NumberOfSetups" e pos
        a24 <- optional $ getAttribute "Attributes" e pos
        a25 <- optional $ getAttribute "KnownBugs" e pos
        a26 <- optional $ getAttribute "CommandLineArgument" e pos
        a27 <- optional $ getAttribute "TestRuns" e pos
        a28 <- optional $ getAttribute "QualificationResult" e pos
        a29 <- optional $ getAttribute "testResults" e pos
        a30 <- optional $ getAttribute "Path" e pos
        commit $ interior e $ return (UseCase a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30)
            `apply` many (parseSchemaType "Errors")
            `apply` many (parseSchemaType "Checks")
            `apply` many (parseSchemaType "Restrictions")
            `apply` many (parseSchemaType "Costs")
            `apply` many (parseSchemaType "Parts")
            `apply` many (parseSchemaType "InferredErrors")
            `apply` many (parseSchemaType "InferredChecks")
            `apply` many (parseSchemaType "InferredRestrictions")
            `apply` many (parseSchemaType "Reviews")
            `apply` many (parseSchemaType "Parameters")
    schemaTypeToXML s x@UseCase{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ useCase_iD x
                       , maybe [] (toXMLAttribute "Name") $ useCase_name x
                       , maybe [] (toXMLAttribute "Description") $ useCase_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ useCase_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ useCase_comment x
                       , maybe [] (toXMLAttribute "Tool") $ useCase_tool x
                       , maybe [] (toXMLAttribute "Inputs") $ useCase_inputs x
                       , maybe [] (toXMLAttribute "Outputs") $ useCase_outputs x
                       , maybe [] (toXMLAttribute "InputsOutputs") $ useCase_inputsOutputs x
                       , maybe [] (toXMLAttribute "Qualifications") $ useCase_qualifications x
                       , maybe [] (toXMLAttribute "MandatoryFeatures") $ useCase_mandatoryFeatures x
                       , maybe [] (toXMLAttribute "ExcludedFeatures") $ useCase_excludedFeatures x
                       , maybe [] (toXMLAttribute "RequiredFeatures") $ useCase_requiredFeatures x
                       , maybe [] (toXMLAttribute "ExpandedAttributes") $ useCase_expandedAttributes x
                       , maybe [] (toXMLAttribute "PartOf") $ useCase_partOf x
                       , maybe [] (toXMLAttribute "CalledUseCases") $ useCase_calledUseCases x
                       , maybe [] (toXMLAttribute "CallingUseCases") $ useCase_callingUseCases x
                       , maybe [] (toXMLAttribute "ShowPartsErrors") $ useCase_showPartsErrors x
                       , maybe [] (toXMLAttribute "ShowPartOfErrors") $ useCase_showPartOfErrors x
                       , maybe [] (toXMLAttribute "Variant") $ useCase_variant x
                       , maybe [] (toXMLAttribute "Tests") $ useCase_tests x
                       , maybe [] (toXMLAttribute "NumberOfExecutions") $ useCase_numberOfExecutions x
                       , maybe [] (toXMLAttribute "InternalReference") $ useCase_internalReference x
                       , maybe [] (toXMLAttribute "NumberOfSetups") $ useCase_numberOfSetups x
                       , maybe [] (toXMLAttribute "Attributes") $ useCase_attributes x
                       , maybe [] (toXMLAttribute "KnownBugs") $ useCase_knownBugs x
                       , maybe [] (toXMLAttribute "CommandLineArgument") $ useCase_commandLineArgument x
                       , maybe [] (toXMLAttribute "TestRuns") $ useCase_testRuns x
                       , maybe [] (toXMLAttribute "QualificationResult") $ useCase_qualificationResult x
                       , maybe [] (toXMLAttribute "testResults") $ useCase_testResults x
                       , maybe [] (toXMLAttribute "Path") $ useCase_path x
                       ]
            [ concatMap (schemaTypeToXML "Errors") $ useCase_errors x
            , concatMap (schemaTypeToXML "Checks") $ useCase_checks x
            , concatMap (schemaTypeToXML "Restrictions") $ useCase_restrictions x
            , concatMap (schemaTypeToXML "Costs") $ useCase_costs x
            , concatMap (schemaTypeToXML "Parts") $ useCase_parts x
            , concatMap (schemaTypeToXML "InferredErrors") $ useCase_inferredErrors x
            , concatMap (schemaTypeToXML "InferredChecks") $ useCase_inferredChecks x
            , concatMap (schemaTypeToXML "InferredRestrictions") $ useCase_inferredRestrictions x
            , concatMap (schemaTypeToXML "Reviews") $ useCase_reviews x
            , concatMap (schemaTypeToXML "Parameters") $ useCase_parameters x
            ]
instance Extension UseCase IToolChainElement where
    supertype v = IToolChainElement_UseCase v
instance Extension UseCase IIdentifiable where
    supertype = (supertype :: IToolChainElement -> IIdentifiable)
              . (supertype :: UseCase -> IToolChainElement)
              
 
data Artifact = Artifact
        { artifact_iD :: Maybe Ecore.EString
        , artifact_name :: Maybe Ecore.EString
        , artifact_description :: Maybe Ecore.EString
        , artifact_longDescription :: Maybe Ecore.EString
        , artifact_comment :: Maybe Ecore.EString
        , artifact_format :: Maybe Ecore.EString
        , artifact_createdBy :: Maybe Xsd.XsdString
        , artifact_usedBy :: Maybe Xsd.XsdString
        , artifact_modifiedBy :: Maybe Xsd.XsdString
        , artifact_toolChain :: Maybe Xsd.AnyURI
        , artifact_isA :: Maybe Xsd.XsdString
        , artifact_occurrences :: Maybe Xsd.XsdString
        , artifact_createdByTool :: Maybe Xsd.XsdString
        , artifact_usedByTool :: Maybe Xsd.XsdString
        , artifact_modifiedByTool :: Maybe Xsd.XsdString
        , artifact_artifactAttributes :: Maybe Xsd.XsdString
        , artifact_variant :: Maybe Xsd.XsdString
        , artifact_isPartOfProduct :: Maybe Ecore.EBoolean
        , artifact_internalReference :: Maybe Ecore.EString
        , artifact_reviews :: [Review]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType Artifact where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "Format" e pos
        a6 <- optional $ getAttribute "CreatedBy" e pos
        a7 <- optional $ getAttribute "UsedBy" e pos
        a8 <- optional $ getAttribute "ModifiedBy" e pos
        a9 <- optional $ getAttribute "ToolChain" e pos
        a10 <- optional $ getAttribute "IsA" e pos
        a11 <- optional $ getAttribute "Occurrences" e pos
        a12 <- optional $ getAttribute "CreatedByTool" e pos
        a13 <- optional $ getAttribute "UsedByTool" e pos
        a14 <- optional $ getAttribute "ModifiedByTool" e pos
        a15 <- optional $ getAttribute "ArtifactAttributes" e pos
        a16 <- optional $ getAttribute "Variant" e pos
        a17 <- optional $ getAttribute "IsPartOfProduct" e pos
        a18 <- optional $ getAttribute "InternalReference" e pos
        commit $ interior e $ return (Artifact a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18)
            `apply` many (parseSchemaType "Reviews")
    schemaTypeToXML s x@Artifact{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ artifact_iD x
                       , maybe [] (toXMLAttribute "Name") $ artifact_name x
                       , maybe [] (toXMLAttribute "Description") $ artifact_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ artifact_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ artifact_comment x
                       , maybe [] (toXMLAttribute "Format") $ artifact_format x
                       , maybe [] (toXMLAttribute "CreatedBy") $ artifact_createdBy x
                       , maybe [] (toXMLAttribute "UsedBy") $ artifact_usedBy x
                       , maybe [] (toXMLAttribute "ModifiedBy") $ artifact_modifiedBy x
                       , maybe [] (toXMLAttribute "ToolChain") $ artifact_toolChain x
                       , maybe [] (toXMLAttribute "IsA") $ artifact_isA x
                       , maybe [] (toXMLAttribute "Occurrences") $ artifact_occurrences x
                       , maybe [] (toXMLAttribute "CreatedByTool") $ artifact_createdByTool x
                       , maybe [] (toXMLAttribute "UsedByTool") $ artifact_usedByTool x
                       , maybe [] (toXMLAttribute "ModifiedByTool") $ artifact_modifiedByTool x
                       , maybe [] (toXMLAttribute "ArtifactAttributes") $ artifact_artifactAttributes x
                       , maybe [] (toXMLAttribute "Variant") $ artifact_variant x
                       , maybe [] (toXMLAttribute "IsPartOfProduct") $ artifact_isPartOfProduct x
                       , maybe [] (toXMLAttribute "InternalReference") $ artifact_internalReference x
                       ]
            [ concatMap (schemaTypeToXML "Reviews") $ artifact_reviews x
            ]
instance Extension Artifact IToolChainElement where
    supertype v = IToolChainElement_Artifact v
instance Extension Artifact IIdentifiable where
    supertype = (supertype :: IToolChainElement -> IIdentifiable)
              . (supertype :: Artifact -> IToolChainElement)
              
 
data Error = Error
        { error_inferredOccurrences :: Maybe Xsd.XsdString
        , error_useCase :: Maybe Xsd.AnyURI
        , error_discoveredBy :: Maybe Xsd.XsdString
        , error_avoidedBy :: Maybe Xsd.XsdString
        , error_check :: Maybe Xsd.AnyURI
        , error_restriction :: Maybe Xsd.AnyURI
        , error_attribute :: Maybe Xsd.AnyURI
        , error_subsumes :: Maybe Xsd.XsdString
        , error_isSubsumedFrom :: Maybe Xsd.XsdString
        , error_tests :: Maybe Xsd.XsdString
        , error_internalReference :: Maybe Ecore.EString
        , error_knownBugs :: Maybe Xsd.XsdString
        , error_variant :: Maybe Xsd.XsdString
        , error_qualifications :: Maybe Xsd.XsdString
        , error_anomalousOpConds :: Maybe Xsd.XsdString
        , error_reviews :: [Review]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType Error where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "InferredOccurrences" e pos
        a1 <- optional $ getAttribute "UseCase" e pos
        a2 <- optional $ getAttribute "DiscoveredBy" e pos
        a3 <- optional $ getAttribute "AvoidedBy" e pos
        a4 <- optional $ getAttribute "Check" e pos
        a5 <- optional $ getAttribute "Restriction" e pos
        a6 <- optional $ getAttribute "Attribute" e pos
        a7 <- optional $ getAttribute "Subsumes" e pos
        a8 <- optional $ getAttribute "IsSubsumedFrom" e pos
        a9 <- optional $ getAttribute "Tests" e pos
        a10 <- optional $ getAttribute "InternalReference" e pos
        a11 <- optional $ getAttribute "KnownBugs" e pos
        a12 <- optional $ getAttribute "Variant" e pos
        a13 <- optional $ getAttribute "Qualifications" e pos
        a14 <- optional $ getAttribute "AnomalousOpConds" e pos
        commit $ interior e $ return (Error a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)
            `apply` many (parseSchemaType "Reviews")
    schemaTypeToXML s x@Error{} =
        toXMLElement s [ maybe [] (toXMLAttribute "InferredOccurrences") $ error_inferredOccurrences x
                       , maybe [] (toXMLAttribute "UseCase") $ error_useCase x
                       , maybe [] (toXMLAttribute "DiscoveredBy") $ error_discoveredBy x
                       , maybe [] (toXMLAttribute "AvoidedBy") $ error_avoidedBy x
                       , maybe [] (toXMLAttribute "Check") $ error_check x
                       , maybe [] (toXMLAttribute "Restriction") $ error_restriction x
                       , maybe [] (toXMLAttribute "Attribute") $ error_attribute x
                       , maybe [] (toXMLAttribute "Subsumes") $ error_subsumes x
                       , maybe [] (toXMLAttribute "IsSubsumedFrom") $ error_isSubsumedFrom x
                       , maybe [] (toXMLAttribute "Tests") $ error_tests x
                       , maybe [] (toXMLAttribute "InternalReference") $ error_internalReference x
                       , maybe [] (toXMLAttribute "KnownBugs") $ error_knownBugs x
                       , maybe [] (toXMLAttribute "Variant") $ error_variant x
                       , maybe [] (toXMLAttribute "Qualifications") $ error_qualifications x
                       , maybe [] (toXMLAttribute "AnomalousOpConds") $ error_anomalousOpConds x
                       ]
            [ concatMap (schemaTypeToXML "Reviews") $ error_reviews x
            ]
instance Extension Error ErrorOccurrence where
    supertype v = ErrorOccurrence_Error v
 
data ConfidenceNeed
    = ConfidenceNeed_T1
    | ConfidenceNeed_T2
    | ConfidenceNeed_T3
    | ConfidenceNeed_TCL1
    | ConfidenceNeed_TCL2
    | ConfidenceNeed_TCL3
    | ConfidenceNeed_Criteria1
    | ConfidenceNeed_Criteria2
    | ConfidenceNeed_Criteria3
    | ConfidenceNeed_None
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType ConfidenceNeed where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ConfidenceNeed where
    acceptingParser =  do literal "T1"; return ConfidenceNeed_T1
                      `onFail` do literal "T2"; return ConfidenceNeed_T2
                      `onFail` do literal "T3"; return ConfidenceNeed_T3
                      `onFail` do literal "TCL1"; return ConfidenceNeed_TCL1
                      `onFail` do literal "TCL2"; return ConfidenceNeed_TCL2
                      `onFail` do literal "TCL3"; return ConfidenceNeed_TCL3
                      `onFail` do literal "Criteria1"; return ConfidenceNeed_Criteria1
                      `onFail` do literal "Criteria2"; return ConfidenceNeed_Criteria2
                      `onFail` do literal "Criteria3"; return ConfidenceNeed_Criteria3
                      `onFail` do literal "None"; return ConfidenceNeed_None
                      
    simpleTypeText ConfidenceNeed_T1 = "T1"
    simpleTypeText ConfidenceNeed_T2 = "T2"
    simpleTypeText ConfidenceNeed_T3 = "T3"
    simpleTypeText ConfidenceNeed_TCL1 = "TCL1"
    simpleTypeText ConfidenceNeed_TCL2 = "TCL2"
    simpleTypeText ConfidenceNeed_TCL3 = "TCL3"
    simpleTypeText ConfidenceNeed_Criteria1 = "Criteria1"
    simpleTypeText ConfidenceNeed_Criteria2 = "Criteria2"
    simpleTypeText ConfidenceNeed_Criteria3 = "Criteria3"
    simpleTypeText ConfidenceNeed_None = "None"
 
data Check = Check
        { check_iD :: Maybe Ecore.EString
        , check_name :: Maybe Ecore.EString
        , check_description :: Maybe Ecore.EString
        , check_longDescription :: Maybe Ecore.EString
        , check_comment :: Maybe Ecore.EString
        , check_useCase :: Maybe Xsd.AnyURI
        , check_detectedError :: Maybe Xsd.XsdString
        , check_detectionProbability :: Maybe Probability
        , check_attribute :: Maybe Xsd.AnyURI
        , check_subsumes :: Maybe Xsd.XsdString
        , check_isSubsumedFrom :: Maybe Xsd.XsdString
        , check_internalReference :: Maybe Ecore.EString
        , check_detectedKnownBugs :: Maybe Xsd.XsdString
        , check_variant :: Maybe Xsd.XsdString
        , check_category :: Maybe Category
        , check_effort :: Maybe Effort
        , check_detectedAnomOpConds :: Maybe Xsd.XsdString
        , check_errors :: [Error]
        , check_reviews :: [Review]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType Check where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "UseCase" e pos
        a6 <- optional $ getAttribute "DetectedError" e pos
        a7 <- optional $ getAttribute "DetectionProbability" e pos
        a8 <- optional $ getAttribute "Attribute" e pos
        a9 <- optional $ getAttribute "Subsumes" e pos
        a10 <- optional $ getAttribute "IsSubsumedFrom" e pos
        a11 <- optional $ getAttribute "InternalReference" e pos
        a12 <- optional $ getAttribute "DetectedKnownBugs" e pos
        a13 <- optional $ getAttribute "Variant" e pos
        a14 <- optional $ getAttribute "Category" e pos
        a15 <- optional $ getAttribute "Effort" e pos
        a16 <- optional $ getAttribute "DetectedAnomOpConds" e pos
        commit $ interior e $ return (Check a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16)
            `apply` many (parseSchemaType "Errors")
            `apply` many (parseSchemaType "Reviews")
    schemaTypeToXML s x@Check{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ check_iD x
                       , maybe [] (toXMLAttribute "Name") $ check_name x
                       , maybe [] (toXMLAttribute "Description") $ check_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ check_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ check_comment x
                       , maybe [] (toXMLAttribute "UseCase") $ check_useCase x
                       , maybe [] (toXMLAttribute "DetectedError") $ check_detectedError x
                       , maybe [] (toXMLAttribute "DetectionProbability") $ check_detectionProbability x
                       , maybe [] (toXMLAttribute "Attribute") $ check_attribute x
                       , maybe [] (toXMLAttribute "Subsumes") $ check_subsumes x
                       , maybe [] (toXMLAttribute "IsSubsumedFrom") $ check_isSubsumedFrom x
                       , maybe [] (toXMLAttribute "InternalReference") $ check_internalReference x
                       , maybe [] (toXMLAttribute "DetectedKnownBugs") $ check_detectedKnownBugs x
                       , maybe [] (toXMLAttribute "Variant") $ check_variant x
                       , maybe [] (toXMLAttribute "Category") $ check_category x
                       , maybe [] (toXMLAttribute "Effort") $ check_effort x
                       , maybe [] (toXMLAttribute "DetectedAnomOpConds") $ check_detectedAnomOpConds x
                       ]
            [ concatMap (schemaTypeToXML "Errors") $ check_errors x
            , concatMap (schemaTypeToXML "Reviews") $ check_reviews x
            ]
instance Extension Check IToolChainElement where
    supertype v = IToolChainElement_Check v
instance Extension Check IIdentifiable where
    supertype = (supertype :: IToolChainElement -> IIdentifiable)
              . (supertype :: Check -> IToolChainElement)
              
 
data Probability
    = Probability_LOW
    | Probability_MEDIUM
    | Probability_HIGH
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType Probability where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Probability where
    acceptingParser =  do literal "LOW"; return Probability_LOW
                      `onFail` do literal "MEDIUM"; return Probability_MEDIUM
                      `onFail` do literal "HIGH"; return Probability_HIGH
                      
    simpleTypeText Probability_LOW = "LOW"
    simpleTypeText Probability_MEDIUM = "MEDIUM"
    simpleTypeText Probability_HIGH = "HIGH"
 
data ToolChain = ToolChain
        { toolChain_iD :: Maybe Ecore.EString
        , toolChain_name :: Maybe Ecore.EString
        , toolChain_description :: Maybe Ecore.EString
        , toolChain_longDescription :: Maybe Ecore.EString
        , toolChain_comment :: Maybe Ecore.EString
        , toolChain_partOf :: Maybe Xsd.AnyURI
        , toolChain_toolAttributes :: Maybe Xsd.XsdString
        , toolChain_inputs :: Maybe Xsd.XsdString
        , toolChain_outputs :: Maybe Xsd.XsdString
        , toolChain_inputsOutputs :: Maybe Xsd.XsdString
        , toolChain_calledTools :: Maybe Xsd.XsdString
        , toolChain_callingTools :: Maybe Xsd.XsdString
        , toolChain_variant :: Maybe Xsd.XsdString
        , toolChain_knownBugManagement :: Maybe Ecore.EString
        , toolChain_knownBugDate :: Maybe Ecore.EString
        , toolChain_numberOfExecutions :: Maybe Ecore.EInt
        , toolChain_internalReference :: Maybe Ecore.EString
        , toolChain_numberOfSetups :: Maybe Ecore.EInt
        , toolChain_supportedMethods :: Maybe Xsd.XsdString
        , toolChain_superTool :: Maybe Xsd.AnyURI
        , toolChain_anomalousOpCondHandling :: Maybe Ecore.EString
        , toolChain_toolOwner :: Maybe Ecore.EString
        , toolChain_toolProvider :: Maybe Ecore.EString
        , toolChain_path :: Maybe Ecore.EString
        , toolChain_aSIL :: Maybe ASIL
        , toolChain_useAssumptions :: Maybe Ecore.EBoolean
        , toolChain_showOnlyAssumptions :: Maybe Ecore.EBoolean
        , toolChain_ignoreArtifacts :: Maybe Ecore.EBoolean
        , toolChain_defaultAssumptionValueForNewElements :: Maybe Ecore.EBoolean
        , toolChain_showErrorStatistics :: Maybe Ecore.EBoolean
        , toolChain_activeVariants :: Maybe Xsd.XsdString
        , toolChain_createMissingIDsDuringValidation :: Maybe Ecore.EBoolean
        , toolChain_riskLevel :: Maybe RiskLevel
        , toolChain_safetyStandard :: Maybe SafetyStandard
        , toolChain_useCases :: [UseCase]
        , toolChain_costs :: [ToolCosts]
        , toolChain_qualifications :: [Qualification]
        , toolChain_tests :: [Test]
        , toolChain_identifications :: [Identification]
        , toolChain_knownBugs :: [KnownBug]
        , toolChain_qualificationProjects :: [QualificationProject]
        , toolChain_reviews :: [Review]
        , toolChain_testRuns :: [TestRun]
        , toolChain_artifacts :: [Artifact]
        , toolChain_anomalousOpConds :: [AnomalousOpCond]
        , toolChain_parameters :: [Parameter]
        , toolChain_tools :: [Tool]
        , toolChain_defaultErrorAttributes :: Maybe DefaultErrorAttributes
        , toolChain_variants :: [Variant]
        , toolChain_standardMethods :: [StandardMethods]
        , toolChain_progressTrackingStates :: [ProgressTrackingState]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType ToolChain where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "PartOf" e pos
        a6 <- optional $ getAttribute "ToolAttributes" e pos
        a7 <- optional $ getAttribute "Inputs" e pos
        a8 <- optional $ getAttribute "Outputs" e pos
        a9 <- optional $ getAttribute "InputsOutputs" e pos
        a10 <- optional $ getAttribute "CalledTools" e pos
        a11 <- optional $ getAttribute "CallingTools" e pos
        a12 <- optional $ getAttribute "Variant" e pos
        a13 <- optional $ getAttribute "KnownBugManagement" e pos
        a14 <- optional $ getAttribute "KnownBugDate" e pos
        a15 <- optional $ getAttribute "NumberOfExecutions" e pos
        a16 <- optional $ getAttribute "InternalReference" e pos
        a17 <- optional $ getAttribute "NumberOfSetups" e pos
        a18 <- optional $ getAttribute "SupportedMethods" e pos
        a19 <- optional $ getAttribute "SuperTool" e pos
        a20 <- optional $ getAttribute "AnomalousOpCondHandling" e pos
        a21 <- optional $ getAttribute "ToolOwner" e pos
        a22 <- optional $ getAttribute "ToolProvider" e pos
        a23 <- optional $ getAttribute "Path" e pos
        a24 <- optional $ getAttribute "ASIL" e pos
        a25 <- optional $ getAttribute "UseAssumptions" e pos
        a26 <- optional $ getAttribute "ShowOnlyAssumptions" e pos
        a27 <- optional $ getAttribute "IgnoreArtifacts" e pos
        a28 <- optional $ getAttribute "DefaultAssumptionValueForNewElements" e pos
        a29 <- optional $ getAttribute "ShowErrorStatistics" e pos
        a30 <- optional $ getAttribute "ActiveVariants" e pos
        a31 <- optional $ getAttribute "CreateMissingIDsDuringValidation" e pos
        a32 <- optional $ getAttribute "RiskLevel" e pos
        a33 <- optional $ getAttribute "SafetyStandard" e pos
        commit $ interior e $ return (ToolChain a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33)
            `apply` many (parseSchemaType "UseCases")
            `apply` many (parseSchemaType "Costs")
            `apply` many (parseSchemaType "Qualifications")
            `apply` many (parseSchemaType "Tests")
            `apply` many (parseSchemaType "Identifications")
            `apply` many (parseSchemaType "KnownBugs")
            `apply` many (parseSchemaType "QualificationProjects")
            `apply` many (parseSchemaType "Reviews")
            `apply` many (parseSchemaType "TestRuns")
            `apply` many (parseSchemaType "Artifacts")
            `apply` many (parseSchemaType "AnomalousOpConds")
            `apply` many (parseSchemaType "Parameters")
            `apply` many (parseSchemaType "Tools")
            `apply` optional (parseSchemaType "DefaultErrorAttributes")
            `apply` many (parseSchemaType "Variants")
            `apply` many (parseSchemaType "StandardMethods")
            `apply` many (parseSchemaType "ProgressTrackingStates")
    schemaTypeToXML s x@ToolChain{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ toolChain_iD x
                       , maybe [] (toXMLAttribute "Name") $ toolChain_name x
                       , maybe [] (toXMLAttribute "Description") $ toolChain_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ toolChain_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ toolChain_comment x
                       , maybe [] (toXMLAttribute "PartOf") $ toolChain_partOf x
                       , maybe [] (toXMLAttribute "ToolAttributes") $ toolChain_toolAttributes x
                       , maybe [] (toXMLAttribute "Inputs") $ toolChain_inputs x
                       , maybe [] (toXMLAttribute "Outputs") $ toolChain_outputs x
                       , maybe [] (toXMLAttribute "InputsOutputs") $ toolChain_inputsOutputs x
                       , maybe [] (toXMLAttribute "CalledTools") $ toolChain_calledTools x
                       , maybe [] (toXMLAttribute "CallingTools") $ toolChain_callingTools x
                       , maybe [] (toXMLAttribute "Variant") $ toolChain_variant x
                       , maybe [] (toXMLAttribute "KnownBugManagement") $ toolChain_knownBugManagement x
                       , maybe [] (toXMLAttribute "KnownBugDate") $ toolChain_knownBugDate x
                       , maybe [] (toXMLAttribute "NumberOfExecutions") $ toolChain_numberOfExecutions x
                       , maybe [] (toXMLAttribute "InternalReference") $ toolChain_internalReference x
                       , maybe [] (toXMLAttribute "NumberOfSetups") $ toolChain_numberOfSetups x
                       , maybe [] (toXMLAttribute "SupportedMethods") $ toolChain_supportedMethods x
                       , maybe [] (toXMLAttribute "SuperTool") $ toolChain_superTool x
                       , maybe [] (toXMLAttribute "AnomalousOpCondHandling") $ toolChain_anomalousOpCondHandling x
                       , maybe [] (toXMLAttribute "ToolOwner") $ toolChain_toolOwner x
                       , maybe [] (toXMLAttribute "ToolProvider") $ toolChain_toolProvider x
                       , maybe [] (toXMLAttribute "Path") $ toolChain_path x
                       , maybe [] (toXMLAttribute "ASIL") $ toolChain_aSIL x
                       , maybe [] (toXMLAttribute "UseAssumptions") $ toolChain_useAssumptions x
                       , maybe [] (toXMLAttribute "ShowOnlyAssumptions") $ toolChain_showOnlyAssumptions x
                       , maybe [] (toXMLAttribute "IgnoreArtifacts") $ toolChain_ignoreArtifacts x
                       , maybe [] (toXMLAttribute "DefaultAssumptionValueForNewElements") $ toolChain_defaultAssumptionValueForNewElements x
                       , maybe [] (toXMLAttribute "ShowErrorStatistics") $ toolChain_showErrorStatistics x
                       , maybe [] (toXMLAttribute "ActiveVariants") $ toolChain_activeVariants x
                       , maybe [] (toXMLAttribute "CreateMissingIDsDuringValidation") $ toolChain_createMissingIDsDuringValidation x
                       , maybe [] (toXMLAttribute "RiskLevel") $ toolChain_riskLevel x
                       , maybe [] (toXMLAttribute "SafetyStandard") $ toolChain_safetyStandard x
                       ]
            [ concatMap (schemaTypeToXML "UseCases") $ toolChain_useCases x
            , concatMap (schemaTypeToXML "Costs") $ toolChain_costs x
            , concatMap (schemaTypeToXML "Qualifications") $ toolChain_qualifications x
            , concatMap (schemaTypeToXML "Tests") $ toolChain_tests x
            , concatMap (schemaTypeToXML "Identifications") $ toolChain_identifications x
            , concatMap (schemaTypeToXML "KnownBugs") $ toolChain_knownBugs x
            , concatMap (schemaTypeToXML "QualificationProjects") $ toolChain_qualificationProjects x
            , concatMap (schemaTypeToXML "Reviews") $ toolChain_reviews x
            , concatMap (schemaTypeToXML "TestRuns") $ toolChain_testRuns x
            , concatMap (schemaTypeToXML "Artifacts") $ toolChain_artifacts x
            , concatMap (schemaTypeToXML "AnomalousOpConds") $ toolChain_anomalousOpConds x
            , concatMap (schemaTypeToXML "Parameters") $ toolChain_parameters x
            , concatMap (schemaTypeToXML "Tools") $ toolChain_tools x
            , maybe [] (schemaTypeToXML "DefaultErrorAttributes") $ toolChain_defaultErrorAttributes x
            , concatMap (schemaTypeToXML "Variants") $ toolChain_variants x
            , concatMap (schemaTypeToXML "StandardMethods") $ toolChain_standardMethods x
            , concatMap (schemaTypeToXML "ProgressTrackingStates") $ toolChain_progressTrackingStates x
            ]
instance Extension ToolChain Tool where
    supertype (ToolChain a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 e0 e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13 e14 e15 e16) =
               Tool a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 e0 e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11
instance Extension ToolChain IToolChainElement where
    supertype = (supertype :: Tool -> IToolChainElement)
              . (supertype :: ToolChain -> Tool)
              
instance Extension ToolChain IIdentifiable where
    supertype = (supertype :: IToolChainElement -> IIdentifiable)
              . (supertype :: Tool -> IToolChainElement)
              . (supertype :: ToolChain -> Tool)
              
 
data QualificationMethod
    = QualificationMethod_UNQUALIFIED
    | QualificationMethod_CONFIDENCE_FROM_USE
    | QualificationMethod_EVALUATION_OF_PROCESS
    | QualificationMethod_VALIDATION
    | QualificationMethod_SAFETY_STANDARD
    | QualificationMethod_OTHER_METHOD_FOR_ASIL_A
    | QualificationMethod_OTHER_METHOD_FOR_ASIL_B
    | QualificationMethod_OTHER_METHOD_FOR_ASIL_C
    | QualificationMethod_OTHER_METHOD_FOR_ASIL_D
    | QualificationMethod_DO_330_TQL_1
    | QualificationMethod_DO_330_TQL_2
    | QualificationMethod_DO_330_TQL_3
    | QualificationMethod_DO_330_TQL_4
    | QualificationMethod_DO_330_TQL_5
    | QualificationMethod_FAILURE_ANALYSIS
    | QualificationMethod_FAILURE_MITIGATION
    | QualificationMethod_VALIDAS_QKIT
    | QualificationMethod_VALIDAS_CKIT
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType QualificationMethod where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType QualificationMethod where
    acceptingParser =  do literal "UNQUALIFIED"; return QualificationMethod_UNQUALIFIED
                      `onFail` do literal "CONFIDENCE_FROM_USE"; return QualificationMethod_CONFIDENCE_FROM_USE
                      `onFail` do literal "EVALUATION_OF_PROCESS"; return QualificationMethod_EVALUATION_OF_PROCESS
                      `onFail` do literal "VALIDATION"; return QualificationMethod_VALIDATION
                      `onFail` do literal "SAFETY_STANDARD"; return QualificationMethod_SAFETY_STANDARD
                      `onFail` do literal "OTHER_METHOD_FOR_ASIL_A"; return QualificationMethod_OTHER_METHOD_FOR_ASIL_A
                      `onFail` do literal "OTHER_METHOD_FOR_ASIL_B"; return QualificationMethod_OTHER_METHOD_FOR_ASIL_B
                      `onFail` do literal "OTHER_METHOD_FOR_ASIL_C"; return QualificationMethod_OTHER_METHOD_FOR_ASIL_C
                      `onFail` do literal "OTHER_METHOD_FOR_ASIL_D"; return QualificationMethod_OTHER_METHOD_FOR_ASIL_D
                      `onFail` do literal "DO_330_TQL_1"; return QualificationMethod_DO_330_TQL_1
                      `onFail` do literal "DO_330_TQL_2"; return QualificationMethod_DO_330_TQL_2
                      `onFail` do literal "DO_330_TQL_3"; return QualificationMethod_DO_330_TQL_3
                      `onFail` do literal "DO_330_TQL_4"; return QualificationMethod_DO_330_TQL_4
                      `onFail` do literal "DO_330_TQL_5"; return QualificationMethod_DO_330_TQL_5
                      `onFail` do literal "FAILURE_ANALYSIS"; return QualificationMethod_FAILURE_ANALYSIS
                      `onFail` do literal "FAILURE_MITIGATION"; return QualificationMethod_FAILURE_MITIGATION
                      `onFail` do literal "VALIDAS_QKIT"; return QualificationMethod_VALIDAS_QKIT
                      `onFail` do literal "VALIDAS_CKIT"; return QualificationMethod_VALIDAS_CKIT
                      
    simpleTypeText QualificationMethod_UNQUALIFIED = "UNQUALIFIED"
    simpleTypeText QualificationMethod_CONFIDENCE_FROM_USE = "CONFIDENCE_FROM_USE"
    simpleTypeText QualificationMethod_EVALUATION_OF_PROCESS = "EVALUATION_OF_PROCESS"
    simpleTypeText QualificationMethod_VALIDATION = "VALIDATION"
    simpleTypeText QualificationMethod_SAFETY_STANDARD = "SAFETY_STANDARD"
    simpleTypeText QualificationMethod_OTHER_METHOD_FOR_ASIL_A = "OTHER_METHOD_FOR_ASIL_A"
    simpleTypeText QualificationMethod_OTHER_METHOD_FOR_ASIL_B = "OTHER_METHOD_FOR_ASIL_B"
    simpleTypeText QualificationMethod_OTHER_METHOD_FOR_ASIL_C = "OTHER_METHOD_FOR_ASIL_C"
    simpleTypeText QualificationMethod_OTHER_METHOD_FOR_ASIL_D = "OTHER_METHOD_FOR_ASIL_D"
    simpleTypeText QualificationMethod_DO_330_TQL_1 = "DO_330_TQL_1"
    simpleTypeText QualificationMethod_DO_330_TQL_2 = "DO_330_TQL_2"
    simpleTypeText QualificationMethod_DO_330_TQL_3 = "DO_330_TQL_3"
    simpleTypeText QualificationMethod_DO_330_TQL_4 = "DO_330_TQL_4"
    simpleTypeText QualificationMethod_DO_330_TQL_5 = "DO_330_TQL_5"
    simpleTypeText QualificationMethod_FAILURE_ANALYSIS = "FAILURE_ANALYSIS"
    simpleTypeText QualificationMethod_FAILURE_MITIGATION = "FAILURE_MITIGATION"
    simpleTypeText QualificationMethod_VALIDAS_QKIT = "VALIDAS_QKIT"
    simpleTypeText QualificationMethod_VALIDAS_CKIT = "VALIDAS_CKIT"
 
data ASIL
    = ASIL_A
    | ASIL_B
    | ASIL_C
    | ASIL_D
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType ASIL where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ASIL where
    acceptingParser =  do literal "A"; return ASIL_A
                      `onFail` do literal "B"; return ASIL_B
                      `onFail` do literal "C"; return ASIL_C
                      `onFail` do literal "D"; return ASIL_D
                      
    simpleTypeText ASIL_A = "A"
    simpleTypeText ASIL_B = "B"
    simpleTypeText ASIL_C = "C"
    simpleTypeText ASIL_D = "D"
 
data Restriction = Restriction
        { restriction_iD :: Maybe Ecore.EString
        , restriction_name :: Maybe Ecore.EString
        , restriction_description :: Maybe Ecore.EString
        , restriction_longDescription :: Maybe Ecore.EString
        , restriction_comment :: Maybe Ecore.EString
        , restriction_restrictionProbability :: Maybe Probability
        , restriction_useCase :: Maybe Xsd.AnyURI
        , restriction_avoidedError :: Maybe Xsd.XsdString
        , restriction_attribute :: Maybe Xsd.AnyURI
        , restriction_subsumes :: Maybe Xsd.XsdString
        , restriction_isSubsumedFrom :: Maybe Xsd.XsdString
        , restriction_internalReference :: Maybe Ecore.EString
        , restriction_avoidedKnownBugs :: Maybe Xsd.XsdString
        , restriction_variant :: Maybe Xsd.XsdString
        , restriction_category :: Maybe Category
        , restriction_effort :: Maybe Effort
        , restriction_avoidedAnomOpConds :: Maybe Xsd.XsdString
        , restriction_errors :: [Error]
        , restriction_reviews :: [Review]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType Restriction where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "RestrictionProbability" e pos
        a6 <- optional $ getAttribute "UseCase" e pos
        a7 <- optional $ getAttribute "AvoidedError" e pos
        a8 <- optional $ getAttribute "Attribute" e pos
        a9 <- optional $ getAttribute "Subsumes" e pos
        a10 <- optional $ getAttribute "IsSubsumedFrom" e pos
        a11 <- optional $ getAttribute "InternalReference" e pos
        a12 <- optional $ getAttribute "AvoidedKnownBugs" e pos
        a13 <- optional $ getAttribute "Variant" e pos
        a14 <- optional $ getAttribute "Category" e pos
        a15 <- optional $ getAttribute "Effort" e pos
        a16 <- optional $ getAttribute "AvoidedAnomOpConds" e pos
        commit $ interior e $ return (Restriction a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16)
            `apply` many (parseSchemaType "Errors")
            `apply` many (parseSchemaType "Reviews")
    schemaTypeToXML s x@Restriction{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ restriction_iD x
                       , maybe [] (toXMLAttribute "Name") $ restriction_name x
                       , maybe [] (toXMLAttribute "Description") $ restriction_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ restriction_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ restriction_comment x
                       , maybe [] (toXMLAttribute "RestrictionProbability") $ restriction_restrictionProbability x
                       , maybe [] (toXMLAttribute "UseCase") $ restriction_useCase x
                       , maybe [] (toXMLAttribute "AvoidedError") $ restriction_avoidedError x
                       , maybe [] (toXMLAttribute "Attribute") $ restriction_attribute x
                       , maybe [] (toXMLAttribute "Subsumes") $ restriction_subsumes x
                       , maybe [] (toXMLAttribute "IsSubsumedFrom") $ restriction_isSubsumedFrom x
                       , maybe [] (toXMLAttribute "InternalReference") $ restriction_internalReference x
                       , maybe [] (toXMLAttribute "AvoidedKnownBugs") $ restriction_avoidedKnownBugs x
                       , maybe [] (toXMLAttribute "Variant") $ restriction_variant x
                       , maybe [] (toXMLAttribute "Category") $ restriction_category x
                       , maybe [] (toXMLAttribute "Effort") $ restriction_effort x
                       , maybe [] (toXMLAttribute "AvoidedAnomOpConds") $ restriction_avoidedAnomOpConds x
                       ]
            [ concatMap (schemaTypeToXML "Errors") $ restriction_errors x
            , concatMap (schemaTypeToXML "Reviews") $ restriction_reviews x
            ]
instance Extension Restriction IToolChainElement where
    supertype v = IToolChainElement_Restriction v
instance Extension Restriction IIdentifiable where
    supertype = (supertype :: IToolChainElement -> IIdentifiable)
              . (supertype :: Restriction -> IToolChainElement)
              
 
data Costs
        = Costs_UseCaseCosts UseCaseCosts
        | Costs_ToolCosts ToolCosts
        
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType Costs where
    parseSchemaType s = do
        (fmap Costs_UseCaseCosts $ parseSchemaType s)
        `onFail`
        (fmap Costs_ToolCosts $ parseSchemaType s)
        `onFail` fail "Parse failed when expecting an extension type of Costs,\n\
\  namely one of:\n\
\UseCaseCosts,ToolCosts"
    schemaTypeToXML _s (Costs_UseCaseCosts x) = schemaTypeToXML "useCaseCosts" x
    schemaTypeToXML _s (Costs_ToolCosts x) = schemaTypeToXML "toolCosts" x
instance Extension Costs IIdentifiable where
    supertype v = IIdentifiable_Costs v
 
data ToolCosts = ToolCosts
        { toolCosts_iD :: Maybe Ecore.EString
        , toolCosts_name :: Maybe Ecore.EString
        , toolCosts_description :: Maybe Ecore.EString
        , toolCosts_longDescription :: Maybe Ecore.EString
        , toolCosts_comment :: Maybe Ecore.EString
        , toolCosts_qualificationCosts :: Maybe Ecore.EInt
        , toolCosts_numberOfLicences :: Maybe Ecore.EInt
        , toolCosts_numberOfApplications :: Maybe Ecore.EInt
        , toolCosts_price :: Maybe Ecore.EInt
        , toolCosts_applicationCost :: Maybe Ecore.EInt
        , toolCosts_supportCost :: Maybe Ecore.EInt
        , toolCosts_installationEffort :: Maybe Ecore.EInt
        , toolCosts_cost :: Maybe Ecore.EFloat
        , toolCosts_type :: Maybe CostType
        , toolCosts_unit :: Maybe CostUnit
        , toolCosts_tool :: Maybe Xsd.AnyURI
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType ToolCosts where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "QualificationCosts" e pos
        a6 <- optional $ getAttribute "NumberOfLicences" e pos
        a7 <- optional $ getAttribute "NumberOfApplications" e pos
        a8 <- optional $ getAttribute "Price" e pos
        a9 <- optional $ getAttribute "ApplicationCost" e pos
        a10 <- optional $ getAttribute "SupportCost" e pos
        a11 <- optional $ getAttribute "InstallationEffort" e pos
        a12 <- optional $ getAttribute "Cost" e pos
        a13 <- optional $ getAttribute "Type" e pos
        a14 <- optional $ getAttribute "Unit" e pos
        a15 <- optional $ getAttribute "Tool" e pos
        commit $ interior e $ return (ToolCosts a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
    schemaTypeToXML s x@ToolCosts{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ toolCosts_iD x
                       , maybe [] (toXMLAttribute "Name") $ toolCosts_name x
                       , maybe [] (toXMLAttribute "Description") $ toolCosts_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ toolCosts_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ toolCosts_comment x
                       , maybe [] (toXMLAttribute "QualificationCosts") $ toolCosts_qualificationCosts x
                       , maybe [] (toXMLAttribute "NumberOfLicences") $ toolCosts_numberOfLicences x
                       , maybe [] (toXMLAttribute "NumberOfApplications") $ toolCosts_numberOfApplications x
                       , maybe [] (toXMLAttribute "Price") $ toolCosts_price x
                       , maybe [] (toXMLAttribute "ApplicationCost") $ toolCosts_applicationCost x
                       , maybe [] (toXMLAttribute "SupportCost") $ toolCosts_supportCost x
                       , maybe [] (toXMLAttribute "InstallationEffort") $ toolCosts_installationEffort x
                       , maybe [] (toXMLAttribute "Cost") $ toolCosts_cost x
                       , maybe [] (toXMLAttribute "Type") $ toolCosts_type x
                       , maybe [] (toXMLAttribute "Unit") $ toolCosts_unit x
                       , maybe [] (toXMLAttribute "Tool") $ toolCosts_tool x
                       ]
            []
instance Extension ToolCosts Costs where
    supertype v = Costs_ToolCosts v
instance Extension ToolCosts IIdentifiable where
    supertype = (supertype :: Costs -> IIdentifiable)
              . (supertype :: ToolCosts -> Costs)
              
 
data UseCaseCosts = UseCaseCosts
        { useCaseCosts_iD :: Maybe Ecore.EString
        , useCaseCosts_name :: Maybe Ecore.EString
        , useCaseCosts_description :: Maybe Ecore.EString
        , useCaseCosts_longDescription :: Maybe Ecore.EString
        , useCaseCosts_comment :: Maybe Ecore.EString
        , useCaseCosts_qualificationCosts :: Maybe Ecore.EInt
        , useCaseCosts_numberOfLicences :: Maybe Ecore.EInt
        , useCaseCosts_numberOfApplications :: Maybe Ecore.EInt
        , useCaseCosts_price :: Maybe Ecore.EInt
        , useCaseCosts_applicationCost :: Maybe Ecore.EInt
        , useCaseCosts_supportCost :: Maybe Ecore.EInt
        , useCaseCosts_installationEffort :: Maybe Ecore.EInt
        , useCaseCosts_cost :: Maybe Ecore.EFloat
        , useCaseCosts_type :: Maybe CostType
        , useCaseCosts_unit :: Maybe CostUnit
        , useCaseCosts_useCase :: Maybe Xsd.AnyURI
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType UseCaseCosts where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "QualificationCosts" e pos
        a6 <- optional $ getAttribute "NumberOfLicences" e pos
        a7 <- optional $ getAttribute "NumberOfApplications" e pos
        a8 <- optional $ getAttribute "Price" e pos
        a9 <- optional $ getAttribute "ApplicationCost" e pos
        a10 <- optional $ getAttribute "SupportCost" e pos
        a11 <- optional $ getAttribute "InstallationEffort" e pos
        a12 <- optional $ getAttribute "Cost" e pos
        a13 <- optional $ getAttribute "Type" e pos
        a14 <- optional $ getAttribute "Unit" e pos
        a15 <- optional $ getAttribute "UseCase" e pos
        commit $ interior e $ return (UseCaseCosts a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
    schemaTypeToXML s x@UseCaseCosts{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ useCaseCosts_iD x
                       , maybe [] (toXMLAttribute "Name") $ useCaseCosts_name x
                       , maybe [] (toXMLAttribute "Description") $ useCaseCosts_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ useCaseCosts_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ useCaseCosts_comment x
                       , maybe [] (toXMLAttribute "QualificationCosts") $ useCaseCosts_qualificationCosts x
                       , maybe [] (toXMLAttribute "NumberOfLicences") $ useCaseCosts_numberOfLicences x
                       , maybe [] (toXMLAttribute "NumberOfApplications") $ useCaseCosts_numberOfApplications x
                       , maybe [] (toXMLAttribute "Price") $ useCaseCosts_price x
                       , maybe [] (toXMLAttribute "ApplicationCost") $ useCaseCosts_applicationCost x
                       , maybe [] (toXMLAttribute "SupportCost") $ useCaseCosts_supportCost x
                       , maybe [] (toXMLAttribute "InstallationEffort") $ useCaseCosts_installationEffort x
                       , maybe [] (toXMLAttribute "Cost") $ useCaseCosts_cost x
                       , maybe [] (toXMLAttribute "Type") $ useCaseCosts_type x
                       , maybe [] (toXMLAttribute "Unit") $ useCaseCosts_unit x
                       , maybe [] (toXMLAttribute "UseCase") $ useCaseCosts_useCase x
                       ]
            []
instance Extension UseCaseCosts Costs where
    supertype v = Costs_UseCaseCosts v
instance Extension UseCaseCosts IIdentifiable where
    supertype = (supertype :: Costs -> IIdentifiable)
              . (supertype :: UseCaseCosts -> Costs)
              
 
data Qualification = Qualification
        { qualification_iD :: Maybe Ecore.EString
        , qualification_name :: Maybe Ecore.EString
        , qualification_description :: Maybe Ecore.EString
        , qualification_longDescription :: Maybe Ecore.EString
        , qualification_comment :: Maybe Ecore.EString
        , qualification_date :: Maybe Ecore.EString
        , qualification_tool :: Maybe Xsd.AnyURI
        , qualification_useCases :: Maybe Xsd.XsdString
        , qualification_variant :: Maybe Xsd.XsdString
        , qualification_tests :: Maybe Xsd.XsdString
        , qualification_internalReference :: Maybe Ecore.EString
        , qualification_identifications :: Maybe Xsd.XsdString
        , qualification_errors :: Maybe Xsd.XsdString
        , qualification_responsible :: Maybe Ecore.EString
        , qualification_qualificationResult :: Maybe QualificationResult
        , qualification_method :: Maybe QualificationMethod
        , qualification_reviews :: [Review]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType Qualification where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "Date" e pos
        a6 <- optional $ getAttribute "Tool" e pos
        a7 <- optional $ getAttribute "UseCases" e pos
        a8 <- optional $ getAttribute "Variant" e pos
        a9 <- optional $ getAttribute "Tests" e pos
        a10 <- optional $ getAttribute "InternalReference" e pos
        a11 <- optional $ getAttribute "Identifications" e pos
        a12 <- optional $ getAttribute "Errors" e pos
        a13 <- optional $ getAttribute "Responsible" e pos
        a14 <- optional $ getAttribute "QualificationResult" e pos
        a15 <- optional $ getAttribute "Method" e pos
        commit $ interior e $ return (Qualification a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
            `apply` many (parseSchemaType "Reviews")
    schemaTypeToXML s x@Qualification{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ qualification_iD x
                       , maybe [] (toXMLAttribute "Name") $ qualification_name x
                       , maybe [] (toXMLAttribute "Description") $ qualification_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ qualification_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ qualification_comment x
                       , maybe [] (toXMLAttribute "Date") $ qualification_date x
                       , maybe [] (toXMLAttribute "Tool") $ qualification_tool x
                       , maybe [] (toXMLAttribute "UseCases") $ qualification_useCases x
                       , maybe [] (toXMLAttribute "Variant") $ qualification_variant x
                       , maybe [] (toXMLAttribute "Tests") $ qualification_tests x
                       , maybe [] (toXMLAttribute "InternalReference") $ qualification_internalReference x
                       , maybe [] (toXMLAttribute "Identifications") $ qualification_identifications x
                       , maybe [] (toXMLAttribute "Errors") $ qualification_errors x
                       , maybe [] (toXMLAttribute "Responsible") $ qualification_responsible x
                       , maybe [] (toXMLAttribute "QualificationResult") $ qualification_qualificationResult x
                       , maybe [] (toXMLAttribute "Method") $ qualification_method x
                       ]
            [ concatMap (schemaTypeToXML "Reviews") $ qualification_reviews x
            ]
instance Extension Qualification IToolChainElement where
    supertype v = IToolChainElement_Qualification v
instance Extension Qualification IIdentifiable where
    supertype = (supertype :: IToolChainElement -> IIdentifiable)
              . (supertype :: Qualification -> IToolChainElement)
              
 
data Feature = Feature
        { feature_iD :: Maybe Ecore.EString
        , feature_name :: Maybe Ecore.EString
        , feature_description :: Maybe Ecore.EString
        , feature_longDescription :: Maybe Ecore.EString
        , feature_comment :: Maybe Ecore.EString
        , feature_tool :: Maybe Xsd.AnyURI
        , feature_inputs :: Maybe Xsd.XsdString
        , feature_outputs :: Maybe Xsd.XsdString
        , feature_inputsOutputs :: Maybe Xsd.XsdString
        , feature_qualifications :: Maybe Xsd.XsdString
        , feature_mandatoryFeatures :: Maybe Xsd.XsdString
        , feature_excludedFeatures :: Maybe Xsd.XsdString
        , feature_requiredFeatures :: Maybe Xsd.XsdString
        , feature_expandedAttributes :: Maybe Xsd.XsdString
        , feature_partOf :: Maybe Xsd.AnyURI
        , feature_calledUseCases :: Maybe Xsd.XsdString
        , feature_callingUseCases :: Maybe Xsd.XsdString
        , feature_showPartsErrors :: Maybe Ecore.EBoolean
        , feature_showPartOfErrors :: Maybe Ecore.EBoolean
        , feature_variant :: Maybe Xsd.XsdString
        , feature_tests :: Maybe Xsd.XsdString
        , feature_numberOfExecutions :: Maybe Ecore.EInt
        , feature_internalReference :: Maybe Ecore.EString
        , feature_numberOfSetups :: Maybe Ecore.EInt
        , feature_attributes :: Maybe Xsd.XsdString
          -- ^ This is called Attribues instead of ToolAttributes for 
          --   compatibily reasons with older versions
        , feature_knownBugs :: Maybe Xsd.XsdString
        , feature_commandLineArgument :: Maybe Ecore.EString
        , feature_testRuns :: Maybe Xsd.XsdString
        , feature_qualificationResult :: Maybe QualificationResult
        , feature_testResults :: Maybe Xsd.XsdString
        , feature_path :: Maybe Ecore.EString
        , feature_inputPattern :: Maybe Ecore.EString
        , feature_argumentPattern :: Maybe Ecore.EString
        , feature_useCases :: Maybe Xsd.XsdString
        , feature_supportedMethods :: Maybe Xsd.XsdString
        , feature_attribute :: Maybe Xsd.AnyURI
        , feature_state :: Maybe Xsd.AnyURI
        , feature_relevantCLParameters :: Maybe Xsd.XsdString
        , feature_errors :: [Error]
        , feature_checks :: [Check]
        , feature_restrictions :: [Restriction]
        , feature_costs :: [UseCaseCosts]
        , feature_parts :: [UseCase]
        , feature_inferredErrors :: [InferredError]
        , feature_inferredChecks :: [InferredCheck]
        , feature_inferredRestrictions :: [InferredRestriction]
        , feature_reviews :: [Review]
        , feature_parameters :: [Parameter]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType Feature where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "Tool" e pos
        a6 <- optional $ getAttribute "Inputs" e pos
        a7 <- optional $ getAttribute "Outputs" e pos
        a8 <- optional $ getAttribute "InputsOutputs" e pos
        a9 <- optional $ getAttribute "Qualifications" e pos
        a10 <- optional $ getAttribute "MandatoryFeatures" e pos
        a11 <- optional $ getAttribute "ExcludedFeatures" e pos
        a12 <- optional $ getAttribute "RequiredFeatures" e pos
        a13 <- optional $ getAttribute "ExpandedAttributes" e pos
        a14 <- optional $ getAttribute "PartOf" e pos
        a15 <- optional $ getAttribute "CalledUseCases" e pos
        a16 <- optional $ getAttribute "CallingUseCases" e pos
        a17 <- optional $ getAttribute "ShowPartsErrors" e pos
        a18 <- optional $ getAttribute "ShowPartOfErrors" e pos
        a19 <- optional $ getAttribute "Variant" e pos
        a20 <- optional $ getAttribute "Tests" e pos
        a21 <- optional $ getAttribute "NumberOfExecutions" e pos
        a22 <- optional $ getAttribute "InternalReference" e pos
        a23 <- optional $ getAttribute "NumberOfSetups" e pos
        a24 <- optional $ getAttribute "Attributes" e pos
        a25 <- optional $ getAttribute "KnownBugs" e pos
        a26 <- optional $ getAttribute "CommandLineArgument" e pos
        a27 <- optional $ getAttribute "TestRuns" e pos
        a28 <- optional $ getAttribute "QualificationResult" e pos
        a29 <- optional $ getAttribute "testResults" e pos
        a30 <- optional $ getAttribute "Path" e pos
        a31 <- optional $ getAttribute "InputPattern" e pos
        a32 <- optional $ getAttribute "ArgumentPattern" e pos
        a33 <- optional $ getAttribute "UseCases" e pos
        a34 <- optional $ getAttribute "SupportedMethods" e pos
        a35 <- optional $ getAttribute "Attribute" e pos
        a36 <- optional $ getAttribute "State" e pos
        a37 <- optional $ getAttribute "RelevantCLParameters" e pos
        commit $ interior e $ return (Feature a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37)
            `apply` many (parseSchemaType "Errors")
            `apply` many (parseSchemaType "Checks")
            `apply` many (parseSchemaType "Restrictions")
            `apply` many (parseSchemaType "Costs")
            `apply` many (parseSchemaType "Parts")
            `apply` many (parseSchemaType "InferredErrors")
            `apply` many (parseSchemaType "InferredChecks")
            `apply` many (parseSchemaType "InferredRestrictions")
            `apply` many (parseSchemaType "Reviews")
            `apply` many (parseSchemaType "Parameters")
    schemaTypeToXML s x@Feature{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ feature_iD x
                       , maybe [] (toXMLAttribute "Name") $ feature_name x
                       , maybe [] (toXMLAttribute "Description") $ feature_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ feature_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ feature_comment x
                       , maybe [] (toXMLAttribute "Tool") $ feature_tool x
                       , maybe [] (toXMLAttribute "Inputs") $ feature_inputs x
                       , maybe [] (toXMLAttribute "Outputs") $ feature_outputs x
                       , maybe [] (toXMLAttribute "InputsOutputs") $ feature_inputsOutputs x
                       , maybe [] (toXMLAttribute "Qualifications") $ feature_qualifications x
                       , maybe [] (toXMLAttribute "MandatoryFeatures") $ feature_mandatoryFeatures x
                       , maybe [] (toXMLAttribute "ExcludedFeatures") $ feature_excludedFeatures x
                       , maybe [] (toXMLAttribute "RequiredFeatures") $ feature_requiredFeatures x
                       , maybe [] (toXMLAttribute "ExpandedAttributes") $ feature_expandedAttributes x
                       , maybe [] (toXMLAttribute "PartOf") $ feature_partOf x
                       , maybe [] (toXMLAttribute "CalledUseCases") $ feature_calledUseCases x
                       , maybe [] (toXMLAttribute "CallingUseCases") $ feature_callingUseCases x
                       , maybe [] (toXMLAttribute "ShowPartsErrors") $ feature_showPartsErrors x
                       , maybe [] (toXMLAttribute "ShowPartOfErrors") $ feature_showPartOfErrors x
                       , maybe [] (toXMLAttribute "Variant") $ feature_variant x
                       , maybe [] (toXMLAttribute "Tests") $ feature_tests x
                       , maybe [] (toXMLAttribute "NumberOfExecutions") $ feature_numberOfExecutions x
                       , maybe [] (toXMLAttribute "InternalReference") $ feature_internalReference x
                       , maybe [] (toXMLAttribute "NumberOfSetups") $ feature_numberOfSetups x
                       , maybe [] (toXMLAttribute "Attributes") $ feature_attributes x
                       , maybe [] (toXMLAttribute "KnownBugs") $ feature_knownBugs x
                       , maybe [] (toXMLAttribute "CommandLineArgument") $ feature_commandLineArgument x
                       , maybe [] (toXMLAttribute "TestRuns") $ feature_testRuns x
                       , maybe [] (toXMLAttribute "QualificationResult") $ feature_qualificationResult x
                       , maybe [] (toXMLAttribute "testResults") $ feature_testResults x
                       , maybe [] (toXMLAttribute "Path") $ feature_path x
                       , maybe [] (toXMLAttribute "InputPattern") $ feature_inputPattern x
                       , maybe [] (toXMLAttribute "ArgumentPattern") $ feature_argumentPattern x
                       , maybe [] (toXMLAttribute "UseCases") $ feature_useCases x
                       , maybe [] (toXMLAttribute "SupportedMethods") $ feature_supportedMethods x
                       , maybe [] (toXMLAttribute "Attribute") $ feature_attribute x
                       , maybe [] (toXMLAttribute "State") $ feature_state x
                       , maybe [] (toXMLAttribute "RelevantCLParameters") $ feature_relevantCLParameters x
                       ]
            [ concatMap (schemaTypeToXML "Errors") $ feature_errors x
            , concatMap (schemaTypeToXML "Checks") $ feature_checks x
            , concatMap (schemaTypeToXML "Restrictions") $ feature_restrictions x
            , concatMap (schemaTypeToXML "Costs") $ feature_costs x
            , concatMap (schemaTypeToXML "Parts") $ feature_parts x
            , concatMap (schemaTypeToXML "InferredErrors") $ feature_inferredErrors x
            , concatMap (schemaTypeToXML "InferredChecks") $ feature_inferredChecks x
            , concatMap (schemaTypeToXML "InferredRestrictions") $ feature_inferredRestrictions x
            , concatMap (schemaTypeToXML "Reviews") $ feature_reviews x
            , concatMap (schemaTypeToXML "Parameters") $ feature_parameters x
            ]
instance Extension Feature UseCase where
    supertype (Feature a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32 a33 a34 a35 a36 a37 e0 e1 e2 e3 e4 e5 e6 e7 e8 e9) =
               UseCase a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 e0 e1 e2 e3 e4 e5 e6 e7 e8 e9
instance Extension Feature IToolChainElement where
    supertype = (supertype :: UseCase -> IToolChainElement)
              . (supertype :: Feature -> UseCase)
              
instance Extension Feature IIdentifiable where
    supertype = (supertype :: IToolChainElement -> IIdentifiable)
              . (supertype :: UseCase -> IToolChainElement)
              . (supertype :: Feature -> UseCase)
              
 
data InferredRestriction = InferredRestriction
        { inferredRestriction_inferredOccurrences :: Maybe Xsd.XsdString
        , inferredRestriction_sourceOccurrence :: Maybe Xsd.AnyURI
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType InferredRestriction where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "InferredOccurrences" e pos
        a1 <- optional $ getAttribute "SourceOccurrence" e pos
        commit $ interior e $ return (InferredRestriction a0 a1)
    schemaTypeToXML s x@InferredRestriction{} =
        toXMLElement s [ maybe [] (toXMLAttribute "InferredOccurrences") $ inferredRestriction_inferredOccurrences x
                       , maybe [] (toXMLAttribute "SourceOccurrence") $ inferredRestriction_sourceOccurrence x
                       ]
            []
instance Extension InferredRestriction RestrictionOccurrence where
    supertype v = RestrictionOccurrence_InferredRestriction v
 
data InferredError = InferredError
        { inferredError_inferredOccurrences :: Maybe Xsd.XsdString
        , inferredError_sourceOccurrence :: Maybe Xsd.AnyURI
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType InferredError where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "InferredOccurrences" e pos
        a1 <- optional $ getAttribute "SourceOccurrence" e pos
        commit $ interior e $ return (InferredError a0 a1)
    schemaTypeToXML s x@InferredError{} =
        toXMLElement s [ maybe [] (toXMLAttribute "InferredOccurrences") $ inferredError_inferredOccurrences x
                       , maybe [] (toXMLAttribute "SourceOccurrence") $ inferredError_sourceOccurrence x
                       ]
            []
instance Extension InferredError ErrorOccurrence where
    supertype v = ErrorOccurrence_InferredError v
 
data InferredCheck = InferredCheck
        { inferredCheck_inferredOccurrences :: Maybe Xsd.XsdString
        , inferredCheck_sourceOccurrence :: Maybe Xsd.AnyURI
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType InferredCheck where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "InferredOccurrences" e pos
        a1 <- optional $ getAttribute "SourceOccurrence" e pos
        commit $ interior e $ return (InferredCheck a0 a1)
    schemaTypeToXML s x@InferredCheck{} =
        toXMLElement s [ maybe [] (toXMLAttribute "InferredOccurrences") $ inferredCheck_inferredOccurrences x
                       , maybe [] (toXMLAttribute "SourceOccurrence") $ inferredCheck_sourceOccurrence x
                       ]
            []
instance Extension InferredCheck CheckOccurrence where
    supertype v = CheckOccurrence_InferredCheck v
 
data Attribute
        = Attribute_ArtifactAttribute ArtifactAttribute
        | Attribute_ToolAttribute ToolAttribute
        
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType Attribute where
    parseSchemaType s = do
        (fmap Attribute_ArtifactAttribute $ parseSchemaType s)
        `onFail`
        (fmap Attribute_ToolAttribute $ parseSchemaType s)
        `onFail` fail "Parse failed when expecting an extension type of Attribute,\n\
\  namely one of:\n\
\ArtifactAttribute,ToolAttribute"
    schemaTypeToXML _s (Attribute_ArtifactAttribute x) = schemaTypeToXML "artifactAttribute" x
    schemaTypeToXML _s (Attribute_ToolAttribute x) = schemaTypeToXML "toolAttribute" x
instance Extension Attribute IIdentifiable where
    supertype v = IIdentifiable_Attribute v
 
data DefaultErrorAttributes = DefaultErrorAttributes
        { defaultErrorAttributes_toolChain :: Maybe Xsd.AnyURI
        , defaultErrorAttributes_attributes :: [Attribute]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType DefaultErrorAttributes where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ToolChain" e pos
        commit $ interior e $ return (DefaultErrorAttributes a0)
            `apply` many (parseSchemaType "Attributes")
    schemaTypeToXML s x@DefaultErrorAttributes{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ToolChain") $ defaultErrorAttributes_toolChain x
                       ]
            [ concatMap (schemaTypeToXML "Attributes") $ defaultErrorAttributes_attributes x
            ]
 
data ToolAttribute = ToolAttribute
        { toolAttribute_iD :: Maybe Ecore.EString
        , toolAttribute_name :: Maybe Ecore.EString
        , toolAttribute_description :: Maybe Ecore.EString
        , toolAttribute_longDescription :: Maybe Ecore.EString
        , toolAttribute_comment :: Maybe Ecore.EString
        , toolAttribute_defaultErrorAttributes :: Maybe Xsd.AnyURI
        , toolAttribute_internalReference :: Maybe Ecore.EString
        , toolAttribute_tools :: Maybe Xsd.XsdString
        , toolAttribute_useCases :: Maybe Xsd.XsdString
        , toolAttribute_errors :: [Error]
        , toolAttribute_checks :: [Check]
        , toolAttribute_restrictions :: [Restriction]
        , toolAttribute_features :: [Feature]
        , toolAttribute_tests :: [Test]
        , toolAttribute_reviews :: [Review]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType ToolAttribute where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "DefaultErrorAttributes" e pos
        a6 <- optional $ getAttribute "InternalReference" e pos
        a7 <- optional $ getAttribute "Tools" e pos
        a8 <- optional $ getAttribute "UseCases" e pos
        commit $ interior e $ return (ToolAttribute a0 a1 a2 a3 a4 a5 a6 a7 a8)
            `apply` many (parseSchemaType "Errors")
            `apply` many (parseSchemaType "Checks")
            `apply` many (parseSchemaType "Restrictions")
            `apply` many (parseSchemaType "Features")
            `apply` many (parseSchemaType "Tests")
            `apply` many (parseSchemaType "Reviews")
    schemaTypeToXML s x@ToolAttribute{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ toolAttribute_iD x
                       , maybe [] (toXMLAttribute "Name") $ toolAttribute_name x
                       , maybe [] (toXMLAttribute "Description") $ toolAttribute_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ toolAttribute_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ toolAttribute_comment x
                       , maybe [] (toXMLAttribute "DefaultErrorAttributes") $ toolAttribute_defaultErrorAttributes x
                       , maybe [] (toXMLAttribute "InternalReference") $ toolAttribute_internalReference x
                       , maybe [] (toXMLAttribute "Tools") $ toolAttribute_tools x
                       , maybe [] (toXMLAttribute "UseCases") $ toolAttribute_useCases x
                       ]
            [ concatMap (schemaTypeToXML "Errors") $ toolAttribute_errors x
            , concatMap (schemaTypeToXML "Checks") $ toolAttribute_checks x
            , concatMap (schemaTypeToXML "Restrictions") $ toolAttribute_restrictions x
            , concatMap (schemaTypeToXML "Features") $ toolAttribute_features x
            , concatMap (schemaTypeToXML "Tests") $ toolAttribute_tests x
            , concatMap (schemaTypeToXML "Reviews") $ toolAttribute_reviews x
            ]
instance Extension ToolAttribute Attribute where
    supertype v = Attribute_ToolAttribute v
instance Extension ToolAttribute IIdentifiable where
    supertype = (supertype :: Attribute -> IIdentifiable)
              . (supertype :: ToolAttribute -> Attribute)
              
 
data ArtifactAttribute = ArtifactAttribute
        { artifactAttribute_iD :: Maybe Ecore.EString
        , artifactAttribute_name :: Maybe Ecore.EString
        , artifactAttribute_description :: Maybe Ecore.EString
        , artifactAttribute_longDescription :: Maybe Ecore.EString
        , artifactAttribute_comment :: Maybe Ecore.EString
        , artifactAttribute_defaultErrorAttributes :: Maybe Xsd.AnyURI
        , artifactAttribute_internalReference :: Maybe Ecore.EString
        , artifactAttribute_artifacts :: Maybe Xsd.XsdString
        , artifactAttribute_errors :: [Error]
        , artifactAttribute_checks :: [Check]
        , artifactAttribute_restrictions :: [Restriction]
        , artifactAttribute_features :: [Feature]
        , artifactAttribute_tests :: [Test]
        , artifactAttribute_reviews :: [Review]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType ArtifactAttribute where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "DefaultErrorAttributes" e pos
        a6 <- optional $ getAttribute "InternalReference" e pos
        a7 <- optional $ getAttribute "Artifacts" e pos
        commit $ interior e $ return (ArtifactAttribute a0 a1 a2 a3 a4 a5 a6 a7)
            `apply` many (parseSchemaType "Errors")
            `apply` many (parseSchemaType "Checks")
            `apply` many (parseSchemaType "Restrictions")
            `apply` many (parseSchemaType "Features")
            `apply` many (parseSchemaType "Tests")
            `apply` many (parseSchemaType "Reviews")
    schemaTypeToXML s x@ArtifactAttribute{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ artifactAttribute_iD x
                       , maybe [] (toXMLAttribute "Name") $ artifactAttribute_name x
                       , maybe [] (toXMLAttribute "Description") $ artifactAttribute_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ artifactAttribute_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ artifactAttribute_comment x
                       , maybe [] (toXMLAttribute "DefaultErrorAttributes") $ artifactAttribute_defaultErrorAttributes x
                       , maybe [] (toXMLAttribute "InternalReference") $ artifactAttribute_internalReference x
                       , maybe [] (toXMLAttribute "Artifacts") $ artifactAttribute_artifacts x
                       ]
            [ concatMap (schemaTypeToXML "Errors") $ artifactAttribute_errors x
            , concatMap (schemaTypeToXML "Checks") $ artifactAttribute_checks x
            , concatMap (schemaTypeToXML "Restrictions") $ artifactAttribute_restrictions x
            , concatMap (schemaTypeToXML "Features") $ artifactAttribute_features x
            , concatMap (schemaTypeToXML "Tests") $ artifactAttribute_tests x
            , concatMap (schemaTypeToXML "Reviews") $ artifactAttribute_reviews x
            ]
instance Extension ArtifactAttribute Attribute where
    supertype v = Attribute_ArtifactAttribute v
instance Extension ArtifactAttribute IIdentifiable where
    supertype = (supertype :: Attribute -> IIdentifiable)
              . (supertype :: ArtifactAttribute -> Attribute)
              
 
data ErrorOccurrence
        = ErrorOccurrence_InferredError InferredError
        | ErrorOccurrence_Error Error
        
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType ErrorOccurrence where
    parseSchemaType s = do
        (fmap ErrorOccurrence_InferredError $ parseSchemaType s)
        `onFail`
        (fmap ErrorOccurrence_Error $ parseSchemaType s)
        `onFail` fail "Parse failed when expecting an extension type of ErrorOccurrence,\n\
\  namely one of:\n\
\InferredError,Error"
    schemaTypeToXML _s (ErrorOccurrence_InferredError x) = schemaTypeToXML "inferredError" x
    schemaTypeToXML _s (ErrorOccurrence_Error x) = schemaTypeToXML "error" x
 
data CheckOccurrence
        = CheckOccurrence_InferredCheck InferredCheck
        
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType CheckOccurrence where
    parseSchemaType s = do
        (fmap CheckOccurrence_InferredCheck $ parseSchemaType s)
        `onFail` fail "Parse failed when expecting an extension type of CheckOccurrence,\n\
\  namely one of:\n\
\InferredCheck"
    schemaTypeToXML _s (CheckOccurrence_InferredCheck x) = schemaTypeToXML "inferredCheck" x
 
data RestrictionOccurrence
        = RestrictionOccurrence_InferredRestriction InferredRestriction
        
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType RestrictionOccurrence where
    parseSchemaType s = do
        (fmap RestrictionOccurrence_InferredRestriction $ parseSchemaType s)
        `onFail` fail "Parse failed when expecting an extension type of RestrictionOccurrence,\n\
\  namely one of:\n\
\InferredRestriction"
    schemaTypeToXML _s (RestrictionOccurrence_InferredRestriction x) = schemaTypeToXML "inferredRestriction" x
 
data Variant = Variant
        { variant_iD :: Maybe Ecore.EString
        , variant_name :: Maybe Ecore.EString
        , variant_description :: Maybe Ecore.EString
        , variant_longDescription :: Maybe Ecore.EString
        , variant_comment :: Maybe Ecore.EString
        , variant_toolChain :: Maybe Xsd.AnyURI
        , variant_tools :: Maybe Xsd.XsdString
        , variant_artifacts :: Maybe Xsd.XsdString
        , variant_useCases :: Maybe Xsd.XsdString
        , variant_qualifications :: Maybe Xsd.XsdString
        , variant_internalReference :: Maybe Ecore.EString
        , variant_checks :: Maybe Xsd.XsdString
        , variant_errors :: Maybe Xsd.XsdString
        , variant_restrictions :: Maybe Xsd.XsdString
        , variant_tests :: Maybe Xsd.XsdString
        , variant_knownBugs :: Maybe Xsd.XsdString
        , variant_qualificationProjects :: Maybe Xsd.XsdString
        , variant_qualificationRoles :: Maybe Xsd.XsdString
        , variant_qualificationSteps :: Maybe Xsd.XsdString
        , variant_qualificationArtifacts :: Maybe Xsd.XsdString
        , variant_identifications :: Maybe Xsd.XsdString
        , variant_testRuns :: Maybe Xsd.XsdString
        , variant_anomalousOpConds :: Maybe Xsd.XsdString
        , variant_parameters :: Maybe Xsd.XsdString
        , variant_enumValues :: Maybe Xsd.XsdString
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType Variant where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "ToolChain" e pos
        a6 <- optional $ getAttribute "Tools" e pos
        a7 <- optional $ getAttribute "Artifacts" e pos
        a8 <- optional $ getAttribute "UseCases" e pos
        a9 <- optional $ getAttribute "Qualifications" e pos
        a10 <- optional $ getAttribute "InternalReference" e pos
        a11 <- optional $ getAttribute "Checks" e pos
        a12 <- optional $ getAttribute "Errors" e pos
        a13 <- optional $ getAttribute "Restrictions" e pos
        a14 <- optional $ getAttribute "Tests" e pos
        a15 <- optional $ getAttribute "KnownBugs" e pos
        a16 <- optional $ getAttribute "QualificationProjects" e pos
        a17 <- optional $ getAttribute "QualificationRoles" e pos
        a18 <- optional $ getAttribute "QualificationSteps" e pos
        a19 <- optional $ getAttribute "QualificationArtifacts" e pos
        a20 <- optional $ getAttribute "Identifications" e pos
        a21 <- optional $ getAttribute "TestRuns" e pos
        a22 <- optional $ getAttribute "AnomalousOpConds" e pos
        a23 <- optional $ getAttribute "Parameters" e pos
        a24 <- optional $ getAttribute "EnumValues" e pos
        commit $ interior e $ return (Variant a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24)
    schemaTypeToXML s x@Variant{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ variant_iD x
                       , maybe [] (toXMLAttribute "Name") $ variant_name x
                       , maybe [] (toXMLAttribute "Description") $ variant_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ variant_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ variant_comment x
                       , maybe [] (toXMLAttribute "ToolChain") $ variant_toolChain x
                       , maybe [] (toXMLAttribute "Tools") $ variant_tools x
                       , maybe [] (toXMLAttribute "Artifacts") $ variant_artifacts x
                       , maybe [] (toXMLAttribute "UseCases") $ variant_useCases x
                       , maybe [] (toXMLAttribute "Qualifications") $ variant_qualifications x
                       , maybe [] (toXMLAttribute "InternalReference") $ variant_internalReference x
                       , maybe [] (toXMLAttribute "Checks") $ variant_checks x
                       , maybe [] (toXMLAttribute "Errors") $ variant_errors x
                       , maybe [] (toXMLAttribute "Restrictions") $ variant_restrictions x
                       , maybe [] (toXMLAttribute "Tests") $ variant_tests x
                       , maybe [] (toXMLAttribute "KnownBugs") $ variant_knownBugs x
                       , maybe [] (toXMLAttribute "QualificationProjects") $ variant_qualificationProjects x
                       , maybe [] (toXMLAttribute "QualificationRoles") $ variant_qualificationRoles x
                       , maybe [] (toXMLAttribute "QualificationSteps") $ variant_qualificationSteps x
                       , maybe [] (toXMLAttribute "QualificationArtifacts") $ variant_qualificationArtifacts x
                       , maybe [] (toXMLAttribute "Identifications") $ variant_identifications x
                       , maybe [] (toXMLAttribute "TestRuns") $ variant_testRuns x
                       , maybe [] (toXMLAttribute "AnomalousOpConds") $ variant_anomalousOpConds x
                       , maybe [] (toXMLAttribute "Parameters") $ variant_parameters x
                       , maybe [] (toXMLAttribute "EnumValues") $ variant_enumValues x
                       ]
            []
instance Extension Variant IIdentifiable where
    supertype v = IIdentifiable_Variant v
 
data ORVariant = ORVariant
        { oRVariant_iD :: Maybe Ecore.EString
        , oRVariant_name :: Maybe Ecore.EString
        , oRVariant_description :: Maybe Ecore.EString
        , oRVariant_longDescription :: Maybe Ecore.EString
        , oRVariant_comment :: Maybe Ecore.EString
        , oRVariant_toolChain :: Maybe Xsd.AnyURI
        , oRVariant_tools :: Maybe Xsd.XsdString
        , oRVariant_artifacts :: Maybe Xsd.XsdString
        , oRVariant_useCases :: Maybe Xsd.XsdString
        , oRVariant_qualifications :: Maybe Xsd.XsdString
        , oRVariant_internalReference :: Maybe Ecore.EString
        , oRVariant_checks :: Maybe Xsd.XsdString
        , oRVariant_errors :: Maybe Xsd.XsdString
        , oRVariant_restrictions :: Maybe Xsd.XsdString
        , oRVariant_tests :: Maybe Xsd.XsdString
        , oRVariant_knownBugs :: Maybe Xsd.XsdString
        , oRVariant_qualificationProjects :: Maybe Xsd.XsdString
        , oRVariant_qualificationRoles :: Maybe Xsd.XsdString
        , oRVariant_qualificationSteps :: Maybe Xsd.XsdString
        , oRVariant_qualificationArtifacts :: Maybe Xsd.XsdString
        , oRVariant_identifications :: Maybe Xsd.XsdString
        , oRVariant_testRuns :: Maybe Xsd.XsdString
        , oRVariant_anomalousOpConds :: Maybe Xsd.XsdString
        , oRVariant_parameters :: Maybe Xsd.XsdString
        , oRVariant_enumValues :: Maybe Xsd.XsdString
        , oRVariant_variants :: Xsd.XsdString
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType ORVariant where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "ToolChain" e pos
        a6 <- optional $ getAttribute "Tools" e pos
        a7 <- optional $ getAttribute "Artifacts" e pos
        a8 <- optional $ getAttribute "UseCases" e pos
        a9 <- optional $ getAttribute "Qualifications" e pos
        a10 <- optional $ getAttribute "InternalReference" e pos
        a11 <- optional $ getAttribute "Checks" e pos
        a12 <- optional $ getAttribute "Errors" e pos
        a13 <- optional $ getAttribute "Restrictions" e pos
        a14 <- optional $ getAttribute "Tests" e pos
        a15 <- optional $ getAttribute "KnownBugs" e pos
        a16 <- optional $ getAttribute "QualificationProjects" e pos
        a17 <- optional $ getAttribute "QualificationRoles" e pos
        a18 <- optional $ getAttribute "QualificationSteps" e pos
        a19 <- optional $ getAttribute "QualificationArtifacts" e pos
        a20 <- optional $ getAttribute "Identifications" e pos
        a21 <- optional $ getAttribute "TestRuns" e pos
        a22 <- optional $ getAttribute "AnomalousOpConds" e pos
        a23 <- optional $ getAttribute "Parameters" e pos
        a24 <- optional $ getAttribute "EnumValues" e pos
        a25 <- getAttribute "Variants" e pos
        commit $ interior e $ return (ORVariant a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25)
    schemaTypeToXML s x@ORVariant{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ oRVariant_iD x
                       , maybe [] (toXMLAttribute "Name") $ oRVariant_name x
                       , maybe [] (toXMLAttribute "Description") $ oRVariant_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ oRVariant_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ oRVariant_comment x
                       , maybe [] (toXMLAttribute "ToolChain") $ oRVariant_toolChain x
                       , maybe [] (toXMLAttribute "Tools") $ oRVariant_tools x
                       , maybe [] (toXMLAttribute "Artifacts") $ oRVariant_artifacts x
                       , maybe [] (toXMLAttribute "UseCases") $ oRVariant_useCases x
                       , maybe [] (toXMLAttribute "Qualifications") $ oRVariant_qualifications x
                       , maybe [] (toXMLAttribute "InternalReference") $ oRVariant_internalReference x
                       , maybe [] (toXMLAttribute "Checks") $ oRVariant_checks x
                       , maybe [] (toXMLAttribute "Errors") $ oRVariant_errors x
                       , maybe [] (toXMLAttribute "Restrictions") $ oRVariant_restrictions x
                       , maybe [] (toXMLAttribute "Tests") $ oRVariant_tests x
                       , maybe [] (toXMLAttribute "KnownBugs") $ oRVariant_knownBugs x
                       , maybe [] (toXMLAttribute "QualificationProjects") $ oRVariant_qualificationProjects x
                       , maybe [] (toXMLAttribute "QualificationRoles") $ oRVariant_qualificationRoles x
                       , maybe [] (toXMLAttribute "QualificationSteps") $ oRVariant_qualificationSteps x
                       , maybe [] (toXMLAttribute "QualificationArtifacts") $ oRVariant_qualificationArtifacts x
                       , maybe [] (toXMLAttribute "Identifications") $ oRVariant_identifications x
                       , maybe [] (toXMLAttribute "TestRuns") $ oRVariant_testRuns x
                       , maybe [] (toXMLAttribute "AnomalousOpConds") $ oRVariant_anomalousOpConds x
                       , maybe [] (toXMLAttribute "Parameters") $ oRVariant_parameters x
                       , maybe [] (toXMLAttribute "EnumValues") $ oRVariant_enumValues x
                       , toXMLAttribute "Variants" $ oRVariant_variants x
                       ]
            []
instance Extension ORVariant Variant where
    supertype (ORVariant a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25) =
               Variant a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24
instance Extension ORVariant IIdentifiable where
    supertype = (supertype :: Variant -> IIdentifiable)
              . (supertype :: ORVariant -> Variant)
              
 
data OneOfVariant = OneOfVariant
        { oneOfVariant_iD :: Maybe Ecore.EString
        , oneOfVariant_name :: Maybe Ecore.EString
        , oneOfVariant_description :: Maybe Ecore.EString
        , oneOfVariant_longDescription :: Maybe Ecore.EString
        , oneOfVariant_comment :: Maybe Ecore.EString
        , oneOfVariant_toolChain :: Maybe Xsd.AnyURI
        , oneOfVariant_tools :: Maybe Xsd.XsdString
        , oneOfVariant_artifacts :: Maybe Xsd.XsdString
        , oneOfVariant_useCases :: Maybe Xsd.XsdString
        , oneOfVariant_qualifications :: Maybe Xsd.XsdString
        , oneOfVariant_internalReference :: Maybe Ecore.EString
        , oneOfVariant_checks :: Maybe Xsd.XsdString
        , oneOfVariant_errors :: Maybe Xsd.XsdString
        , oneOfVariant_restrictions :: Maybe Xsd.XsdString
        , oneOfVariant_tests :: Maybe Xsd.XsdString
        , oneOfVariant_knownBugs :: Maybe Xsd.XsdString
        , oneOfVariant_qualificationProjects :: Maybe Xsd.XsdString
        , oneOfVariant_qualificationRoles :: Maybe Xsd.XsdString
        , oneOfVariant_qualificationSteps :: Maybe Xsd.XsdString
        , oneOfVariant_qualificationArtifacts :: Maybe Xsd.XsdString
        , oneOfVariant_identifications :: Maybe Xsd.XsdString
        , oneOfVariant_testRuns :: Maybe Xsd.XsdString
        , oneOfVariant_anomalousOpConds :: Maybe Xsd.XsdString
        , oneOfVariant_parameters :: Maybe Xsd.XsdString
        , oneOfVariant_enumValues :: Maybe Xsd.XsdString
        , oneOfVariant_variants :: Xsd.XsdString
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType OneOfVariant where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "ToolChain" e pos
        a6 <- optional $ getAttribute "Tools" e pos
        a7 <- optional $ getAttribute "Artifacts" e pos
        a8 <- optional $ getAttribute "UseCases" e pos
        a9 <- optional $ getAttribute "Qualifications" e pos
        a10 <- optional $ getAttribute "InternalReference" e pos
        a11 <- optional $ getAttribute "Checks" e pos
        a12 <- optional $ getAttribute "Errors" e pos
        a13 <- optional $ getAttribute "Restrictions" e pos
        a14 <- optional $ getAttribute "Tests" e pos
        a15 <- optional $ getAttribute "KnownBugs" e pos
        a16 <- optional $ getAttribute "QualificationProjects" e pos
        a17 <- optional $ getAttribute "QualificationRoles" e pos
        a18 <- optional $ getAttribute "QualificationSteps" e pos
        a19 <- optional $ getAttribute "QualificationArtifacts" e pos
        a20 <- optional $ getAttribute "Identifications" e pos
        a21 <- optional $ getAttribute "TestRuns" e pos
        a22 <- optional $ getAttribute "AnomalousOpConds" e pos
        a23 <- optional $ getAttribute "Parameters" e pos
        a24 <- optional $ getAttribute "EnumValues" e pos
        a25 <- getAttribute "Variants" e pos
        commit $ interior e $ return (OneOfVariant a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25)
    schemaTypeToXML s x@OneOfVariant{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ oneOfVariant_iD x
                       , maybe [] (toXMLAttribute "Name") $ oneOfVariant_name x
                       , maybe [] (toXMLAttribute "Description") $ oneOfVariant_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ oneOfVariant_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ oneOfVariant_comment x
                       , maybe [] (toXMLAttribute "ToolChain") $ oneOfVariant_toolChain x
                       , maybe [] (toXMLAttribute "Tools") $ oneOfVariant_tools x
                       , maybe [] (toXMLAttribute "Artifacts") $ oneOfVariant_artifacts x
                       , maybe [] (toXMLAttribute "UseCases") $ oneOfVariant_useCases x
                       , maybe [] (toXMLAttribute "Qualifications") $ oneOfVariant_qualifications x
                       , maybe [] (toXMLAttribute "InternalReference") $ oneOfVariant_internalReference x
                       , maybe [] (toXMLAttribute "Checks") $ oneOfVariant_checks x
                       , maybe [] (toXMLAttribute "Errors") $ oneOfVariant_errors x
                       , maybe [] (toXMLAttribute "Restrictions") $ oneOfVariant_restrictions x
                       , maybe [] (toXMLAttribute "Tests") $ oneOfVariant_tests x
                       , maybe [] (toXMLAttribute "KnownBugs") $ oneOfVariant_knownBugs x
                       , maybe [] (toXMLAttribute "QualificationProjects") $ oneOfVariant_qualificationProjects x
                       , maybe [] (toXMLAttribute "QualificationRoles") $ oneOfVariant_qualificationRoles x
                       , maybe [] (toXMLAttribute "QualificationSteps") $ oneOfVariant_qualificationSteps x
                       , maybe [] (toXMLAttribute "QualificationArtifacts") $ oneOfVariant_qualificationArtifacts x
                       , maybe [] (toXMLAttribute "Identifications") $ oneOfVariant_identifications x
                       , maybe [] (toXMLAttribute "TestRuns") $ oneOfVariant_testRuns x
                       , maybe [] (toXMLAttribute "AnomalousOpConds") $ oneOfVariant_anomalousOpConds x
                       , maybe [] (toXMLAttribute "Parameters") $ oneOfVariant_parameters x
                       , maybe [] (toXMLAttribute "EnumValues") $ oneOfVariant_enumValues x
                       , toXMLAttribute "Variants" $ oneOfVariant_variants x
                       ]
            []
instance Extension OneOfVariant Variant where
    supertype (OneOfVariant a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25) =
               Variant a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24
instance Extension OneOfVariant IIdentifiable where
    supertype = (supertype :: Variant -> IIdentifiable)
              . (supertype :: OneOfVariant -> Variant)
              
 
data ANDVariant = ANDVariant
        { aNDVariant_iD :: Maybe Ecore.EString
        , aNDVariant_name :: Maybe Ecore.EString
        , aNDVariant_description :: Maybe Ecore.EString
        , aNDVariant_longDescription :: Maybe Ecore.EString
        , aNDVariant_comment :: Maybe Ecore.EString
        , aNDVariant_toolChain :: Maybe Xsd.AnyURI
        , aNDVariant_tools :: Maybe Xsd.XsdString
        , aNDVariant_artifacts :: Maybe Xsd.XsdString
        , aNDVariant_useCases :: Maybe Xsd.XsdString
        , aNDVariant_qualifications :: Maybe Xsd.XsdString
        , aNDVariant_internalReference :: Maybe Ecore.EString
        , aNDVariant_checks :: Maybe Xsd.XsdString
        , aNDVariant_errors :: Maybe Xsd.XsdString
        , aNDVariant_restrictions :: Maybe Xsd.XsdString
        , aNDVariant_tests :: Maybe Xsd.XsdString
        , aNDVariant_knownBugs :: Maybe Xsd.XsdString
        , aNDVariant_qualificationProjects :: Maybe Xsd.XsdString
        , aNDVariant_qualificationRoles :: Maybe Xsd.XsdString
        , aNDVariant_qualificationSteps :: Maybe Xsd.XsdString
        , aNDVariant_qualificationArtifacts :: Maybe Xsd.XsdString
        , aNDVariant_identifications :: Maybe Xsd.XsdString
        , aNDVariant_testRuns :: Maybe Xsd.XsdString
        , aNDVariant_anomalousOpConds :: Maybe Xsd.XsdString
        , aNDVariant_parameters :: Maybe Xsd.XsdString
        , aNDVariant_enumValues :: Maybe Xsd.XsdString
        , aNDVariant_variants :: Xsd.XsdString
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType ANDVariant where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "ToolChain" e pos
        a6 <- optional $ getAttribute "Tools" e pos
        a7 <- optional $ getAttribute "Artifacts" e pos
        a8 <- optional $ getAttribute "UseCases" e pos
        a9 <- optional $ getAttribute "Qualifications" e pos
        a10 <- optional $ getAttribute "InternalReference" e pos
        a11 <- optional $ getAttribute "Checks" e pos
        a12 <- optional $ getAttribute "Errors" e pos
        a13 <- optional $ getAttribute "Restrictions" e pos
        a14 <- optional $ getAttribute "Tests" e pos
        a15 <- optional $ getAttribute "KnownBugs" e pos
        a16 <- optional $ getAttribute "QualificationProjects" e pos
        a17 <- optional $ getAttribute "QualificationRoles" e pos
        a18 <- optional $ getAttribute "QualificationSteps" e pos
        a19 <- optional $ getAttribute "QualificationArtifacts" e pos
        a20 <- optional $ getAttribute "Identifications" e pos
        a21 <- optional $ getAttribute "TestRuns" e pos
        a22 <- optional $ getAttribute "AnomalousOpConds" e pos
        a23 <- optional $ getAttribute "Parameters" e pos
        a24 <- optional $ getAttribute "EnumValues" e pos
        a25 <- getAttribute "Variants" e pos
        commit $ interior e $ return (ANDVariant a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25)
    schemaTypeToXML s x@ANDVariant{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ aNDVariant_iD x
                       , maybe [] (toXMLAttribute "Name") $ aNDVariant_name x
                       , maybe [] (toXMLAttribute "Description") $ aNDVariant_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ aNDVariant_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ aNDVariant_comment x
                       , maybe [] (toXMLAttribute "ToolChain") $ aNDVariant_toolChain x
                       , maybe [] (toXMLAttribute "Tools") $ aNDVariant_tools x
                       , maybe [] (toXMLAttribute "Artifacts") $ aNDVariant_artifacts x
                       , maybe [] (toXMLAttribute "UseCases") $ aNDVariant_useCases x
                       , maybe [] (toXMLAttribute "Qualifications") $ aNDVariant_qualifications x
                       , maybe [] (toXMLAttribute "InternalReference") $ aNDVariant_internalReference x
                       , maybe [] (toXMLAttribute "Checks") $ aNDVariant_checks x
                       , maybe [] (toXMLAttribute "Errors") $ aNDVariant_errors x
                       , maybe [] (toXMLAttribute "Restrictions") $ aNDVariant_restrictions x
                       , maybe [] (toXMLAttribute "Tests") $ aNDVariant_tests x
                       , maybe [] (toXMLAttribute "KnownBugs") $ aNDVariant_knownBugs x
                       , maybe [] (toXMLAttribute "QualificationProjects") $ aNDVariant_qualificationProjects x
                       , maybe [] (toXMLAttribute "QualificationRoles") $ aNDVariant_qualificationRoles x
                       , maybe [] (toXMLAttribute "QualificationSteps") $ aNDVariant_qualificationSteps x
                       , maybe [] (toXMLAttribute "QualificationArtifacts") $ aNDVariant_qualificationArtifacts x
                       , maybe [] (toXMLAttribute "Identifications") $ aNDVariant_identifications x
                       , maybe [] (toXMLAttribute "TestRuns") $ aNDVariant_testRuns x
                       , maybe [] (toXMLAttribute "AnomalousOpConds") $ aNDVariant_anomalousOpConds x
                       , maybe [] (toXMLAttribute "Parameters") $ aNDVariant_parameters x
                       , maybe [] (toXMLAttribute "EnumValues") $ aNDVariant_enumValues x
                       , toXMLAttribute "Variants" $ aNDVariant_variants x
                       ]
            []
instance Extension ANDVariant Variant where
    supertype (ANDVariant a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25) =
               Variant a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24
instance Extension ANDVariant IIdentifiable where
    supertype = (supertype :: Variant -> IIdentifiable)
              . (supertype :: ANDVariant -> Variant)
              
 
data Test = Test
        { test_iD :: Maybe Ecore.EString
        , test_name :: Maybe Ecore.EString
        , test_description :: Maybe Ecore.EString
        , test_longDescription :: Maybe Ecore.EString
        , test_comment :: Maybe Ecore.EString
        , test_errors :: Maybe Xsd.XsdString
        , test_superTest :: Maybe Xsd.AnyURI
        , test_useCases :: Maybe Xsd.XsdString
        , test_path :: Maybe Ecore.EString
        , test_qualifications :: Maybe Xsd.XsdString
        , test_tool :: Maybe Xsd.AnyURI
        , test_isRobustness :: Maybe Ecore.EBoolean
        , test_internalReference :: Maybe Ecore.EString
        , test_knownBugs :: Maybe Xsd.XsdString
        , test_variant :: Maybe Xsd.XsdString
        , test_attribute :: Maybe Xsd.AnyURI
        , test_testRuns :: Maybe Xsd.XsdString
        , test_testResults :: Maybe Xsd.XsdString
        , test_anomalousOpConds :: Maybe Xsd.XsdString
        , test_isManual :: Maybe Ecore.EBoolean
        , test_hasRequirementsBased :: Maybe Ecore.EBoolean
        , test_hasNegativeTest :: Maybe Ecore.EBoolean
        , test_hasBoundaryTest :: Maybe Ecore.EBoolean
        , test_hasEquivalenceTest :: Maybe Ecore.EBoolean
        , test_hasErrorGuessingTest :: Maybe Ecore.EBoolean
        , test_hasCodeCoverage :: Maybe CodeCoverageMetric
        , test_numberOfUnitTests :: Maybe Ecore.EInt
        , test_multipleExecution :: Maybe Ecore.EBoolean
        , test_subTests :: [Test]
        , test_reviews :: [Review]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType Test where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "Errors" e pos
        a6 <- optional $ getAttribute "SuperTest" e pos
        a7 <- optional $ getAttribute "UseCases" e pos
        a8 <- optional $ getAttribute "Path" e pos
        a9 <- optional $ getAttribute "Qualifications" e pos
        a10 <- optional $ getAttribute "Tool" e pos
        a11 <- optional $ getAttribute "IsRobustness" e pos
        a12 <- optional $ getAttribute "InternalReference" e pos
        a13 <- optional $ getAttribute "KnownBugs" e pos
        a14 <- optional $ getAttribute "Variant" e pos
        a15 <- optional $ getAttribute "Attribute" e pos
        a16 <- optional $ getAttribute "TestRuns" e pos
        a17 <- optional $ getAttribute "TestResults" e pos
        a18 <- optional $ getAttribute "AnomalousOpConds" e pos
        a19 <- optional $ getAttribute "IsManual" e pos
        a20 <- optional $ getAttribute "HasRequirementsBased" e pos
        a21 <- optional $ getAttribute "HasNegativeTest" e pos
        a22 <- optional $ getAttribute "HasBoundaryTest" e pos
        a23 <- optional $ getAttribute "HasEquivalenceTest" e pos
        a24 <- optional $ getAttribute "HasErrorGuessingTest" e pos
        a25 <- optional $ getAttribute "HasCodeCoverage" e pos
        a26 <- optional $ getAttribute "NumberOfUnitTests" e pos
        a27 <- optional $ getAttribute "multipleExecution" e pos
        commit $ interior e $ return (Test a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27)
            `apply` many (parseSchemaType "SubTests")
            `apply` many (parseSchemaType "Reviews")
    schemaTypeToXML s x@Test{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ test_iD x
                       , maybe [] (toXMLAttribute "Name") $ test_name x
                       , maybe [] (toXMLAttribute "Description") $ test_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ test_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ test_comment x
                       , maybe [] (toXMLAttribute "Errors") $ test_errors x
                       , maybe [] (toXMLAttribute "SuperTest") $ test_superTest x
                       , maybe [] (toXMLAttribute "UseCases") $ test_useCases x
                       , maybe [] (toXMLAttribute "Path") $ test_path x
                       , maybe [] (toXMLAttribute "Qualifications") $ test_qualifications x
                       , maybe [] (toXMLAttribute "Tool") $ test_tool x
                       , maybe [] (toXMLAttribute "IsRobustness") $ test_isRobustness x
                       , maybe [] (toXMLAttribute "InternalReference") $ test_internalReference x
                       , maybe [] (toXMLAttribute "KnownBugs") $ test_knownBugs x
                       , maybe [] (toXMLAttribute "Variant") $ test_variant x
                       , maybe [] (toXMLAttribute "Attribute") $ test_attribute x
                       , maybe [] (toXMLAttribute "TestRuns") $ test_testRuns x
                       , maybe [] (toXMLAttribute "TestResults") $ test_testResults x
                       , maybe [] (toXMLAttribute "AnomalousOpConds") $ test_anomalousOpConds x
                       , maybe [] (toXMLAttribute "IsManual") $ test_isManual x
                       , maybe [] (toXMLAttribute "HasRequirementsBased") $ test_hasRequirementsBased x
                       , maybe [] (toXMLAttribute "HasNegativeTest") $ test_hasNegativeTest x
                       , maybe [] (toXMLAttribute "HasBoundaryTest") $ test_hasBoundaryTest x
                       , maybe [] (toXMLAttribute "HasEquivalenceTest") $ test_hasEquivalenceTest x
                       , maybe [] (toXMLAttribute "HasErrorGuessingTest") $ test_hasErrorGuessingTest x
                       , maybe [] (toXMLAttribute "HasCodeCoverage") $ test_hasCodeCoverage x
                       , maybe [] (toXMLAttribute "NumberOfUnitTests") $ test_numberOfUnitTests x
                       , maybe [] (toXMLAttribute "multipleExecution") $ test_multipleExecution x
                       ]
            [ concatMap (schemaTypeToXML "SubTests") $ test_subTests x
            , concatMap (schemaTypeToXML "Reviews") $ test_reviews x
            ]
instance Extension Test IToolChainElement where
    supertype v = IToolChainElement_Test v
instance Extension Test IIdentifiable where
    supertype = (supertype :: IToolChainElement -> IIdentifiable)
              . (supertype :: Test -> IToolChainElement)
              
 
data CostType
    = CostType_FIXED_COSTS
    | CostType_COSTS_PER_EXECUTION
    | CostType_COSTS_PER_SETUP
    | CostType_COSTS_PER_YEAR
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType CostType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CostType where
    acceptingParser =  do literal "FIXED_COSTS"; return CostType_FIXED_COSTS
                      `onFail` do literal "COSTS_PER_EXECUTION"; return CostType_COSTS_PER_EXECUTION
                      `onFail` do literal "COSTS_PER_SETUP"; return CostType_COSTS_PER_SETUP
                      `onFail` do literal "COSTS_PER_YEAR"; return CostType_COSTS_PER_YEAR
                      
    simpleTypeText CostType_FIXED_COSTS = "FIXED_COSTS"
    simpleTypeText CostType_COSTS_PER_EXECUTION = "COSTS_PER_EXECUTION"
    simpleTypeText CostType_COSTS_PER_SETUP = "COSTS_PER_SETUP"
    simpleTypeText CostType_COSTS_PER_YEAR = "COSTS_PER_YEAR"
 
data CostUnit
    = CostUnit_MONEY
    | CostUnit_HUMAN_TIME
    | CostUnit_COMPUTER_TIME
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType CostUnit where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CostUnit where
    acceptingParser =  do literal "MONEY"; return CostUnit_MONEY
                      `onFail` do literal "HUMAN_TIME"; return CostUnit_HUMAN_TIME
                      `onFail` do literal "COMPUTER_TIME"; return CostUnit_COMPUTER_TIME
                      
    simpleTypeText CostUnit_MONEY = "MONEY"
    simpleTypeText CostUnit_HUMAN_TIME = "HUMAN_TIME"
    simpleTypeText CostUnit_COMPUTER_TIME = "COMPUTER_TIME"
 
data Identification = Identification
        { identification_iD :: Maybe Ecore.EString
        , identification_name :: Maybe Ecore.EString
        , identification_description :: Maybe Ecore.EString
        , identification_longDescription :: Maybe Ecore.EString
        , identification_comment :: Maybe Ecore.EString
        , identification_version :: Maybe Ecore.EString
        , identification_release :: Maybe Ecore.EString
        , identification_configuration :: Maybe Ecore.EString
        , identification_internalReference :: Maybe Ecore.EString
        , identification_tool :: Maybe Xsd.AnyURI
        , identification_feature :: Maybe Xsd.AnyURI
        , identification_qualification :: Maybe Xsd.AnyURI
        , identification_environment :: Maybe Ecore.EString
        , identification_installation :: Maybe Ecore.EString
        , identification_documentation :: Maybe Ecore.EString
        , identification_knownBugManagement :: Maybe Ecore.EString
        , identification_variant :: Maybe Xsd.XsdString
        , identification_knownBugs :: Maybe Xsd.XsdString
        , identification_knownBugList :: Maybe Ecore.EString
        , identification_testRuns :: Maybe Xsd.XsdString
        , identification_coveredByVV :: Maybe Ecore.EBoolean
        , identification_nextIdentification :: Maybe Xsd.AnyURI
        , identification_reviews :: [Review]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType Identification where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "Version" e pos
        a6 <- optional $ getAttribute "Release" e pos
        a7 <- optional $ getAttribute "Configuration" e pos
        a8 <- optional $ getAttribute "InternalReference" e pos
        a9 <- optional $ getAttribute "Tool" e pos
        a10 <- optional $ getAttribute "Feature" e pos
        a11 <- optional $ getAttribute "Qualification" e pos
        a12 <- optional $ getAttribute "Environment" e pos
        a13 <- optional $ getAttribute "Installation" e pos
        a14 <- optional $ getAttribute "Documentation" e pos
        a15 <- optional $ getAttribute "KnownBugManagement" e pos
        a16 <- optional $ getAttribute "Variant" e pos
        a17 <- optional $ getAttribute "KnownBugs" e pos
        a18 <- optional $ getAttribute "KnownBugList" e pos
        a19 <- optional $ getAttribute "TestRuns" e pos
        a20 <- optional $ getAttribute "CoveredByVV" e pos
        a21 <- optional $ getAttribute "NextIdentification" e pos
        commit $ interior e $ return (Identification a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21)
            `apply` many (parseSchemaType "Reviews")
    schemaTypeToXML s x@Identification{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ identification_iD x
                       , maybe [] (toXMLAttribute "Name") $ identification_name x
                       , maybe [] (toXMLAttribute "Description") $ identification_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ identification_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ identification_comment x
                       , maybe [] (toXMLAttribute "Version") $ identification_version x
                       , maybe [] (toXMLAttribute "Release") $ identification_release x
                       , maybe [] (toXMLAttribute "Configuration") $ identification_configuration x
                       , maybe [] (toXMLAttribute "InternalReference") $ identification_internalReference x
                       , maybe [] (toXMLAttribute "Tool") $ identification_tool x
                       , maybe [] (toXMLAttribute "Feature") $ identification_feature x
                       , maybe [] (toXMLAttribute "Qualification") $ identification_qualification x
                       , maybe [] (toXMLAttribute "Environment") $ identification_environment x
                       , maybe [] (toXMLAttribute "Installation") $ identification_installation x
                       , maybe [] (toXMLAttribute "Documentation") $ identification_documentation x
                       , maybe [] (toXMLAttribute "KnownBugManagement") $ identification_knownBugManagement x
                       , maybe [] (toXMLAttribute "Variant") $ identification_variant x
                       , maybe [] (toXMLAttribute "KnownBugs") $ identification_knownBugs x
                       , maybe [] (toXMLAttribute "KnownBugList") $ identification_knownBugList x
                       , maybe [] (toXMLAttribute "TestRuns") $ identification_testRuns x
                       , maybe [] (toXMLAttribute "CoveredByVV") $ identification_coveredByVV x
                       , maybe [] (toXMLAttribute "NextIdentification") $ identification_nextIdentification x
                       ]
            [ concatMap (schemaTypeToXML "Reviews") $ identification_reviews x
            ]
instance Extension Identification IIdentifiable where
    supertype v = IIdentifiable_Identification v
 
data KnownBug = KnownBug
        { knownBug_iD :: Maybe Ecore.EString
        , knownBug_name :: Maybe Ecore.EString
        , knownBug_description :: Maybe Ecore.EString
        , knownBug_longDescription :: Maybe Ecore.EString
        , knownBug_comment :: Maybe Ecore.EString
        , knownBug_fixed :: Maybe Ecore.EBoolean
        , knownBug_internalReference :: Maybe Ecore.EString
        , knownBug_tool :: Maybe Xsd.AnyURI
        , knownBug_tests :: Maybe Xsd.XsdString
        , knownBug_errors :: Maybe Xsd.XsdString
        , knownBug_avoidedBy :: Maybe Xsd.XsdString
        , knownBug_discoveredBy :: Maybe Xsd.XsdString
        , knownBug_variant :: Maybe Xsd.XsdString
        , knownBug_identifications :: Maybe Xsd.XsdString
        , knownBug_useCases :: Maybe Xsd.XsdString
        , knownBug_testResults :: Maybe Xsd.XsdString
        , knownBug_observableVerdict :: Maybe Verdict
        , knownBug_observableMessagePattern :: Maybe Ecore.EString
        , knownBug_observableTestNamePattern :: Maybe Ecore.EString
        , knownBug_reviews :: [Review]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType KnownBug where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "Fixed" e pos
        a6 <- optional $ getAttribute "InternalReference" e pos
        a7 <- optional $ getAttribute "Tool" e pos
        a8 <- optional $ getAttribute "Tests" e pos
        a9 <- optional $ getAttribute "Errors" e pos
        a10 <- optional $ getAttribute "AvoidedBy" e pos
        a11 <- optional $ getAttribute "DiscoveredBy" e pos
        a12 <- optional $ getAttribute "Variant" e pos
        a13 <- optional $ getAttribute "Identifications" e pos
        a14 <- optional $ getAttribute "UseCases" e pos
        a15 <- optional $ getAttribute "TestResults" e pos
        a16 <- optional $ getAttribute "ObservableVerdict" e pos
        a17 <- optional $ getAttribute "ObservableMessagePattern" e pos
        a18 <- optional $ getAttribute "ObservableTestNamePattern" e pos
        commit $ interior e $ return (KnownBug a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18)
            `apply` many (parseSchemaType "Reviews")
    schemaTypeToXML s x@KnownBug{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ knownBug_iD x
                       , maybe [] (toXMLAttribute "Name") $ knownBug_name x
                       , maybe [] (toXMLAttribute "Description") $ knownBug_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ knownBug_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ knownBug_comment x
                       , maybe [] (toXMLAttribute "Fixed") $ knownBug_fixed x
                       , maybe [] (toXMLAttribute "InternalReference") $ knownBug_internalReference x
                       , maybe [] (toXMLAttribute "Tool") $ knownBug_tool x
                       , maybe [] (toXMLAttribute "Tests") $ knownBug_tests x
                       , maybe [] (toXMLAttribute "Errors") $ knownBug_errors x
                       , maybe [] (toXMLAttribute "AvoidedBy") $ knownBug_avoidedBy x
                       , maybe [] (toXMLAttribute "DiscoveredBy") $ knownBug_discoveredBy x
                       , maybe [] (toXMLAttribute "Variant") $ knownBug_variant x
                       , maybe [] (toXMLAttribute "Identifications") $ knownBug_identifications x
                       , maybe [] (toXMLAttribute "UseCases") $ knownBug_useCases x
                       , maybe [] (toXMLAttribute "TestResults") $ knownBug_testResults x
                       , maybe [] (toXMLAttribute "ObservableVerdict") $ knownBug_observableVerdict x
                       , maybe [] (toXMLAttribute "ObservableMessagePattern") $ knownBug_observableMessagePattern x
                       , maybe [] (toXMLAttribute "ObservableTestNamePattern") $ knownBug_observableTestNamePattern x
                       ]
            [ concatMap (schemaTypeToXML "Reviews") $ knownBug_reviews x
            ]
instance Extension KnownBug IIdentifiable where
    supertype v = IIdentifiable_KnownBug v
 
data QualificationProject = QualificationProject
        { qualificationProject_iD :: Maybe Ecore.EString
        , qualificationProject_name :: Maybe Ecore.EString
        , qualificationProject_description :: Maybe Ecore.EString
        , qualificationProject_longDescription :: Maybe Ecore.EString
        , qualificationProject_comment :: Maybe Ecore.EString
        , qualificationProject_tool :: Maybe Xsd.AnyURI
        , qualificationProject_method :: Maybe QualificationMethod
        , qualificationProject_internalReference :: Maybe Ecore.EString
        , qualificationProject_variant :: Maybe Xsd.XsdString
        , qualificationProject_projectStartDate :: Maybe Ecore.EString
        , qualificationProject_projectEndDate :: Maybe Ecore.EString
        , qualificationProject_qualificationRoles :: [QualificationRole]
        , qualificationProject_qualificationSteps :: [QualificationStep]
        , qualificationProject_qualificationArtifacts :: [QualificationArtifact]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType QualificationProject where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "Tool" e pos
        a6 <- optional $ getAttribute "Method" e pos
        a7 <- optional $ getAttribute "InternalReference" e pos
        a8 <- optional $ getAttribute "Variant" e pos
        a9 <- optional $ getAttribute "ProjectStartDate" e pos
        a10 <- optional $ getAttribute "ProjectEndDate" e pos
        commit $ interior e $ return (QualificationProject a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
            `apply` many (parseSchemaType "QualificationRoles")
            `apply` many (parseSchemaType "QualificationSteps")
            `apply` many (parseSchemaType "QualificationArtifacts")
    schemaTypeToXML s x@QualificationProject{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ qualificationProject_iD x
                       , maybe [] (toXMLAttribute "Name") $ qualificationProject_name x
                       , maybe [] (toXMLAttribute "Description") $ qualificationProject_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ qualificationProject_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ qualificationProject_comment x
                       , maybe [] (toXMLAttribute "Tool") $ qualificationProject_tool x
                       , maybe [] (toXMLAttribute "Method") $ qualificationProject_method x
                       , maybe [] (toXMLAttribute "InternalReference") $ qualificationProject_internalReference x
                       , maybe [] (toXMLAttribute "Variant") $ qualificationProject_variant x
                       , maybe [] (toXMLAttribute "ProjectStartDate") $ qualificationProject_projectStartDate x
                       , maybe [] (toXMLAttribute "ProjectEndDate") $ qualificationProject_projectEndDate x
                       ]
            [ concatMap (schemaTypeToXML "QualificationRoles") $ qualificationProject_qualificationRoles x
            , concatMap (schemaTypeToXML "QualificationSteps") $ qualificationProject_qualificationSteps x
            , concatMap (schemaTypeToXML "QualificationArtifacts") $ qualificationProject_qualificationArtifacts x
            ]
instance Extension QualificationProject IIdentifiable where
    supertype v = IIdentifiable_QualificationProject v
 
data QualificationRole = QualificationRole
        { qualificationRole_iD :: Maybe Ecore.EString
        , qualificationRole_name :: Maybe Ecore.EString
        , qualificationRole_description :: Maybe Ecore.EString
        , qualificationRole_longDescription :: Maybe Ecore.EString
        , qualificationRole_comment :: Maybe Ecore.EString
        , qualificationRole_qualificationProject :: Maybe Xsd.AnyURI
        , qualificationRole_assignedPerson :: Maybe Ecore.EString
        , qualificationRole_internalReference :: Maybe Ecore.EString
        , qualificationRole_qualificationSteps :: Maybe Xsd.XsdString
        , qualificationRole_variant :: Maybe Xsd.XsdString
        , qualificationRole_qualificationArtifacts :: Maybe Xsd.XsdString
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType QualificationRole where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "QualificationProject" e pos
        a6 <- optional $ getAttribute "AssignedPerson" e pos
        a7 <- optional $ getAttribute "InternalReference" e pos
        a8 <- optional $ getAttribute "QualificationSteps" e pos
        a9 <- optional $ getAttribute "Variant" e pos
        a10 <- optional $ getAttribute "QualificationArtifacts" e pos
        commit $ interior e $ return (QualificationRole a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
    schemaTypeToXML s x@QualificationRole{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ qualificationRole_iD x
                       , maybe [] (toXMLAttribute "Name") $ qualificationRole_name x
                       , maybe [] (toXMLAttribute "Description") $ qualificationRole_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ qualificationRole_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ qualificationRole_comment x
                       , maybe [] (toXMLAttribute "QualificationProject") $ qualificationRole_qualificationProject x
                       , maybe [] (toXMLAttribute "AssignedPerson") $ qualificationRole_assignedPerson x
                       , maybe [] (toXMLAttribute "InternalReference") $ qualificationRole_internalReference x
                       , maybe [] (toXMLAttribute "QualificationSteps") $ qualificationRole_qualificationSteps x
                       , maybe [] (toXMLAttribute "Variant") $ qualificationRole_variant x
                       , maybe [] (toXMLAttribute "QualificationArtifacts") $ qualificationRole_qualificationArtifacts x
                       ]
            []
instance Extension QualificationRole IIdentifiable where
    supertype v = IIdentifiable_QualificationRole v
 
data QualificationStep = QualificationStep
        { qualificationStep_iD :: Maybe Ecore.EString
        , qualificationStep_name :: Maybe Ecore.EString
        , qualificationStep_description :: Maybe Ecore.EString
        , qualificationStep_longDescription :: Maybe Ecore.EString
        , qualificationStep_comment :: Maybe Ecore.EString
        , qualificationStep_plannedDate :: Maybe Ecore.EString
        , qualificationStep_internalReference :: Maybe Ecore.EString
        , qualificationStep_effortEstimation :: Maybe Ecore.EString
        , qualificationStep_qualificationProject :: Maybe Xsd.AnyURI
        , qualificationStep_qualificationRole :: Maybe Xsd.AnyURI
        , qualificationStep_superQualificationStep :: Maybe Xsd.AnyURI
        , qualificationStep_requiredQualificationSteps :: Maybe Xsd.XsdString
        , qualificationStep_followingQualificationSteps :: Maybe Xsd.XsdString
        , qualificationStep_variant :: Maybe Xsd.XsdString
        , qualificationStep_finishedDate :: Maybe Ecore.EString
        , qualificationStep_finishedComment :: Maybe Ecore.EString
        , qualificationStep_outputs :: Maybe Xsd.XsdString
        , qualificationStep_inputs :: Maybe Xsd.XsdString
        , qualificationStep_automated :: Maybe Ecore.EBoolean
        , qualificationStep_subQualificationSteps :: [QualificationStep]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType QualificationStep where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "PlannedDate" e pos
        a6 <- optional $ getAttribute "InternalReference" e pos
        a7 <- optional $ getAttribute "EffortEstimation" e pos
        a8 <- optional $ getAttribute "QualificationProject" e pos
        a9 <- optional $ getAttribute "QualificationRole" e pos
        a10 <- optional $ getAttribute "SuperQualificationStep" e pos
        a11 <- optional $ getAttribute "RequiredQualificationSteps" e pos
        a12 <- optional $ getAttribute "FollowingQualificationSteps" e pos
        a13 <- optional $ getAttribute "Variant" e pos
        a14 <- optional $ getAttribute "FinishedDate" e pos
        a15 <- optional $ getAttribute "FinishedComment" e pos
        a16 <- optional $ getAttribute "Outputs" e pos
        a17 <- optional $ getAttribute "Inputs" e pos
        a18 <- optional $ getAttribute "Automated" e pos
        commit $ interior e $ return (QualificationStep a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18)
            `apply` many (parseSchemaType "SubQualificationSteps")
    schemaTypeToXML s x@QualificationStep{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ qualificationStep_iD x
                       , maybe [] (toXMLAttribute "Name") $ qualificationStep_name x
                       , maybe [] (toXMLAttribute "Description") $ qualificationStep_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ qualificationStep_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ qualificationStep_comment x
                       , maybe [] (toXMLAttribute "PlannedDate") $ qualificationStep_plannedDate x
                       , maybe [] (toXMLAttribute "InternalReference") $ qualificationStep_internalReference x
                       , maybe [] (toXMLAttribute "EffortEstimation") $ qualificationStep_effortEstimation x
                       , maybe [] (toXMLAttribute "QualificationProject") $ qualificationStep_qualificationProject x
                       , maybe [] (toXMLAttribute "QualificationRole") $ qualificationStep_qualificationRole x
                       , maybe [] (toXMLAttribute "SuperQualificationStep") $ qualificationStep_superQualificationStep x
                       , maybe [] (toXMLAttribute "RequiredQualificationSteps") $ qualificationStep_requiredQualificationSteps x
                       , maybe [] (toXMLAttribute "FollowingQualificationSteps") $ qualificationStep_followingQualificationSteps x
                       , maybe [] (toXMLAttribute "Variant") $ qualificationStep_variant x
                       , maybe [] (toXMLAttribute "FinishedDate") $ qualificationStep_finishedDate x
                       , maybe [] (toXMLAttribute "FinishedComment") $ qualificationStep_finishedComment x
                       , maybe [] (toXMLAttribute "Outputs") $ qualificationStep_outputs x
                       , maybe [] (toXMLAttribute "Inputs") $ qualificationStep_inputs x
                       , maybe [] (toXMLAttribute "Automated") $ qualificationStep_automated x
                       ]
            [ concatMap (schemaTypeToXML "SubQualificationSteps") $ qualificationStep_subQualificationSteps x
            ]
instance Extension QualificationStep IIdentifiable where
    supertype v = IIdentifiable_QualificationStep v
 
data QualificationArtifact = QualificationArtifact
        { qualificationArtifact_iD :: Maybe Ecore.EString
        , qualificationArtifact_name :: Maybe Ecore.EString
        , qualificationArtifact_description :: Maybe Ecore.EString
        , qualificationArtifact_longDescription :: Maybe Ecore.EString
        , qualificationArtifact_comment :: Maybe Ecore.EString
        , qualificationArtifact_variant :: Maybe Xsd.XsdString
        , qualificationArtifact_qualificationProject :: Maybe Xsd.AnyURI
        , qualificationArtifact_qualificationRole :: Maybe Xsd.AnyURI
        , qualificationArtifact_createdBy :: Maybe Xsd.XsdString
        , qualificationArtifact_usedBy :: Maybe Xsd.XsdString
        , qualificationArtifact_path :: Maybe Ecore.EString
        , qualificationArtifact_internalReference :: Maybe Ecore.EString
        , qualificationArtifact_automated :: Maybe Ecore.EBoolean
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType QualificationArtifact where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "Variant" e pos
        a6 <- optional $ getAttribute "QualificationProject" e pos
        a7 <- optional $ getAttribute "QualificationRole" e pos
        a8 <- optional $ getAttribute "CreatedBy" e pos
        a9 <- optional $ getAttribute "UsedBy" e pos
        a10 <- optional $ getAttribute "Path" e pos
        a11 <- optional $ getAttribute "InternalReference" e pos
        a12 <- optional $ getAttribute "Automated" e pos
        commit $ interior e $ return (QualificationArtifact a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
    schemaTypeToXML s x@QualificationArtifact{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ qualificationArtifact_iD x
                       , maybe [] (toXMLAttribute "Name") $ qualificationArtifact_name x
                       , maybe [] (toXMLAttribute "Description") $ qualificationArtifact_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ qualificationArtifact_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ qualificationArtifact_comment x
                       , maybe [] (toXMLAttribute "Variant") $ qualificationArtifact_variant x
                       , maybe [] (toXMLAttribute "QualificationProject") $ qualificationArtifact_qualificationProject x
                       , maybe [] (toXMLAttribute "QualificationRole") $ qualificationArtifact_qualificationRole x
                       , maybe [] (toXMLAttribute "CreatedBy") $ qualificationArtifact_createdBy x
                       , maybe [] (toXMLAttribute "UsedBy") $ qualificationArtifact_usedBy x
                       , maybe [] (toXMLAttribute "Path") $ qualificationArtifact_path x
                       , maybe [] (toXMLAttribute "InternalReference") $ qualificationArtifact_internalReference x
                       , maybe [] (toXMLAttribute "Automated") $ qualificationArtifact_automated x
                       ]
            []
instance Extension QualificationArtifact IIdentifiable where
    supertype v = IIdentifiable_QualificationArtifact v
 
data SafetyStandard
    = SafetyStandard_ISO26262
    | SafetyStandard_IEC61508
    | SafetyStandard_EN50128
    | SafetyStandard_DO178C
    | SafetyStandard_DO278A
    | SafetyStandard_DO330
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType SafetyStandard where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType SafetyStandard where
    acceptingParser =  do literal "ISO26262"; return SafetyStandard_ISO26262
                      `onFail` do literal "IEC61508"; return SafetyStandard_IEC61508
                      `onFail` do literal "EN50128"; return SafetyStandard_EN50128
                      `onFail` do literal "DO178C"; return SafetyStandard_DO178C
                      `onFail` do literal "DO278A"; return SafetyStandard_DO278A
                      `onFail` do literal "DO330"; return SafetyStandard_DO330
                      
    simpleTypeText SafetyStandard_ISO26262 = "ISO26262"
    simpleTypeText SafetyStandard_IEC61508 = "IEC61508"
    simpleTypeText SafetyStandard_EN50128 = "EN50128"
    simpleTypeText SafetyStandard_DO178C = "DO178C"
    simpleTypeText SafetyStandard_DO278A = "DO278A"
    simpleTypeText SafetyStandard_DO330 = "DO330"
 
data Method = Method
        { method_iD :: Maybe Ecore.EString
        , method_name :: Maybe Ecore.EString
        , method_description :: Maybe Ecore.EString
        , method_longDescription :: Maybe Ecore.EString
        , method_comment :: Maybe Ecore.EString
        , method_toolSupported :: Maybe Ecore.EBoolean
        , method_mandatoryFromLevel :: Maybe RiskLevel
        , method_mandatoryToLevel :: Maybe RiskLevel
        , method_availableFeatures :: Maybe Xsd.XsdString
        , method_availableTools :: Maybe Xsd.XsdString
        , method_standardMethods :: Maybe Xsd.AnyURI
        , method_kind :: Maybe MethodKind
        , method_reference :: Maybe Ecore.EString
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType Method where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "ToolSupported" e pos
        a6 <- optional $ getAttribute "MandatoryFromLevel" e pos
        a7 <- optional $ getAttribute "MandatoryToLevel" e pos
        a8 <- optional $ getAttribute "AvailableFeatures" e pos
        a9 <- optional $ getAttribute "AvailableTools" e pos
        a10 <- optional $ getAttribute "StandardMethods" e pos
        a11 <- optional $ getAttribute "Kind" e pos
        a12 <- optional $ getAttribute "Reference" e pos
        commit $ interior e $ return (Method a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
    schemaTypeToXML s x@Method{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ method_iD x
                       , maybe [] (toXMLAttribute "Name") $ method_name x
                       , maybe [] (toXMLAttribute "Description") $ method_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ method_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ method_comment x
                       , maybe [] (toXMLAttribute "ToolSupported") $ method_toolSupported x
                       , maybe [] (toXMLAttribute "MandatoryFromLevel") $ method_mandatoryFromLevel x
                       , maybe [] (toXMLAttribute "MandatoryToLevel") $ method_mandatoryToLevel x
                       , maybe [] (toXMLAttribute "AvailableFeatures") $ method_availableFeatures x
                       , maybe [] (toXMLAttribute "AvailableTools") $ method_availableTools x
                       , maybe [] (toXMLAttribute "StandardMethods") $ method_standardMethods x
                       , maybe [] (toXMLAttribute "Kind") $ method_kind x
                       , maybe [] (toXMLAttribute "Reference") $ method_reference x
                       ]
            []
instance Extension Method IIdentifiable where
    supertype v = IIdentifiable_Method v
 
data RiskLevel
    = RiskLevel_ASILA
    | RiskLevel_ASILB
    | RiskLevel_ASILC
    | RiskLevel_ASILD
    | RiskLevel_SIL1
    | RiskLevel_SIL2
    | RiskLevel_SIL3
    | RiskLevel_SIL4
    | RiskLevel_TQL1
    | RiskLevel_TQL2
    | RiskLevel_TQL3
    | RiskLevel_TQL4
    | RiskLevel_TQL5
    | RiskLevel_AL1
    | RiskLevel_AL2
    | RiskLevel_AL3
    | RiskLevel_AL4
    | RiskLevel_AL5
    | RiskLevel_SLA
    | RiskLevel_SLB
    | RiskLevel_SLC
    | RiskLevel_SLD
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType RiskLevel where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType RiskLevel where
    acceptingParser =  do literal "ASILA"; return RiskLevel_ASILA
                      `onFail` do literal "ASILB"; return RiskLevel_ASILB
                      `onFail` do literal "ASILC"; return RiskLevel_ASILC
                      `onFail` do literal "ASILD"; return RiskLevel_ASILD
                      `onFail` do literal "SIL1"; return RiskLevel_SIL1
                      `onFail` do literal "SIL2"; return RiskLevel_SIL2
                      `onFail` do literal "SIL3"; return RiskLevel_SIL3
                      `onFail` do literal "SIL4"; return RiskLevel_SIL4
                      `onFail` do literal "TQL1"; return RiskLevel_TQL1
                      `onFail` do literal "TQL2"; return RiskLevel_TQL2
                      `onFail` do literal "TQL3"; return RiskLevel_TQL3
                      `onFail` do literal "TQL4"; return RiskLevel_TQL4
                      `onFail` do literal "TQL5"; return RiskLevel_TQL5
                      `onFail` do literal "AL1"; return RiskLevel_AL1
                      `onFail` do literal "AL2"; return RiskLevel_AL2
                      `onFail` do literal "AL3"; return RiskLevel_AL3
                      `onFail` do literal "AL4"; return RiskLevel_AL4
                      `onFail` do literal "AL5"; return RiskLevel_AL5
                      `onFail` do literal "SLA"; return RiskLevel_SLA
                      `onFail` do literal "SLB"; return RiskLevel_SLB
                      `onFail` do literal "SLC"; return RiskLevel_SLC
                      `onFail` do literal "SLD"; return RiskLevel_SLD
                      
    simpleTypeText RiskLevel_ASILA = "ASILA"
    simpleTypeText RiskLevel_ASILB = "ASILB"
    simpleTypeText RiskLevel_ASILC = "ASILC"
    simpleTypeText RiskLevel_ASILD = "ASILD"
    simpleTypeText RiskLevel_SIL1 = "SIL1"
    simpleTypeText RiskLevel_SIL2 = "SIL2"
    simpleTypeText RiskLevel_SIL3 = "SIL3"
    simpleTypeText RiskLevel_SIL4 = "SIL4"
    simpleTypeText RiskLevel_TQL1 = "TQL1"
    simpleTypeText RiskLevel_TQL2 = "TQL2"
    simpleTypeText RiskLevel_TQL3 = "TQL3"
    simpleTypeText RiskLevel_TQL4 = "TQL4"
    simpleTypeText RiskLevel_TQL5 = "TQL5"
    simpleTypeText RiskLevel_AL1 = "AL1"
    simpleTypeText RiskLevel_AL2 = "AL2"
    simpleTypeText RiskLevel_AL3 = "AL3"
    simpleTypeText RiskLevel_AL4 = "AL4"
    simpleTypeText RiskLevel_AL5 = "AL5"
    simpleTypeText RiskLevel_SLA = "SLA"
    simpleTypeText RiskLevel_SLB = "SLB"
    simpleTypeText RiskLevel_SLC = "SLC"
    simpleTypeText RiskLevel_SLD = "SLD"
 
data StandardMethods = StandardMethods
        { standardMethods_toolChain :: Maybe Xsd.AnyURI
        , standardMethods_safetyStandard :: Maybe SafetyStandard
        , standardMethods_methods :: [Method]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType StandardMethods where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ToolChain" e pos
        a1 <- optional $ getAttribute "SafetyStandard" e pos
        commit $ interior e $ return (StandardMethods a0 a1)
            `apply` many (parseSchemaType "Methods")
    schemaTypeToXML s x@StandardMethods{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ToolChain") $ standardMethods_toolChain x
                       , maybe [] (toXMLAttribute "SafetyStandard") $ standardMethods_safetyStandard x
                       ]
            [ concatMap (schemaTypeToXML "Methods") $ standardMethods_methods x
            ]
 
data ConfidenceLevel
    = ConfidenceLevel_L1
    | ConfidenceLevel_L2
    | ConfidenceLevel_L3
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType ConfidenceLevel where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ConfidenceLevel where
    acceptingParser =  do literal "L1"; return ConfidenceLevel_L1
                      `onFail` do literal "L2"; return ConfidenceLevel_L2
                      `onFail` do literal "L3"; return ConfidenceLevel_L3
                      
    simpleTypeText ConfidenceLevel_L1 = "L1"
    simpleTypeText ConfidenceLevel_L2 = "L2"
    simpleTypeText ConfidenceLevel_L3 = "L3"
 
data MethodKind
    = MethodKind_Construction
    | MethodKind_Analysis
    | MethodKind_Verification
    | MethodKind_Other
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType MethodKind where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType MethodKind where
    acceptingParser =  do literal "Construction"; return MethodKind_Construction
                      `onFail` do literal "Analysis"; return MethodKind_Analysis
                      `onFail` do literal "Verification"; return MethodKind_Verification
                      `onFail` do literal "Other"; return MethodKind_Other
                      
    simpleTypeText MethodKind_Construction = "Construction"
    simpleTypeText MethodKind_Analysis = "Analysis"
    simpleTypeText MethodKind_Verification = "Verification"
    simpleTypeText MethodKind_Other = "Other"
 
data Review = Review
        { review_iD :: Maybe Ecore.EString
        , review_name :: Maybe Ecore.EString
        , review_description :: Maybe Ecore.EString
        , review_longDescription :: Maybe Ecore.EString
        , review_comment :: Maybe Ecore.EString
        , review_date :: Maybe Ecore.EString
        , review_result :: Maybe ReviewResult
        , review_reviewer :: Maybe Ecore.EString
        , review_internalReference :: Maybe Ecore.EString
        , review_test :: Maybe Xsd.AnyURI
        , review_tool :: Maybe Xsd.AnyURI
        , review_useCase :: Maybe Xsd.AnyURI
        , review_error :: Maybe Xsd.AnyURI
        , review_knownBug :: Maybe Xsd.AnyURI
        , review_check :: Maybe Xsd.AnyURI
        , review_restriction :: Maybe Xsd.AnyURI
        , review_artifact :: Maybe Xsd.AnyURI
        , review_qualification :: Maybe Xsd.AnyURI
        , review_anomalousOpCond :: Maybe Xsd.AnyURI
        , review_attribute :: Maybe Xsd.AnyURI
        , review_identification :: Maybe Xsd.AnyURI
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType Review where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "Date" e pos
        a6 <- optional $ getAttribute "Result" e pos
        a7 <- optional $ getAttribute "Reviewer" e pos
        a8 <- optional $ getAttribute "InternalReference" e pos
        a9 <- optional $ getAttribute "Test" e pos
        a10 <- optional $ getAttribute "Tool" e pos
        a11 <- optional $ getAttribute "UseCase" e pos
        a12 <- optional $ getAttribute "Error" e pos
        a13 <- optional $ getAttribute "KnownBug" e pos
        a14 <- optional $ getAttribute "Check" e pos
        a15 <- optional $ getAttribute "Restriction" e pos
        a16 <- optional $ getAttribute "Artifact" e pos
        a17 <- optional $ getAttribute "Qualification" e pos
        a18 <- optional $ getAttribute "AnomalousOpCond" e pos
        a19 <- optional $ getAttribute "Attribute" e pos
        a20 <- optional $ getAttribute "Identification" e pos
        commit $ interior e $ return (Review a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20)
    schemaTypeToXML s x@Review{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ review_iD x
                       , maybe [] (toXMLAttribute "Name") $ review_name x
                       , maybe [] (toXMLAttribute "Description") $ review_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ review_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ review_comment x
                       , maybe [] (toXMLAttribute "Date") $ review_date x
                       , maybe [] (toXMLAttribute "Result") $ review_result x
                       , maybe [] (toXMLAttribute "Reviewer") $ review_reviewer x
                       , maybe [] (toXMLAttribute "InternalReference") $ review_internalReference x
                       , maybe [] (toXMLAttribute "Test") $ review_test x
                       , maybe [] (toXMLAttribute "Tool") $ review_tool x
                       , maybe [] (toXMLAttribute "UseCase") $ review_useCase x
                       , maybe [] (toXMLAttribute "Error") $ review_error x
                       , maybe [] (toXMLAttribute "KnownBug") $ review_knownBug x
                       , maybe [] (toXMLAttribute "Check") $ review_check x
                       , maybe [] (toXMLAttribute "Restriction") $ review_restriction x
                       , maybe [] (toXMLAttribute "Artifact") $ review_artifact x
                       , maybe [] (toXMLAttribute "Qualification") $ review_qualification x
                       , maybe [] (toXMLAttribute "AnomalousOpCond") $ review_anomalousOpCond x
                       , maybe [] (toXMLAttribute "Attribute") $ review_attribute x
                       , maybe [] (toXMLAttribute "Identification") $ review_identification x
                       ]
            []
instance Extension Review IIdentifiable where
    supertype v = IIdentifiable_Review v
 
data ReviewResult
    = ReviewResult_OPEN
    | ReviewResult_FAIL
    | ReviewResult_PASS
    | ReviewResult_PASS_BEFORE_MODIFICATION
    | ReviewResult_FAIL_BEFORE_MODIFICATION
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType ReviewResult where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ReviewResult where
    acceptingParser =  do literal "OPEN"; return ReviewResult_OPEN
                      `onFail` do literal "FAIL"; return ReviewResult_FAIL
                      `onFail` do literal "PASS"; return ReviewResult_PASS
                      `onFail` do literal "PASS_BEFORE_MODIFICATION"; return ReviewResult_PASS_BEFORE_MODIFICATION
                      `onFail` do literal "FAIL_BEFORE_MODIFICATION"; return ReviewResult_FAIL_BEFORE_MODIFICATION
                      
    simpleTypeText ReviewResult_OPEN = "OPEN"
    simpleTypeText ReviewResult_FAIL = "FAIL"
    simpleTypeText ReviewResult_PASS = "PASS"
    simpleTypeText ReviewResult_PASS_BEFORE_MODIFICATION = "PASS_BEFORE_MODIFICATION"
    simpleTypeText ReviewResult_FAIL_BEFORE_MODIFICATION = "FAIL_BEFORE_MODIFICATION"
 
--  (There are no subtypes defined for this abstract type.)
data IBug = IBug deriving (GHCG.Generic,Eq,Show)
instance SchemaType IBug where
    parseSchemaType s = fail "Parse failed when expecting an extension type of IBug:\n  No extension types are known."
    schemaTypeToXML s _ = toXMLElement s [] []
instance Extension IBug IToolChainElement where
    supertype v = IToolChainElement_IBug v
 
--  (There are no subtypes defined for this abstract type.)
data IDerivable = IDerivable deriving (GHCG.Generic,Eq,Show)
instance SchemaType IDerivable where
    parseSchemaType s = fail "Parse failed when expecting an extension type of IDerivable:\n  No extension types are known."
    schemaTypeToXML s _ = toXMLElement s [] []
 
data TestRun = TestRun
        { testRun_iD :: Maybe Ecore.EString
        , testRun_name :: Maybe Ecore.EString
        , testRun_description :: Maybe Ecore.EString
        , testRun_longDescription :: Maybe Ecore.EString
        , testRun_comment :: Maybe Ecore.EString
        , testRun_startDate :: Maybe Ecore.EString
        , testRun_endDate :: Maybe Ecore.EString
        , testRun_platform :: Maybe Ecore.EString
        , testRun_testRunDirectory :: Maybe Ecore.EString
        , testRun_executionResult :: Maybe Verdict
        , testRun_tests :: Maybe Xsd.XsdString
        , testRun_tool :: Maybe Xsd.AnyURI
        , testRun_verdict :: Maybe Verdict
        , testRun_identification :: Maybe Xsd.AnyURI
        , testRun_variants :: Maybe Xsd.XsdString
        , testRun_numberOfUnitResults :: Maybe Ecore.EInt
        , testRun_useCases :: Maybe Xsd.XsdString
        , testRun_testResults :: [TestResult]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType TestRun where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "StartDate" e pos
        a6 <- optional $ getAttribute "EndDate" e pos
        a7 <- optional $ getAttribute "Platform" e pos
        a8 <- optional $ getAttribute "TestRunDirectory" e pos
        a9 <- optional $ getAttribute "ExecutionResult" e pos
        a10 <- optional $ getAttribute "Tests" e pos
        a11 <- optional $ getAttribute "Tool" e pos
        a12 <- optional $ getAttribute "Verdict" e pos
        a13 <- optional $ getAttribute "Identification" e pos
        a14 <- optional $ getAttribute "Variants" e pos
        a15 <- optional $ getAttribute "NumberOfUnitResults" e pos
        a16 <- optional $ getAttribute "UseCases" e pos
        commit $ interior e $ return (TestRun a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16)
            `apply` many (parseSchemaType "TestResults")
    schemaTypeToXML s x@TestRun{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ testRun_iD x
                       , maybe [] (toXMLAttribute "Name") $ testRun_name x
                       , maybe [] (toXMLAttribute "Description") $ testRun_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ testRun_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ testRun_comment x
                       , maybe [] (toXMLAttribute "StartDate") $ testRun_startDate x
                       , maybe [] (toXMLAttribute "EndDate") $ testRun_endDate x
                       , maybe [] (toXMLAttribute "Platform") $ testRun_platform x
                       , maybe [] (toXMLAttribute "TestRunDirectory") $ testRun_testRunDirectory x
                       , maybe [] (toXMLAttribute "ExecutionResult") $ testRun_executionResult x
                       , maybe [] (toXMLAttribute "Tests") $ testRun_tests x
                       , maybe [] (toXMLAttribute "Tool") $ testRun_tool x
                       , maybe [] (toXMLAttribute "Verdict") $ testRun_verdict x
                       , maybe [] (toXMLAttribute "Identification") $ testRun_identification x
                       , maybe [] (toXMLAttribute "Variants") $ testRun_variants x
                       , maybe [] (toXMLAttribute "NumberOfUnitResults") $ testRun_numberOfUnitResults x
                       , maybe [] (toXMLAttribute "UseCases") $ testRun_useCases x
                       ]
            [ concatMap (schemaTypeToXML "TestResults") $ testRun_testResults x
            ]
instance Extension TestRun IIdentifiable where
    supertype v = IIdentifiable_TestRun v
 
data Verdict
    = Verdict_NONE
    | Verdict_PASS
    | Verdict_FAIL
    | Verdict_ERROR
    | Verdict_SKIPPED
    | Verdict_ANALYZED
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType Verdict where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Verdict where
    acceptingParser =  do literal "NONE"; return Verdict_NONE
                      `onFail` do literal "PASS"; return Verdict_PASS
                      `onFail` do literal "FAIL"; return Verdict_FAIL
                      `onFail` do literal "ERROR"; return Verdict_ERROR
                      `onFail` do literal "SKIPPED"; return Verdict_SKIPPED
                      `onFail` do literal "ANALYZED"; return Verdict_ANALYZED
                      
    simpleTypeText Verdict_NONE = "NONE"
    simpleTypeText Verdict_PASS = "PASS"
    simpleTypeText Verdict_FAIL = "FAIL"
    simpleTypeText Verdict_ERROR = "ERROR"
    simpleTypeText Verdict_SKIPPED = "SKIPPED"
    simpleTypeText Verdict_ANALYZED = "ANALYZED"
 
data TestResult = TestResult
        { testResult_iD :: Maybe Ecore.EString
        , testResult_name :: Maybe Ecore.EString
        , testResult_description :: Maybe Ecore.EString
        , testResult_longDescription :: Maybe Ecore.EString
        , testResult_comment :: Maybe Ecore.EString
        , testResult_message :: Maybe Ecore.EString
        , testResult_executionResult :: Maybe Verdict
        , testResult_verdict :: Maybe Verdict
        , testResult_testRun :: Maybe Xsd.AnyURI
        , testResult_test :: Maybe Xsd.AnyURI
        , testResult_superTestResult :: Maybe Xsd.AnyURI
        , testResult_path :: Maybe Ecore.EString
        , testResult_knownBugs :: Maybe Xsd.XsdString
        , testResult_anomalousOpConds :: Maybe Xsd.XsdString
        , testResult_numberOfUnitResults :: Maybe Ecore.EInt
        , testResult_useCases :: Maybe Xsd.XsdString
        , testResult_subTestResults :: [TestResult]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType TestResult where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "Message" e pos
        a6 <- optional $ getAttribute "ExecutionResult" e pos
        a7 <- optional $ getAttribute "Verdict" e pos
        a8 <- optional $ getAttribute "TestRun" e pos
        a9 <- optional $ getAttribute "Test" e pos
        a10 <- optional $ getAttribute "SuperTestResult" e pos
        a11 <- optional $ getAttribute "Path" e pos
        a12 <- optional $ getAttribute "KnownBugs" e pos
        a13 <- optional $ getAttribute "AnomalousOpConds" e pos
        a14 <- optional $ getAttribute "NumberOfUnitResults" e pos
        a15 <- optional $ getAttribute "useCases" e pos
        commit $ interior e $ return (TestResult a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
            `apply` many (parseSchemaType "SubTestResults")
    schemaTypeToXML s x@TestResult{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ testResult_iD x
                       , maybe [] (toXMLAttribute "Name") $ testResult_name x
                       , maybe [] (toXMLAttribute "Description") $ testResult_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ testResult_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ testResult_comment x
                       , maybe [] (toXMLAttribute "Message") $ testResult_message x
                       , maybe [] (toXMLAttribute "ExecutionResult") $ testResult_executionResult x
                       , maybe [] (toXMLAttribute "Verdict") $ testResult_verdict x
                       , maybe [] (toXMLAttribute "TestRun") $ testResult_testRun x
                       , maybe [] (toXMLAttribute "Test") $ testResult_test x
                       , maybe [] (toXMLAttribute "SuperTestResult") $ testResult_superTestResult x
                       , maybe [] (toXMLAttribute "Path") $ testResult_path x
                       , maybe [] (toXMLAttribute "KnownBugs") $ testResult_knownBugs x
                       , maybe [] (toXMLAttribute "AnomalousOpConds") $ testResult_anomalousOpConds x
                       , maybe [] (toXMLAttribute "NumberOfUnitResults") $ testResult_numberOfUnitResults x
                       , maybe [] (toXMLAttribute "useCases") $ testResult_useCases x
                       ]
            [ concatMap (schemaTypeToXML "SubTestResults") $ testResult_subTestResults x
            ]
instance Extension TestResult IIdentifiable where
    supertype v = IIdentifiable_TestResult v
 
data Library = Library
        { library_iD :: Maybe Ecore.EString
        , library_name :: Maybe Ecore.EString
        , library_description :: Maybe Ecore.EString
        , library_longDescription :: Maybe Ecore.EString
        , library_comment :: Maybe Ecore.EString
        , library_partOf :: Maybe Xsd.AnyURI
        , library_toolAttributes :: Maybe Xsd.XsdString
        , library_inputs :: Maybe Xsd.XsdString
        , library_outputs :: Maybe Xsd.XsdString
        , library_inputsOutputs :: Maybe Xsd.XsdString
        , library_calledTools :: Maybe Xsd.XsdString
        , library_callingTools :: Maybe Xsd.XsdString
        , library_variant :: Maybe Xsd.XsdString
        , library_knownBugManagement :: Maybe Ecore.EString
        , library_knownBugDate :: Maybe Ecore.EString
        , library_numberOfExecutions :: Maybe Ecore.EInt
        , library_internalReference :: Maybe Ecore.EString
        , library_numberOfSetups :: Maybe Ecore.EInt
        , library_supportedMethods :: Maybe Xsd.XsdString
        , library_superTool :: Maybe Xsd.AnyURI
        , library_anomalousOpCondHandling :: Maybe Ecore.EString
        , library_toolOwner :: Maybe Ecore.EString
        , library_toolProvider :: Maybe Ecore.EString
        , library_path :: Maybe Ecore.EString
        , library_classification :: Maybe LibraryClassification
        , library_useCases :: [UseCase]
        , library_costs :: [ToolCosts]
        , library_qualifications :: [Qualification]
        , library_tests :: [Test]
        , library_identifications :: [Identification]
        , library_knownBugs :: [KnownBug]
        , library_qualificationProjects :: [QualificationProject]
        , library_reviews :: [Review]
        , library_testRuns :: [TestRun]
        , library_artifacts :: [Artifact]
        , library_anomalousOpConds :: [AnomalousOpCond]
        , library_parameters :: [Parameter]
        , library_libraryQualifications :: [LibraryQualification]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType Library where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "PartOf" e pos
        a6 <- optional $ getAttribute "ToolAttributes" e pos
        a7 <- optional $ getAttribute "Inputs" e pos
        a8 <- optional $ getAttribute "Outputs" e pos
        a9 <- optional $ getAttribute "InputsOutputs" e pos
        a10 <- optional $ getAttribute "CalledTools" e pos
        a11 <- optional $ getAttribute "CallingTools" e pos
        a12 <- optional $ getAttribute "Variant" e pos
        a13 <- optional $ getAttribute "KnownBugManagement" e pos
        a14 <- optional $ getAttribute "KnownBugDate" e pos
        a15 <- optional $ getAttribute "NumberOfExecutions" e pos
        a16 <- optional $ getAttribute "InternalReference" e pos
        a17 <- optional $ getAttribute "NumberOfSetups" e pos
        a18 <- optional $ getAttribute "SupportedMethods" e pos
        a19 <- optional $ getAttribute "SuperTool" e pos
        a20 <- optional $ getAttribute "AnomalousOpCondHandling" e pos
        a21 <- optional $ getAttribute "ToolOwner" e pos
        a22 <- optional $ getAttribute "ToolProvider" e pos
        a23 <- optional $ getAttribute "Path" e pos
        a24 <- optional $ getAttribute "Classification" e pos
        commit $ interior e $ return (Library a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24)
            `apply` many (parseSchemaType "UseCases")
            `apply` many (parseSchemaType "Costs")
            `apply` many (parseSchemaType "Qualifications")
            `apply` many (parseSchemaType "Tests")
            `apply` many (parseSchemaType "Identifications")
            `apply` many (parseSchemaType "KnownBugs")
            `apply` many (parseSchemaType "QualificationProjects")
            `apply` many (parseSchemaType "Reviews")
            `apply` many (parseSchemaType "TestRuns")
            `apply` many (parseSchemaType "Artifacts")
            `apply` many (parseSchemaType "AnomalousOpConds")
            `apply` many (parseSchemaType "Parameters")
            `apply` many (parseSchemaType "LibraryQualifications")
    schemaTypeToXML s x@Library{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ library_iD x
                       , maybe [] (toXMLAttribute "Name") $ library_name x
                       , maybe [] (toXMLAttribute "Description") $ library_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ library_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ library_comment x
                       , maybe [] (toXMLAttribute "PartOf") $ library_partOf x
                       , maybe [] (toXMLAttribute "ToolAttributes") $ library_toolAttributes x
                       , maybe [] (toXMLAttribute "Inputs") $ library_inputs x
                       , maybe [] (toXMLAttribute "Outputs") $ library_outputs x
                       , maybe [] (toXMLAttribute "InputsOutputs") $ library_inputsOutputs x
                       , maybe [] (toXMLAttribute "CalledTools") $ library_calledTools x
                       , maybe [] (toXMLAttribute "CallingTools") $ library_callingTools x
                       , maybe [] (toXMLAttribute "Variant") $ library_variant x
                       , maybe [] (toXMLAttribute "KnownBugManagement") $ library_knownBugManagement x
                       , maybe [] (toXMLAttribute "KnownBugDate") $ library_knownBugDate x
                       , maybe [] (toXMLAttribute "NumberOfExecutions") $ library_numberOfExecutions x
                       , maybe [] (toXMLAttribute "InternalReference") $ library_internalReference x
                       , maybe [] (toXMLAttribute "NumberOfSetups") $ library_numberOfSetups x
                       , maybe [] (toXMLAttribute "SupportedMethods") $ library_supportedMethods x
                       , maybe [] (toXMLAttribute "SuperTool") $ library_superTool x
                       , maybe [] (toXMLAttribute "AnomalousOpCondHandling") $ library_anomalousOpCondHandling x
                       , maybe [] (toXMLAttribute "ToolOwner") $ library_toolOwner x
                       , maybe [] (toXMLAttribute "ToolProvider") $ library_toolProvider x
                       , maybe [] (toXMLAttribute "Path") $ library_path x
                       , maybe [] (toXMLAttribute "Classification") $ library_classification x
                       ]
            [ concatMap (schemaTypeToXML "UseCases") $ library_useCases x
            , concatMap (schemaTypeToXML "Costs") $ library_costs x
            , concatMap (schemaTypeToXML "Qualifications") $ library_qualifications x
            , concatMap (schemaTypeToXML "Tests") $ library_tests x
            , concatMap (schemaTypeToXML "Identifications") $ library_identifications x
            , concatMap (schemaTypeToXML "KnownBugs") $ library_knownBugs x
            , concatMap (schemaTypeToXML "QualificationProjects") $ library_qualificationProjects x
            , concatMap (schemaTypeToXML "Reviews") $ library_reviews x
            , concatMap (schemaTypeToXML "TestRuns") $ library_testRuns x
            , concatMap (schemaTypeToXML "Artifacts") $ library_artifacts x
            , concatMap (schemaTypeToXML "AnomalousOpConds") $ library_anomalousOpConds x
            , concatMap (schemaTypeToXML "Parameters") $ library_parameters x
            , concatMap (schemaTypeToXML "LibraryQualifications") $ library_libraryQualifications x
            ]
instance Extension Library Tool where
    supertype (Library a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 e0 e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12) =
               Tool a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 e0 e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11
instance Extension Library IToolChainElement where
    supertype = (supertype :: Tool -> IToolChainElement)
              . (supertype :: Library -> Tool)
              
instance Extension Library IIdentifiable where
    supertype = (supertype :: IToolChainElement -> IIdentifiable)
              . (supertype :: Tool -> IToolChainElement)
              . (supertype :: Library -> Tool)
              
 
data Category
    = Category_NONE
    | Category_MANUAL
    | Category_TOOL_BASED
    | Category_IMPLICIT
    | Category_REDUNDANT
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType Category where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Category where
    acceptingParser =  do literal "NONE"; return Category_NONE
                      `onFail` do literal "MANUAL"; return Category_MANUAL
                      `onFail` do literal "TOOL_BASED"; return Category_TOOL_BASED
                      `onFail` do literal "IMPLICIT"; return Category_IMPLICIT
                      `onFail` do literal "REDUNDANT"; return Category_REDUNDANT
                      
    simpleTypeText Category_NONE = "NONE"
    simpleTypeText Category_MANUAL = "MANUAL"
    simpleTypeText Category_TOOL_BASED = "TOOL_BASED"
    simpleTypeText Category_IMPLICIT = "IMPLICIT"
    simpleTypeText Category_REDUNDANT = "REDUNDANT"
 
data Effort
    = Effort_UNCLEAR
    | Effort_NONE
    | Effort_LOW
    | Effort_MEDIUM
    | Effort_HIGH
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType Effort where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Effort where
    acceptingParser =  do literal "UNCLEAR"; return Effort_UNCLEAR
                      `onFail` do literal "NONE"; return Effort_NONE
                      `onFail` do literal "LOW"; return Effort_LOW
                      `onFail` do literal "MEDIUM"; return Effort_MEDIUM
                      `onFail` do literal "HIGH"; return Effort_HIGH
                      
    simpleTypeText Effort_UNCLEAR = "UNCLEAR"
    simpleTypeText Effort_NONE = "NONE"
    simpleTypeText Effort_LOW = "LOW"
    simpleTypeText Effort_MEDIUM = "MEDIUM"
    simpleTypeText Effort_HIGH = "HIGH"
 
data AnomalousOpCond = AnomalousOpCond
        { anomalousOpCond_iD :: Maybe Ecore.EString
        , anomalousOpCond_name :: Maybe Ecore.EString
        , anomalousOpCond_description :: Maybe Ecore.EString
        , anomalousOpCond_longDescription :: Maybe Ecore.EString
        , anomalousOpCond_comment :: Maybe Ecore.EString
        , anomalousOpCond_internalReference :: Maybe Ecore.EString
        , anomalousOpCond_tool :: Maybe Xsd.AnyURI
        , anomalousOpCond_tests :: Maybe Xsd.XsdString
        , anomalousOpCond_errors :: Maybe Xsd.XsdString
        , anomalousOpCond_avoidedBy :: Maybe Xsd.XsdString
        , anomalousOpCond_discoveredBy :: Maybe Xsd.XsdString
        , anomalousOpCond_variant :: Maybe Xsd.XsdString
        , anomalousOpCond_testResults :: Maybe Xsd.XsdString
        , anomalousOpCond_expectedBehaviour :: Maybe Ecore.EString
        , anomalousOpCond_reviews :: [Review]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType AnomalousOpCond where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "InternalReference" e pos
        a6 <- optional $ getAttribute "Tool" e pos
        a7 <- optional $ getAttribute "Tests" e pos
        a8 <- optional $ getAttribute "Errors" e pos
        a9 <- optional $ getAttribute "AvoidedBy" e pos
        a10 <- optional $ getAttribute "DiscoveredBy" e pos
        a11 <- optional $ getAttribute "Variant" e pos
        a12 <- optional $ getAttribute "TestResults" e pos
        a13 <- optional $ getAttribute "ExpectedBehaviour" e pos
        commit $ interior e $ return (AnomalousOpCond a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13)
            `apply` many (parseSchemaType "Reviews")
    schemaTypeToXML s x@AnomalousOpCond{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ anomalousOpCond_iD x
                       , maybe [] (toXMLAttribute "Name") $ anomalousOpCond_name x
                       , maybe [] (toXMLAttribute "Description") $ anomalousOpCond_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ anomalousOpCond_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ anomalousOpCond_comment x
                       , maybe [] (toXMLAttribute "InternalReference") $ anomalousOpCond_internalReference x
                       , maybe [] (toXMLAttribute "Tool") $ anomalousOpCond_tool x
                       , maybe [] (toXMLAttribute "Tests") $ anomalousOpCond_tests x
                       , maybe [] (toXMLAttribute "Errors") $ anomalousOpCond_errors x
                       , maybe [] (toXMLAttribute "AvoidedBy") $ anomalousOpCond_avoidedBy x
                       , maybe [] (toXMLAttribute "DiscoveredBy") $ anomalousOpCond_discoveredBy x
                       , maybe [] (toXMLAttribute "Variant") $ anomalousOpCond_variant x
                       , maybe [] (toXMLAttribute "TestResults") $ anomalousOpCond_testResults x
                       , maybe [] (toXMLAttribute "ExpectedBehaviour") $ anomalousOpCond_expectedBehaviour x
                       ]
            [ concatMap (schemaTypeToXML "Reviews") $ anomalousOpCond_reviews x
            ]
instance Extension AnomalousOpCond IIdentifiable where
    supertype v = IIdentifiable_AnomalousOpCond v
 
data QualificationResult
    = QualificationResult_UNQUALIFIED
    | QualificationResult_DISQUALIFIED
    | QualificationResult_SUCCESSFULLY_QUALIFIED
    | QualificationResult_PREQUALIFIED
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType QualificationResult where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType QualificationResult where
    acceptingParser =  do literal "UNQUALIFIED"; return QualificationResult_UNQUALIFIED
                      `onFail` do literal "DISQUALIFIED"; return QualificationResult_DISQUALIFIED
                      `onFail` do literal "SUCCESSFULLY_QUALIFIED"; return QualificationResult_SUCCESSFULLY_QUALIFIED
                      `onFail` do literal "PREQUALIFIED"; return QualificationResult_PREQUALIFIED
                      
    simpleTypeText QualificationResult_UNQUALIFIED = "UNQUALIFIED"
    simpleTypeText QualificationResult_DISQUALIFIED = "DISQUALIFIED"
    simpleTypeText QualificationResult_SUCCESSFULLY_QUALIFIED = "SUCCESSFULLY_QUALIFIED"
    simpleTypeText QualificationResult_PREQUALIFIED = "PREQUALIFIED"
 
data NOTVariant = NOTVariant
        { nOTVariant_iD :: Maybe Ecore.EString
        , nOTVariant_name :: Maybe Ecore.EString
        , nOTVariant_description :: Maybe Ecore.EString
        , nOTVariant_longDescription :: Maybe Ecore.EString
        , nOTVariant_comment :: Maybe Ecore.EString
        , nOTVariant_toolChain :: Maybe Xsd.AnyURI
        , nOTVariant_tools :: Maybe Xsd.XsdString
        , nOTVariant_artifacts :: Maybe Xsd.XsdString
        , nOTVariant_useCases :: Maybe Xsd.XsdString
        , nOTVariant_qualifications :: Maybe Xsd.XsdString
        , nOTVariant_internalReference :: Maybe Ecore.EString
        , nOTVariant_checks :: Maybe Xsd.XsdString
        , nOTVariant_errors :: Maybe Xsd.XsdString
        , nOTVariant_restrictions :: Maybe Xsd.XsdString
        , nOTVariant_tests :: Maybe Xsd.XsdString
        , nOTVariant_knownBugs :: Maybe Xsd.XsdString
        , nOTVariant_qualificationProjects :: Maybe Xsd.XsdString
        , nOTVariant_qualificationRoles :: Maybe Xsd.XsdString
        , nOTVariant_qualificationSteps :: Maybe Xsd.XsdString
        , nOTVariant_qualificationArtifacts :: Maybe Xsd.XsdString
        , nOTVariant_identifications :: Maybe Xsd.XsdString
        , nOTVariant_testRuns :: Maybe Xsd.XsdString
        , nOTVariant_anomalousOpConds :: Maybe Xsd.XsdString
        , nOTVariant_parameters :: Maybe Xsd.XsdString
        , nOTVariant_enumValues :: Maybe Xsd.XsdString
        , nOTVariant_variant :: Xsd.AnyURI
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType NOTVariant where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "ToolChain" e pos
        a6 <- optional $ getAttribute "Tools" e pos
        a7 <- optional $ getAttribute "Artifacts" e pos
        a8 <- optional $ getAttribute "UseCases" e pos
        a9 <- optional $ getAttribute "Qualifications" e pos
        a10 <- optional $ getAttribute "InternalReference" e pos
        a11 <- optional $ getAttribute "Checks" e pos
        a12 <- optional $ getAttribute "Errors" e pos
        a13 <- optional $ getAttribute "Restrictions" e pos
        a14 <- optional $ getAttribute "Tests" e pos
        a15 <- optional $ getAttribute "KnownBugs" e pos
        a16 <- optional $ getAttribute "QualificationProjects" e pos
        a17 <- optional $ getAttribute "QualificationRoles" e pos
        a18 <- optional $ getAttribute "QualificationSteps" e pos
        a19 <- optional $ getAttribute "QualificationArtifacts" e pos
        a20 <- optional $ getAttribute "Identifications" e pos
        a21 <- optional $ getAttribute "TestRuns" e pos
        a22 <- optional $ getAttribute "AnomalousOpConds" e pos
        a23 <- optional $ getAttribute "Parameters" e pos
        a24 <- optional $ getAttribute "EnumValues" e pos
        a25 <- getAttribute "Variant" e pos
        commit $ interior e $ return (NOTVariant a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25)
    schemaTypeToXML s x@NOTVariant{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ nOTVariant_iD x
                       , maybe [] (toXMLAttribute "Name") $ nOTVariant_name x
                       , maybe [] (toXMLAttribute "Description") $ nOTVariant_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ nOTVariant_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ nOTVariant_comment x
                       , maybe [] (toXMLAttribute "ToolChain") $ nOTVariant_toolChain x
                       , maybe [] (toXMLAttribute "Tools") $ nOTVariant_tools x
                       , maybe [] (toXMLAttribute "Artifacts") $ nOTVariant_artifacts x
                       , maybe [] (toXMLAttribute "UseCases") $ nOTVariant_useCases x
                       , maybe [] (toXMLAttribute "Qualifications") $ nOTVariant_qualifications x
                       , maybe [] (toXMLAttribute "InternalReference") $ nOTVariant_internalReference x
                       , maybe [] (toXMLAttribute "Checks") $ nOTVariant_checks x
                       , maybe [] (toXMLAttribute "Errors") $ nOTVariant_errors x
                       , maybe [] (toXMLAttribute "Restrictions") $ nOTVariant_restrictions x
                       , maybe [] (toXMLAttribute "Tests") $ nOTVariant_tests x
                       , maybe [] (toXMLAttribute "KnownBugs") $ nOTVariant_knownBugs x
                       , maybe [] (toXMLAttribute "QualificationProjects") $ nOTVariant_qualificationProjects x
                       , maybe [] (toXMLAttribute "QualificationRoles") $ nOTVariant_qualificationRoles x
                       , maybe [] (toXMLAttribute "QualificationSteps") $ nOTVariant_qualificationSteps x
                       , maybe [] (toXMLAttribute "QualificationArtifacts") $ nOTVariant_qualificationArtifacts x
                       , maybe [] (toXMLAttribute "Identifications") $ nOTVariant_identifications x
                       , maybe [] (toXMLAttribute "TestRuns") $ nOTVariant_testRuns x
                       , maybe [] (toXMLAttribute "AnomalousOpConds") $ nOTVariant_anomalousOpConds x
                       , maybe [] (toXMLAttribute "Parameters") $ nOTVariant_parameters x
                       , maybe [] (toXMLAttribute "EnumValues") $ nOTVariant_enumValues x
                       , toXMLAttribute "Variant" $ nOTVariant_variant x
                       ]
            []
instance Extension NOTVariant Variant where
    supertype (NOTVariant a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25) =
               Variant a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24
instance Extension NOTVariant IIdentifiable where
    supertype = (supertype :: Variant -> IIdentifiable)
              . (supertype :: NOTVariant -> Variant)
              
 
data ParameterType
    = ParameterType_TEXT_VARIABLE
    | ParameterType_DIR_PATH
    | ParameterType_FILE_PATH
    | ParameterType_EXECUTABLE_PATH
    | ParameterType_BOOLEAN
    | ParameterType_PROCESS_TO_KILL
    | ParameterType_ENUMERATION
    | ParameterType_BEFORE_TEST_HOOK_EXE
    | ParameterType_AFTER_TEST_HOOK_EXE
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType ParameterType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ParameterType where
    acceptingParser =  do literal "TEXT_VARIABLE"; return ParameterType_TEXT_VARIABLE
                      `onFail` do literal "DIR_PATH"; return ParameterType_DIR_PATH
                      `onFail` do literal "FILE_PATH"; return ParameterType_FILE_PATH
                      `onFail` do literal "EXECUTABLE_PATH"; return ParameterType_EXECUTABLE_PATH
                      `onFail` do literal "BOOLEAN"; return ParameterType_BOOLEAN
                      `onFail` do literal "PROCESS_TO_KILL"; return ParameterType_PROCESS_TO_KILL
                      `onFail` do literal "ENUMERATION"; return ParameterType_ENUMERATION
                      `onFail` do literal "BEFORE_TEST_HOOK_EXE"; return ParameterType_BEFORE_TEST_HOOK_EXE
                      `onFail` do literal "AFTER_TEST_HOOK_EXE"; return ParameterType_AFTER_TEST_HOOK_EXE
                      
    simpleTypeText ParameterType_TEXT_VARIABLE = "TEXT_VARIABLE"
    simpleTypeText ParameterType_DIR_PATH = "DIR_PATH"
    simpleTypeText ParameterType_FILE_PATH = "FILE_PATH"
    simpleTypeText ParameterType_EXECUTABLE_PATH = "EXECUTABLE_PATH"
    simpleTypeText ParameterType_BOOLEAN = "BOOLEAN"
    simpleTypeText ParameterType_PROCESS_TO_KILL = "PROCESS_TO_KILL"
    simpleTypeText ParameterType_ENUMERATION = "ENUMERATION"
    simpleTypeText ParameterType_BEFORE_TEST_HOOK_EXE = "BEFORE_TEST_HOOK_EXE"
    simpleTypeText ParameterType_AFTER_TEST_HOOK_EXE = "AFTER_TEST_HOOK_EXE"
 
data Parameter = Parameter
        { parameter_iD :: Maybe Ecore.EString
        , parameter_name :: Maybe Ecore.EString
        , parameter_description :: Maybe Ecore.EString
        , parameter_longDescription :: Maybe Ecore.EString
        , parameter_comment :: Maybe Ecore.EString
        , parameter_value :: Maybe Ecore.EString
        , parameter_defaultValue :: Maybe Ecore.EString
        , parameter_orderNumber :: Maybe Ecore.EInt
        , parameter_type :: Maybe ParameterType
        , parameter_variant :: Maybe Xsd.XsdString
        , parameter_mandatory :: Maybe Ecore.EBoolean
        , parameter_multiLines :: Maybe Ecore.EInt
        , parameter_validationOnly :: Maybe Ecore.EBoolean
        , parameter_readOnly :: Maybe Ecore.EBoolean
        , parameter_inVisible :: Maybe Ecore.EBoolean
        , parameter_warnMessage :: Maybe Ecore.EString
        , parameter_confidential :: Maybe Ecore.EBoolean
        , parameter_enumValues :: [EnumValue]
        , parameter_reviews :: [Review]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType Parameter where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "Value" e pos
        a6 <- optional $ getAttribute "DefaultValue" e pos
        a7 <- optional $ getAttribute "OrderNumber" e pos
        a8 <- optional $ getAttribute "Type" e pos
        a9 <- optional $ getAttribute "Variant" e pos
        a10 <- optional $ getAttribute "Mandatory" e pos
        a11 <- optional $ getAttribute "MultiLines" e pos
        a12 <- optional $ getAttribute "ValidationOnly" e pos
        a13 <- optional $ getAttribute "ReadOnly" e pos
        a14 <- optional $ getAttribute "InVisible" e pos
        a15 <- optional $ getAttribute "WarnMessage" e pos
        a16 <- optional $ getAttribute "Confidential" e pos
        commit $ interior e $ return (Parameter a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16)
            `apply` many (parseSchemaType "EnumValues")
            `apply` many (parseSchemaType "Reviews")
    schemaTypeToXML s x@Parameter{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ parameter_iD x
                       , maybe [] (toXMLAttribute "Name") $ parameter_name x
                       , maybe [] (toXMLAttribute "Description") $ parameter_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ parameter_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ parameter_comment x
                       , maybe [] (toXMLAttribute "Value") $ parameter_value x
                       , maybe [] (toXMLAttribute "DefaultValue") $ parameter_defaultValue x
                       , maybe [] (toXMLAttribute "OrderNumber") $ parameter_orderNumber x
                       , maybe [] (toXMLAttribute "Type") $ parameter_type x
                       , maybe [] (toXMLAttribute "Variant") $ parameter_variant x
                       , maybe [] (toXMLAttribute "Mandatory") $ parameter_mandatory x
                       , maybe [] (toXMLAttribute "MultiLines") $ parameter_multiLines x
                       , maybe [] (toXMLAttribute "ValidationOnly") $ parameter_validationOnly x
                       , maybe [] (toXMLAttribute "ReadOnly") $ parameter_readOnly x
                       , maybe [] (toXMLAttribute "InVisible") $ parameter_inVisible x
                       , maybe [] (toXMLAttribute "WarnMessage") $ parameter_warnMessage x
                       , maybe [] (toXMLAttribute "Confidential") $ parameter_confidential x
                       ]
            [ concatMap (schemaTypeToXML "EnumValues") $ parameter_enumValues x
            , concatMap (schemaTypeToXML "Reviews") $ parameter_reviews x
            ]
instance Extension Parameter IIdentifiable where
    supertype v = IIdentifiable_Parameter v
 
data EnumValue = EnumValue
        { enumValue_iD :: Maybe Ecore.EString
        , enumValue_name :: Maybe Ecore.EString
        , enumValue_description :: Maybe Ecore.EString
        , enumValue_longDescription :: Maybe Ecore.EString
        , enumValue_comment :: Maybe Ecore.EString
        , enumValue_orderNumber :: Maybe Ecore.EInt
        , enumValue_variant :: Maybe Xsd.XsdString
        , enumValue_reviews :: [Review]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EnumValue where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "OrderNumber" e pos
        a6 <- optional $ getAttribute "Variant" e pos
        commit $ interior e $ return (EnumValue a0 a1 a2 a3 a4 a5 a6)
            `apply` many (parseSchemaType "Reviews")
    schemaTypeToXML s x@EnumValue{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ enumValue_iD x
                       , maybe [] (toXMLAttribute "Name") $ enumValue_name x
                       , maybe [] (toXMLAttribute "Description") $ enumValue_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ enumValue_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ enumValue_comment x
                       , maybe [] (toXMLAttribute "OrderNumber") $ enumValue_orderNumber x
                       , maybe [] (toXMLAttribute "Variant") $ enumValue_variant x
                       ]
            [ concatMap (schemaTypeToXML "Reviews") $ enumValue_reviews x
            ]
instance Extension EnumValue IIdentifiable where
    supertype v = IIdentifiable_EnumValue v
 
data ToolQualification = ToolQualification
        { toolQualification_iD :: Maybe Ecore.EString
        , toolQualification_name :: Maybe Ecore.EString
        , toolQualification_description :: Maybe Ecore.EString
        , toolQualification_longDescription :: Maybe Ecore.EString
        , toolQualification_comment :: Maybe Ecore.EString
        , toolQualification_date :: Maybe Ecore.EString
        , toolQualification_tool :: Maybe Xsd.AnyURI
        , toolQualification_useCases :: Maybe Xsd.XsdString
        , toolQualification_variant :: Maybe Xsd.XsdString
        , toolQualification_tests :: Maybe Xsd.XsdString
        , toolQualification_internalReference :: Maybe Ecore.EString
        , toolQualification_identifications :: Maybe Xsd.XsdString
        , toolQualification_errors :: Maybe Xsd.XsdString
        , toolQualification_responsible :: Maybe Ecore.EString
        , toolQualification_qualificationResult :: Maybe QualificationResult
        , toolQualification_method :: Maybe QualificationMethod
        , toolQualification_toolQualificationMethod :: Maybe QualificationMethod
        , toolQualification_reviews :: [Review]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType ToolQualification where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "Date" e pos
        a6 <- optional $ getAttribute "Tool" e pos
        a7 <- optional $ getAttribute "UseCases" e pos
        a8 <- optional $ getAttribute "Variant" e pos
        a9 <- optional $ getAttribute "Tests" e pos
        a10 <- optional $ getAttribute "InternalReference" e pos
        a11 <- optional $ getAttribute "Identifications" e pos
        a12 <- optional $ getAttribute "Errors" e pos
        a13 <- optional $ getAttribute "Responsible" e pos
        a14 <- optional $ getAttribute "QualificationResult" e pos
        a15 <- optional $ getAttribute "Method" e pos
        a16 <- optional $ getAttribute "ToolQualificationMethod" e pos
        commit $ interior e $ return (ToolQualification a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16)
            `apply` many (parseSchemaType "Reviews")
    schemaTypeToXML s x@ToolQualification{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ toolQualification_iD x
                       , maybe [] (toXMLAttribute "Name") $ toolQualification_name x
                       , maybe [] (toXMLAttribute "Description") $ toolQualification_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ toolQualification_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ toolQualification_comment x
                       , maybe [] (toXMLAttribute "Date") $ toolQualification_date x
                       , maybe [] (toXMLAttribute "Tool") $ toolQualification_tool x
                       , maybe [] (toXMLAttribute "UseCases") $ toolQualification_useCases x
                       , maybe [] (toXMLAttribute "Variant") $ toolQualification_variant x
                       , maybe [] (toXMLAttribute "Tests") $ toolQualification_tests x
                       , maybe [] (toXMLAttribute "InternalReference") $ toolQualification_internalReference x
                       , maybe [] (toXMLAttribute "Identifications") $ toolQualification_identifications x
                       , maybe [] (toXMLAttribute "Errors") $ toolQualification_errors x
                       , maybe [] (toXMLAttribute "Responsible") $ toolQualification_responsible x
                       , maybe [] (toXMLAttribute "QualificationResult") $ toolQualification_qualificationResult x
                       , maybe [] (toXMLAttribute "Method") $ toolQualification_method x
                       , maybe [] (toXMLAttribute "ToolQualificationMethod") $ toolQualification_toolQualificationMethod x
                       ]
            [ concatMap (schemaTypeToXML "Reviews") $ toolQualification_reviews x
            ]
instance Extension ToolQualification Qualification where
    supertype (ToolQualification a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 e0) =
               Qualification a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 e0
instance Extension ToolQualification IToolChainElement where
    supertype = (supertype :: Qualification -> IToolChainElement)
              . (supertype :: ToolQualification -> Qualification)
              
instance Extension ToolQualification IIdentifiable where
    supertype = (supertype :: IToolChainElement -> IIdentifiable)
              . (supertype :: Qualification -> IToolChainElement)
              . (supertype :: ToolQualification -> Qualification)
              
 
data LibraryQualification = LibraryQualification
        { libraryQualification_iD :: Maybe Ecore.EString
        , libraryQualification_name :: Maybe Ecore.EString
        , libraryQualification_description :: Maybe Ecore.EString
        , libraryQualification_longDescription :: Maybe Ecore.EString
        , libraryQualification_comment :: Maybe Ecore.EString
        , libraryQualification_date :: Maybe Ecore.EString
        , libraryQualification_tool :: Maybe Xsd.AnyURI
        , libraryQualification_useCases :: Maybe Xsd.XsdString
        , libraryQualification_variant :: Maybe Xsd.XsdString
        , libraryQualification_tests :: Maybe Xsd.XsdString
        , libraryQualification_internalReference :: Maybe Ecore.EString
        , libraryQualification_identifications :: Maybe Xsd.XsdString
        , libraryQualification_errors :: Maybe Xsd.XsdString
        , libraryQualification_responsible :: Maybe Ecore.EString
        , libraryQualification_qualificationResult :: Maybe QualificationResult
        , libraryQualification_method :: Maybe QualificationMethod
        , libraryQualification_libraryQualificationMethod :: Maybe LibraryQualificationMethod
        , libraryQualification_library :: Maybe Xsd.AnyURI
        , libraryQualification_reviews :: [Review]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType LibraryQualification where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "Date" e pos
        a6 <- optional $ getAttribute "Tool" e pos
        a7 <- optional $ getAttribute "UseCases" e pos
        a8 <- optional $ getAttribute "Variant" e pos
        a9 <- optional $ getAttribute "Tests" e pos
        a10 <- optional $ getAttribute "InternalReference" e pos
        a11 <- optional $ getAttribute "Identifications" e pos
        a12 <- optional $ getAttribute "Errors" e pos
        a13 <- optional $ getAttribute "Responsible" e pos
        a14 <- optional $ getAttribute "QualificationResult" e pos
        a15 <- optional $ getAttribute "Method" e pos
        a16 <- optional $ getAttribute "LibraryQualificationMethod" e pos
        a17 <- optional $ getAttribute "Library" e pos
        commit $ interior e $ return (LibraryQualification a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17)
            `apply` many (parseSchemaType "Reviews")
    schemaTypeToXML s x@LibraryQualification{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ libraryQualification_iD x
                       , maybe [] (toXMLAttribute "Name") $ libraryQualification_name x
                       , maybe [] (toXMLAttribute "Description") $ libraryQualification_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ libraryQualification_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ libraryQualification_comment x
                       , maybe [] (toXMLAttribute "Date") $ libraryQualification_date x
                       , maybe [] (toXMLAttribute "Tool") $ libraryQualification_tool x
                       , maybe [] (toXMLAttribute "UseCases") $ libraryQualification_useCases x
                       , maybe [] (toXMLAttribute "Variant") $ libraryQualification_variant x
                       , maybe [] (toXMLAttribute "Tests") $ libraryQualification_tests x
                       , maybe [] (toXMLAttribute "InternalReference") $ libraryQualification_internalReference x
                       , maybe [] (toXMLAttribute "Identifications") $ libraryQualification_identifications x
                       , maybe [] (toXMLAttribute "Errors") $ libraryQualification_errors x
                       , maybe [] (toXMLAttribute "Responsible") $ libraryQualification_responsible x
                       , maybe [] (toXMLAttribute "QualificationResult") $ libraryQualification_qualificationResult x
                       , maybe [] (toXMLAttribute "Method") $ libraryQualification_method x
                       , maybe [] (toXMLAttribute "LibraryQualificationMethod") $ libraryQualification_libraryQualificationMethod x
                       , maybe [] (toXMLAttribute "Library") $ libraryQualification_library x
                       ]
            [ concatMap (schemaTypeToXML "Reviews") $ libraryQualification_reviews x
            ]
instance Extension LibraryQualification Qualification where
    supertype (LibraryQualification a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 e0) =
               Qualification a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 e0
instance Extension LibraryQualification IToolChainElement where
    supertype = (supertype :: Qualification -> IToolChainElement)
              . (supertype :: LibraryQualification -> Qualification)
              
instance Extension LibraryQualification IIdentifiable where
    supertype = (supertype :: IToolChainElement -> IIdentifiable)
              . (supertype :: Qualification -> IToolChainElement)
              . (supertype :: LibraryQualification -> Qualification)
              
 
data LibraryQualificationMethod
    = LibraryQualificationMethod_UNQUALIFIED
    | LibraryQualificationMethod_ISO26262_SEOOC
    | LibraryQualificationMethod_ISO26262_8_12
    | LibraryQualificationMethod_PROVEN_IN_USE
    | LibraryQualificationMethod_EN_50128_7_3_4_7
    | LibraryQualificationMethod_IEC_61508_ROUTE_3S
    | LibraryQualificationMethod_STANDARD_COMPLIANT
    | LibraryQualificationMethod_CERTIFICATION
    | LibraryQualificationMethod_VALIDAS_QKIT
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType LibraryQualificationMethod where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType LibraryQualificationMethod where
    acceptingParser =  do literal "UNQUALIFIED"; return LibraryQualificationMethod_UNQUALIFIED
                      `onFail` do literal "ISO26262_SEOOC"; return LibraryQualificationMethod_ISO26262_SEOOC
                      `onFail` do literal "ISO26262_8_12"; return LibraryQualificationMethod_ISO26262_8_12
                      `onFail` do literal "PROVEN_IN_USE"; return LibraryQualificationMethod_PROVEN_IN_USE
                      `onFail` do literal "EN_50128_7_3_4_7"; return LibraryQualificationMethod_EN_50128_7_3_4_7
                      `onFail` do literal "IEC_61508_ROUTE_3S"; return LibraryQualificationMethod_IEC_61508_ROUTE_3S
                      `onFail` do literal "STANDARD_COMPLIANT"; return LibraryQualificationMethod_STANDARD_COMPLIANT
                      `onFail` do literal "CERTIFICATION"; return LibraryQualificationMethod_CERTIFICATION
                      `onFail` do literal "VALIDAS_QKIT"; return LibraryQualificationMethod_VALIDAS_QKIT
                      
    simpleTypeText LibraryQualificationMethod_UNQUALIFIED = "UNQUALIFIED"
    simpleTypeText LibraryQualificationMethod_ISO26262_SEOOC = "ISO26262_SEOOC"
    simpleTypeText LibraryQualificationMethod_ISO26262_8_12 = "ISO26262_8_12"
    simpleTypeText LibraryQualificationMethod_PROVEN_IN_USE = "PROVEN_IN_USE"
    simpleTypeText LibraryQualificationMethod_EN_50128_7_3_4_7 = "EN_50128_7_3_4_7"
    simpleTypeText LibraryQualificationMethod_IEC_61508_ROUTE_3S = "IEC_61508_ROUTE_3S"
    simpleTypeText LibraryQualificationMethod_STANDARD_COMPLIANT = "STANDARD_COMPLIANT"
    simpleTypeText LibraryQualificationMethod_CERTIFICATION = "CERTIFICATION"
    simpleTypeText LibraryQualificationMethod_VALIDAS_QKIT = "VALIDAS_QKIT"
 
data LibraryClassification
    = LibraryClassification_NEW
    | LibraryClassification_MODIFIED
    | LibraryClassification_UNCHANGED
    | LibraryClassification_UPDATED
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType LibraryClassification where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType LibraryClassification where
    acceptingParser =  do literal "NEW"; return LibraryClassification_NEW
                      `onFail` do literal "MODIFIED"; return LibraryClassification_MODIFIED
                      `onFail` do literal "UNCHANGED"; return LibraryClassification_UNCHANGED
                      `onFail` do literal "UPDATED"; return LibraryClassification_UPDATED
                      
    simpleTypeText LibraryClassification_NEW = "NEW"
    simpleTypeText LibraryClassification_MODIFIED = "MODIFIED"
    simpleTypeText LibraryClassification_UNCHANGED = "UNCHANGED"
    simpleTypeText LibraryClassification_UPDATED = "UPDATED"
 
data CodeCoverageMetric
    = CodeCoverageMetric_NOT_ANALYZED
    | CodeCoverageMetric_STATEMENT_COVERAGE
    | CodeCoverageMetric_BRANCH_COVERAGE
    | CodeCoverageMetric_CONDITION_COVERAGE
    | CodeCoverageMetric_MCDC_COVERAGE
    | CodeCoverageMetric_FUNCTION_COVERAGE
    deriving (GHCG.Generic,Eq,Show,Enum)
instance SchemaType CodeCoverageMetric where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CodeCoverageMetric where
    acceptingParser =  do literal "NOT_ANALYZED"; return CodeCoverageMetric_NOT_ANALYZED
                      `onFail` do literal "STATEMENT_COVERAGE"; return CodeCoverageMetric_STATEMENT_COVERAGE
                      `onFail` do literal "BRANCH_COVERAGE"; return CodeCoverageMetric_BRANCH_COVERAGE
                      `onFail` do literal "CONDITION_COVERAGE"; return CodeCoverageMetric_CONDITION_COVERAGE
                      `onFail` do literal "MCDC_COVERAGE"; return CodeCoverageMetric_MCDC_COVERAGE
                      `onFail` do literal "FUNCTION_COVERAGE"; return CodeCoverageMetric_FUNCTION_COVERAGE
                      
    simpleTypeText CodeCoverageMetric_NOT_ANALYZED = "NOT_ANALYZED"
    simpleTypeText CodeCoverageMetric_STATEMENT_COVERAGE = "STATEMENT_COVERAGE"
    simpleTypeText CodeCoverageMetric_BRANCH_COVERAGE = "BRANCH_COVERAGE"
    simpleTypeText CodeCoverageMetric_CONDITION_COVERAGE = "CONDITION_COVERAGE"
    simpleTypeText CodeCoverageMetric_MCDC_COVERAGE = "MCDC_COVERAGE"
    simpleTypeText CodeCoverageMetric_FUNCTION_COVERAGE = "FUNCTION_COVERAGE"
 
data ProgressTrackingState = ProgressTrackingState
        { progressTrackingState_iD :: Maybe Ecore.EString
        , progressTrackingState_name :: Maybe Ecore.EString
        , progressTrackingState_description :: Maybe Ecore.EString
        , progressTrackingState_longDescription :: Maybe Ecore.EString
        , progressTrackingState_comment :: Maybe Ecore.EString
        , progressTrackingState_order :: Ecore.EInt
        , progressTrackingState_associatedFeatures :: Maybe Xsd.XsdString
        , progressTrackingState_completionPercentage :: Ecore.EDouble
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType ProgressTrackingState where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- getAttribute "Order" e pos
        a6 <- optional $ getAttribute "AssociatedFeatures" e pos
        a7 <- getAttribute "CompletionPercentage" e pos
        commit $ interior e $ return (ProgressTrackingState a0 a1 a2 a3 a4 a5 a6 a7)
    schemaTypeToXML s x@ProgressTrackingState{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ progressTrackingState_iD x
                       , maybe [] (toXMLAttribute "Name") $ progressTrackingState_name x
                       , maybe [] (toXMLAttribute "Description") $ progressTrackingState_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ progressTrackingState_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ progressTrackingState_comment x
                       , toXMLAttribute "Order" $ progressTrackingState_order x
                       , maybe [] (toXMLAttribute "AssociatedFeatures") $ progressTrackingState_associatedFeatures x
                       , toXMLAttribute "CompletionPercentage" $ progressTrackingState_completionPercentage x
                       ]
            []
instance Extension ProgressTrackingState IIdentifiable where
    supertype v = IIdentifiable_ProgressTrackingState v
 
data CommandLineParameter = CommandLineParameter
        { commandLineParameter_iD :: Maybe Ecore.EString
        , commandLineParameter_name :: Maybe Ecore.EString
        , commandLineParameter_description :: Maybe Ecore.EString
        , commandLineParameter_longDescription :: Maybe Ecore.EString
        , commandLineParameter_comment :: Maybe Ecore.EString
        , commandLineParameter_value :: Maybe Ecore.EString
        , commandLineParameter_defaultValue :: Maybe Ecore.EString
        , commandLineParameter_orderNumber :: Maybe Ecore.EInt
        , commandLineParameter_type :: Maybe ParameterType
        , commandLineParameter_variant :: Maybe Xsd.XsdString
        , commandLineParameter_mandatory :: Maybe Ecore.EBoolean
        , commandLineParameter_multiLines :: Maybe Ecore.EInt
        , commandLineParameter_validationOnly :: Maybe Ecore.EBoolean
        , commandLineParameter_readOnly :: Maybe Ecore.EBoolean
        , commandLineParameter_inVisible :: Maybe Ecore.EBoolean
        , commandLineParameter_warnMessage :: Maybe Ecore.EString
        , commandLineParameter_confidential :: Maybe Ecore.EBoolean
        , commandLineParameter_relevantFeatures :: Maybe Xsd.XsdString
        , commandLineParameter_enumValues :: [EnumValue]
        , commandLineParameter_reviews :: [Review]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType CommandLineParameter where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ID" e pos
        a1 <- optional $ getAttribute "Name" e pos
        a2 <- optional $ getAttribute "Description" e pos
        a3 <- optional $ getAttribute "LongDescription" e pos
        a4 <- optional $ getAttribute "Comment" e pos
        a5 <- optional $ getAttribute "Value" e pos
        a6 <- optional $ getAttribute "DefaultValue" e pos
        a7 <- optional $ getAttribute "OrderNumber" e pos
        a8 <- optional $ getAttribute "Type" e pos
        a9 <- optional $ getAttribute "Variant" e pos
        a10 <- optional $ getAttribute "Mandatory" e pos
        a11 <- optional $ getAttribute "MultiLines" e pos
        a12 <- optional $ getAttribute "ValidationOnly" e pos
        a13 <- optional $ getAttribute "ReadOnly" e pos
        a14 <- optional $ getAttribute "InVisible" e pos
        a15 <- optional $ getAttribute "WarnMessage" e pos
        a16 <- optional $ getAttribute "Confidential" e pos
        a17 <- optional $ getAttribute "RelevantFeatures" e pos
        commit $ interior e $ return (CommandLineParameter a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17)
            `apply` many (parseSchemaType "EnumValues")
            `apply` many (parseSchemaType "Reviews")
    schemaTypeToXML s x@CommandLineParameter{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ID") $ commandLineParameter_iD x
                       , maybe [] (toXMLAttribute "Name") $ commandLineParameter_name x
                       , maybe [] (toXMLAttribute "Description") $ commandLineParameter_description x
                       , maybe [] (toXMLAttribute "LongDescription") $ commandLineParameter_longDescription x
                       , maybe [] (toXMLAttribute "Comment") $ commandLineParameter_comment x
                       , maybe [] (toXMLAttribute "Value") $ commandLineParameter_value x
                       , maybe [] (toXMLAttribute "DefaultValue") $ commandLineParameter_defaultValue x
                       , maybe [] (toXMLAttribute "OrderNumber") $ commandLineParameter_orderNumber x
                       , maybe [] (toXMLAttribute "Type") $ commandLineParameter_type x
                       , maybe [] (toXMLAttribute "Variant") $ commandLineParameter_variant x
                       , maybe [] (toXMLAttribute "Mandatory") $ commandLineParameter_mandatory x
                       , maybe [] (toXMLAttribute "MultiLines") $ commandLineParameter_multiLines x
                       , maybe [] (toXMLAttribute "ValidationOnly") $ commandLineParameter_validationOnly x
                       , maybe [] (toXMLAttribute "ReadOnly") $ commandLineParameter_readOnly x
                       , maybe [] (toXMLAttribute "InVisible") $ commandLineParameter_inVisible x
                       , maybe [] (toXMLAttribute "WarnMessage") $ commandLineParameter_warnMessage x
                       , maybe [] (toXMLAttribute "Confidential") $ commandLineParameter_confidential x
                       , maybe [] (toXMLAttribute "RelevantFeatures") $ commandLineParameter_relevantFeatures x
                       ]
            [ concatMap (schemaTypeToXML "EnumValues") $ commandLineParameter_enumValues x
            , concatMap (schemaTypeToXML "Reviews") $ commandLineParameter_reviews x
            ]
instance Extension CommandLineParameter Parameter where
    supertype (CommandLineParameter a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 e0 e1) =
               Parameter a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 e0 e1
instance Extension CommandLineParameter IIdentifiable where
    supertype = (supertype :: Parameter -> IIdentifiable)
              . (supertype :: CommandLineParameter -> Parameter)
              
 
--  (There are no subtypes defined for this abstract type.)
data WithNameAndPath = WithNameAndPath deriving (GHCG.Generic,Eq,Show)
instance SchemaType WithNameAndPath where
    parseSchemaType s = fail "Parse failed when expecting an extension type of WithNameAndPath:\n  No extension types are known."
    schemaTypeToXML s _ = toXMLElement s [] []
