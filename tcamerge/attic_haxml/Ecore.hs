{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Ecore
  ( module Ecore
  ) where
 
import qualified GHC.Generics as GHCG

import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementEAttribute :: XMLParser EAttribute
elementEAttribute = parseSchemaType "EAttribute"
elementToXMLEAttribute :: EAttribute -> [Content ()]
elementToXMLEAttribute = schemaTypeToXML "EAttribute"
 
elementEAnnotation :: XMLParser EAnnotation
elementEAnnotation = parseSchemaType "EAnnotation"
elementToXMLEAnnotation :: EAnnotation -> [Content ()]
elementToXMLEAnnotation = schemaTypeToXML "EAnnotation"
 
elementEClass :: XMLParser EClass
elementEClass = parseSchemaType "EClass"
elementToXMLEClass :: EClass -> [Content ()]
elementToXMLEClass = schemaTypeToXML "EClass"
 
elementEClassifier :: XMLParser EClassifier
elementEClassifier = parseSchemaType "EClassifier"
elementToXMLEClassifier :: EClassifier -> [Content ()]
elementToXMLEClassifier = schemaTypeToXML "EClassifier"
 
elementEDataType :: XMLParser EDataType
elementEDataType = parseSchemaType "EDataType"
elementToXMLEDataType :: EDataType -> [Content ()]
elementToXMLEDataType = schemaTypeToXML "EDataType"
 
elementEEnum :: XMLParser EEnum
elementEEnum = parseSchemaType "EEnum"
elementToXMLEEnum :: EEnum -> [Content ()]
elementToXMLEEnum = schemaTypeToXML "EEnum"
 
elementEEnumLiteral :: XMLParser EEnumLiteral
elementEEnumLiteral = parseSchemaType "EEnumLiteral"
elementToXMLEEnumLiteral :: EEnumLiteral -> [Content ()]
elementToXMLEEnumLiteral = schemaTypeToXML "EEnumLiteral"
 
elementEFactory :: XMLParser EFactory
elementEFactory = parseSchemaType "EFactory"
elementToXMLEFactory :: EFactory -> [Content ()]
elementToXMLEFactory = schemaTypeToXML "EFactory"
 
elementEModelElement :: XMLParser EModelElement
elementEModelElement = parseSchemaType "EModelElement"
elementToXMLEModelElement :: EModelElement -> [Content ()]
elementToXMLEModelElement = schemaTypeToXML "EModelElement"
 
elementENamedElement :: XMLParser ENamedElement
elementENamedElement = parseSchemaType "ENamedElement"
elementToXMLENamedElement :: ENamedElement -> [Content ()]
elementToXMLENamedElement = schemaTypeToXML "ENamedElement"
 
elementEObject :: XMLParser AnyElement
elementEObject = parseSchemaType "EObject"
elementToXMLEObject :: AnyElement -> [Content ()]
elementToXMLEObject = schemaTypeToXML "EObject"
 
elementEOperation :: XMLParser EOperation
elementEOperation = parseSchemaType "EOperation"
elementToXMLEOperation :: EOperation -> [Content ()]
elementToXMLEOperation = schemaTypeToXML "EOperation"
 
elementEPackage :: XMLParser EPackage
elementEPackage = parseSchemaType "EPackage"
elementToXMLEPackage :: EPackage -> [Content ()]
elementToXMLEPackage = schemaTypeToXML "EPackage"
 
elementEParameter :: XMLParser EParameter
elementEParameter = parseSchemaType "EParameter"
elementToXMLEParameter :: EParameter -> [Content ()]
elementToXMLEParameter = schemaTypeToXML "EParameter"
 
elementEReference :: XMLParser EReference
elementEReference = parseSchemaType "EReference"
elementToXMLEReference :: EReference -> [Content ()]
elementToXMLEReference = schemaTypeToXML "EReference"
 
elementEStructuralFeature :: XMLParser EStructuralFeature
elementEStructuralFeature = parseSchemaType "EStructuralFeature"
elementToXMLEStructuralFeature :: EStructuralFeature -> [Content ()]
elementToXMLEStructuralFeature = schemaTypeToXML "EStructuralFeature"
 
elementETypedElement :: XMLParser ETypedElement
elementETypedElement = parseSchemaType "ETypedElement"
elementToXMLETypedElement :: ETypedElement -> [Content ()]
elementToXMLETypedElement = schemaTypeToXML "ETypedElement"
 
elementEStringToStringMapEntry :: XMLParser EStringToStringMapEntry
elementEStringToStringMapEntry = parseSchemaType "EStringToStringMapEntry"
elementToXMLEStringToStringMapEntry :: EStringToStringMapEntry -> [Content ()]
elementToXMLEStringToStringMapEntry = schemaTypeToXML "EStringToStringMapEntry"
 
elementEGenericType :: XMLParser EGenericType
elementEGenericType = parseSchemaType "EGenericType"
elementToXMLEGenericType :: EGenericType -> [Content ()]
elementToXMLEGenericType = schemaTypeToXML "EGenericType"
 
elementETypeParameter :: XMLParser ETypeParameter
elementETypeParameter = parseSchemaType "ETypeParameter"
elementToXMLETypeParameter :: ETypeParameter -> [Content ()]
elementToXMLETypeParameter = schemaTypeToXML "ETypeParameter"
 
data EAttribute = EAttribute
        { eAttribute_name :: Maybe EString
        , eAttribute_ordered :: Maybe EBoolean
        , eAttribute_unique :: Maybe EBoolean
        , eAttribute_lowerBound :: Maybe EInt
        , eAttribute_upperBound :: Maybe EInt
        , eAttribute_many :: Maybe EBoolean
        , eAttribute_required :: Maybe EBoolean
        , eAttribute_eType :: Maybe Xsd.AnyURI
        , eAttribute_changeable :: Maybe EBoolean
        , eAttribute_volatile :: Maybe EBoolean
        , eAttribute_transient :: Maybe EBoolean
        , eAttribute_defaultValueLiteral :: Maybe EString
        , eAttribute_defaultValue :: Maybe EJavaObject
        , eAttribute_unsettable :: Maybe EBoolean
        , eAttribute_derived :: Maybe EBoolean
        , eAttribute_iD :: Maybe EBoolean
        , eAttribute_eAttributeType :: Maybe Xsd.AnyURI
        , eAttribute_eAnnotations :: [EAnnotation]
        , eAttribute_eGenericType :: Maybe EGenericType
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EAttribute where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "name" e pos
        a1 <- optional $ getAttribute "ordered" e pos
        a2 <- optional $ getAttribute "unique" e pos
        a3 <- optional $ getAttribute "lowerBound" e pos
        a4 <- optional $ getAttribute "upperBound" e pos
        a5 <- optional $ getAttribute "many" e pos
        a6 <- optional $ getAttribute "required" e pos
        a7 <- optional $ getAttribute "eType" e pos
        a8 <- optional $ getAttribute "changeable" e pos
        a9 <- optional $ getAttribute "volatile" e pos
        a10 <- optional $ getAttribute "transient" e pos
        a11 <- optional $ getAttribute "defaultValueLiteral" e pos
        a12 <- optional $ getAttribute "defaultValue" e pos
        a13 <- optional $ getAttribute "unsettable" e pos
        a14 <- optional $ getAttribute "derived" e pos
        a15 <- optional $ getAttribute "iD" e pos
        a16 <- optional $ getAttribute "eAttributeType" e pos
        commit $ interior e $ return (EAttribute a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16)
            `apply` many (parseSchemaType "eAnnotations")
            `apply` optional (parseSchemaType "eGenericType")
    schemaTypeToXML s x@EAttribute{} =
        toXMLElement s [ maybe [] (toXMLAttribute "name") $ eAttribute_name x
                       , maybe [] (toXMLAttribute "ordered") $ eAttribute_ordered x
                       , maybe [] (toXMLAttribute "unique") $ eAttribute_unique x
                       , maybe [] (toXMLAttribute "lowerBound") $ eAttribute_lowerBound x
                       , maybe [] (toXMLAttribute "upperBound") $ eAttribute_upperBound x
                       , maybe [] (toXMLAttribute "many") $ eAttribute_many x
                       , maybe [] (toXMLAttribute "required") $ eAttribute_required x
                       , maybe [] (toXMLAttribute "eType") $ eAttribute_eType x
                       , maybe [] (toXMLAttribute "changeable") $ eAttribute_changeable x
                       , maybe [] (toXMLAttribute "volatile") $ eAttribute_volatile x
                       , maybe [] (toXMLAttribute "transient") $ eAttribute_transient x
                       , maybe [] (toXMLAttribute "defaultValueLiteral") $ eAttribute_defaultValueLiteral x
                       , maybe [] (toXMLAttribute "defaultValue") $ eAttribute_defaultValue x
                       , maybe [] (toXMLAttribute "unsettable") $ eAttribute_unsettable x
                       , maybe [] (toXMLAttribute "derived") $ eAttribute_derived x
                       , maybe [] (toXMLAttribute "iD") $ eAttribute_iD x
                       , maybe [] (toXMLAttribute "eAttributeType") $ eAttribute_eAttributeType x
                       ]
            [ concatMap (schemaTypeToXML "eAnnotations") $ eAttribute_eAnnotations x
            , maybe [] (schemaTypeToXML "eGenericType") $ eAttribute_eGenericType x
            ]
instance Extension EAttribute EStructuralFeature where
    supertype v = EStructuralFeature_EAttribute v
instance Extension EAttribute ETypedElement where
    supertype = (supertype :: EStructuralFeature -> ETypedElement)
              . (supertype :: EAttribute -> EStructuralFeature)
              
instance Extension EAttribute ENamedElement where
    supertype = (supertype :: ETypedElement -> ENamedElement)
              . (supertype :: EStructuralFeature -> ETypedElement)
              . (supertype :: EAttribute -> EStructuralFeature)
              
instance Extension EAttribute EModelElement where
    supertype = (supertype :: ENamedElement -> EModelElement)
              . (supertype :: ETypedElement -> ENamedElement)
              . (supertype :: EStructuralFeature -> ETypedElement)
              . (supertype :: EAttribute -> EStructuralFeature)
              
 
data EAnnotation = EAnnotation
        { eAnnotation_source :: Maybe EString
        , eAnnotation_references :: Maybe Xsd.XsdString
        , eAnnotation_eAnnotations :: [EAnnotation]
        , eAnnotation_details :: [EStringToStringMapEntry]
        , eAnnotation_contents :: [AnyElement]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EAnnotation where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "source" e pos
        a1 <- optional $ getAttribute "references" e pos
        commit $ interior e $ return (EAnnotation a0 a1)
            `apply` many (parseSchemaType "eAnnotations")
            `apply` many (parseSchemaType "details")
            `apply` many (parseSchemaType "contents")
    schemaTypeToXML s x@EAnnotation{} =
        toXMLElement s [ maybe [] (toXMLAttribute "source") $ eAnnotation_source x
                       , maybe [] (toXMLAttribute "references") $ eAnnotation_references x
                       ]
            [ concatMap (schemaTypeToXML "eAnnotations") $ eAnnotation_eAnnotations x
            , concatMap (schemaTypeToXML "details") $ eAnnotation_details x
            , concatMap (schemaTypeToXML "contents") $ eAnnotation_contents x
            ]
instance Extension EAnnotation EModelElement where
    supertype v = EModelElement_EAnnotation v
 
data EClass = EClass
        { eClass_name :: Maybe EString
        , eClass_instanceClassName :: Maybe EString
        , eClass_instanceClass :: Maybe EJavaClass
        , eClass_defaultValue :: Maybe EJavaObject
        , eClass_instanceTypeName :: Maybe EString
        , eClass_abstract :: Maybe EBoolean
        , eClass_interface :: Maybe EBoolean
        , eClass_eSuperTypes :: Maybe Xsd.XsdString
        , eClass_eAllAttributes :: Maybe Xsd.XsdString
        , eClass_eAllReferences :: Maybe Xsd.XsdString
        , eClass_eReferences :: Maybe Xsd.XsdString
        , eClass_eAttributes :: Maybe Xsd.XsdString
        , eClass_eAllContainments :: Maybe Xsd.XsdString
        , eClass_eAllOperations :: Maybe Xsd.XsdString
        , eClass_eAllStructuralFeatures :: Maybe Xsd.XsdString
        , eClass_eAllSuperTypes :: Maybe Xsd.XsdString
        , eClass_eIDAttribute :: Maybe Xsd.IDREF
        , eClass_eAllGenericSuperTypes :: Maybe Xsd.XsdString
        , eClass_eAnnotations :: [EAnnotation]
        , eClass_eTypeParameters :: [ETypeParameter]
        , eClass_eOperations :: [EOperation]
        , eClass_eStructuralFeatures :: [EStructuralFeature]
        , eClass_eGenericSuperTypes :: [EGenericType]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EClass where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "name" e pos
        a1 <- optional $ getAttribute "instanceClassName" e pos
        a2 <- optional $ getAttribute "instanceClass" e pos
        a3 <- optional $ getAttribute "defaultValue" e pos
        a4 <- optional $ getAttribute "instanceTypeName" e pos
        a5 <- optional $ getAttribute "abstract" e pos
        a6 <- optional $ getAttribute "interface" e pos
        a7 <- optional $ getAttribute "eSuperTypes" e pos
        a8 <- optional $ getAttribute "eAllAttributes" e pos
        a9 <- optional $ getAttribute "eAllReferences" e pos
        a10 <- optional $ getAttribute "eReferences" e pos
        a11 <- optional $ getAttribute "eAttributes" e pos
        a12 <- optional $ getAttribute "eAllContainments" e pos
        a13 <- optional $ getAttribute "eAllOperations" e pos
        a14 <- optional $ getAttribute "eAllStructuralFeatures" e pos
        a15 <- optional $ getAttribute "eAllSuperTypes" e pos
        a16 <- optional $ getAttribute "eIDAttribute" e pos
        a17 <- optional $ getAttribute "eAllGenericSuperTypes" e pos
        commit $ interior e $ return (EClass a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17)
            `apply` many (parseSchemaType "eAnnotations")
            `apply` many (parseSchemaType "eTypeParameters")
            `apply` many (parseSchemaType "eOperations")
            `apply` many (parseSchemaType "eStructuralFeatures")
            `apply` many (parseSchemaType "eGenericSuperTypes")
    schemaTypeToXML s x@EClass{} =
        toXMLElement s [ maybe [] (toXMLAttribute "name") $ eClass_name x
                       , maybe [] (toXMLAttribute "instanceClassName") $ eClass_instanceClassName x
                       , maybe [] (toXMLAttribute "instanceClass") $ eClass_instanceClass x
                       , maybe [] (toXMLAttribute "defaultValue") $ eClass_defaultValue x
                       , maybe [] (toXMLAttribute "instanceTypeName") $ eClass_instanceTypeName x
                       , maybe [] (toXMLAttribute "abstract") $ eClass_abstract x
                       , maybe [] (toXMLAttribute "interface") $ eClass_interface x
                       , maybe [] (toXMLAttribute "eSuperTypes") $ eClass_eSuperTypes x
                       , maybe [] (toXMLAttribute "eAllAttributes") $ eClass_eAllAttributes x
                       , maybe [] (toXMLAttribute "eAllReferences") $ eClass_eAllReferences x
                       , maybe [] (toXMLAttribute "eReferences") $ eClass_eReferences x
                       , maybe [] (toXMLAttribute "eAttributes") $ eClass_eAttributes x
                       , maybe [] (toXMLAttribute "eAllContainments") $ eClass_eAllContainments x
                       , maybe [] (toXMLAttribute "eAllOperations") $ eClass_eAllOperations x
                       , maybe [] (toXMLAttribute "eAllStructuralFeatures") $ eClass_eAllStructuralFeatures x
                       , maybe [] (toXMLAttribute "eAllSuperTypes") $ eClass_eAllSuperTypes x
                       , maybe [] (toXMLAttribute "eIDAttribute") $ eClass_eIDAttribute x
                       , maybe [] (toXMLAttribute "eAllGenericSuperTypes") $ eClass_eAllGenericSuperTypes x
                       ]
            [ concatMap (schemaTypeToXML "eAnnotations") $ eClass_eAnnotations x
            , concatMap (schemaTypeToXML "eTypeParameters") $ eClass_eTypeParameters x
            , concatMap (schemaTypeToXML "eOperations") $ eClass_eOperations x
            , concatMap (schemaTypeToXML "eStructuralFeatures") $ eClass_eStructuralFeatures x
            , concatMap (schemaTypeToXML "eGenericSuperTypes") $ eClass_eGenericSuperTypes x
            ]
instance Extension EClass EClassifier where
    supertype v = EClassifier_EClass v
instance Extension EClass ENamedElement where
    supertype = (supertype :: EClassifier -> ENamedElement)
              . (supertype :: EClass -> EClassifier)
              
instance Extension EClass EModelElement where
    supertype = (supertype :: ENamedElement -> EModelElement)
              . (supertype :: EClassifier -> ENamedElement)
              . (supertype :: EClass -> EClassifier)
              
 
data EClassifier
        = EClassifier_EDataType EDataType
        | EClassifier_EClass EClass
        
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EClassifier where
    parseSchemaType s = do
        (fmap EClassifier_EDataType $ parseSchemaType s)
        `onFail`
        (fmap EClassifier_EClass $ parseSchemaType s)
        `onFail` fail "Parse failed when expecting an extension type of EClassifier,\n\
\  namely one of:\n\
\EDataType,EClass"
    schemaTypeToXML _s (EClassifier_EDataType x) = schemaTypeToXML "eDataType" x
    schemaTypeToXML _s (EClassifier_EClass x) = schemaTypeToXML "eClass" x
instance Extension EClassifier ENamedElement where
    supertype v = ENamedElement_EClassifier v
 
data EDataType = EDataType
        { eDataType_name :: Maybe EString
        , eDataType_instanceClassName :: Maybe EString
        , eDataType_instanceClass :: Maybe EJavaClass
        , eDataType_defaultValue :: Maybe EJavaObject
        , eDataType_instanceTypeName :: Maybe EString
        , eDataType_serializable :: Maybe EBoolean
        , eDataType_eAnnotations :: [EAnnotation]
        , eDataType_eTypeParameters :: [ETypeParameter]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EDataType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "name" e pos
        a1 <- optional $ getAttribute "instanceClassName" e pos
        a2 <- optional $ getAttribute "instanceClass" e pos
        a3 <- optional $ getAttribute "defaultValue" e pos
        a4 <- optional $ getAttribute "instanceTypeName" e pos
        a5 <- optional $ getAttribute "serializable" e pos
        commit $ interior e $ return (EDataType a0 a1 a2 a3 a4 a5)
            `apply` many (parseSchemaType "eAnnotations")
            `apply` many (parseSchemaType "eTypeParameters")
    schemaTypeToXML s x@EDataType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "name") $ eDataType_name x
                       , maybe [] (toXMLAttribute "instanceClassName") $ eDataType_instanceClassName x
                       , maybe [] (toXMLAttribute "instanceClass") $ eDataType_instanceClass x
                       , maybe [] (toXMLAttribute "defaultValue") $ eDataType_defaultValue x
                       , maybe [] (toXMLAttribute "instanceTypeName") $ eDataType_instanceTypeName x
                       , maybe [] (toXMLAttribute "serializable") $ eDataType_serializable x
                       ]
            [ concatMap (schemaTypeToXML "eAnnotations") $ eDataType_eAnnotations x
            , concatMap (schemaTypeToXML "eTypeParameters") $ eDataType_eTypeParameters x
            ]
instance Extension EDataType EClassifier where
    supertype v = EClassifier_EDataType v
instance Extension EDataType ENamedElement where
    supertype = (supertype :: EClassifier -> ENamedElement)
              . (supertype :: EDataType -> EClassifier)
              
instance Extension EDataType EModelElement where
    supertype = (supertype :: ENamedElement -> EModelElement)
              . (supertype :: EClassifier -> ENamedElement)
              . (supertype :: EDataType -> EClassifier)
              
 
data EEnum = EEnum
        { eEnum_name :: Maybe EString
        , eEnum_instanceClassName :: Maybe EString
        , eEnum_instanceClass :: Maybe EJavaClass
        , eEnum_defaultValue :: Maybe EJavaObject
        , eEnum_instanceTypeName :: Maybe EString
        , eEnum_serializable :: Maybe EBoolean
        , eEnum_eAnnotations :: [EAnnotation]
        , eEnum_eTypeParameters :: [ETypeParameter]
        , eEnum_eLiterals :: [EEnumLiteral]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EEnum where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "name" e pos
        a1 <- optional $ getAttribute "instanceClassName" e pos
        a2 <- optional $ getAttribute "instanceClass" e pos
        a3 <- optional $ getAttribute "defaultValue" e pos
        a4 <- optional $ getAttribute "instanceTypeName" e pos
        a5 <- optional $ getAttribute "serializable" e pos
        commit $ interior e $ return (EEnum a0 a1 a2 a3 a4 a5)
            `apply` many (parseSchemaType "eAnnotations")
            `apply` many (parseSchemaType "eTypeParameters")
            `apply` many (parseSchemaType "eLiterals")
    schemaTypeToXML s x@EEnum{} =
        toXMLElement s [ maybe [] (toXMLAttribute "name") $ eEnum_name x
                       , maybe [] (toXMLAttribute "instanceClassName") $ eEnum_instanceClassName x
                       , maybe [] (toXMLAttribute "instanceClass") $ eEnum_instanceClass x
                       , maybe [] (toXMLAttribute "defaultValue") $ eEnum_defaultValue x
                       , maybe [] (toXMLAttribute "instanceTypeName") $ eEnum_instanceTypeName x
                       , maybe [] (toXMLAttribute "serializable") $ eEnum_serializable x
                       ]
            [ concatMap (schemaTypeToXML "eAnnotations") $ eEnum_eAnnotations x
            , concatMap (schemaTypeToXML "eTypeParameters") $ eEnum_eTypeParameters x
            , concatMap (schemaTypeToXML "eLiterals") $ eEnum_eLiterals x
            ]
instance Extension EEnum EDataType where
    supertype (EEnum a0 a1 a2 a3 a4 a5 e0 e1 e2) =
               EDataType a0 a1 a2 a3 a4 a5 e0 e1
instance Extension EEnum EClassifier where
    supertype = (supertype :: EDataType -> EClassifier)
              . (supertype :: EEnum -> EDataType)
              
instance Extension EEnum ENamedElement where
    supertype = (supertype :: EClassifier -> ENamedElement)
              . (supertype :: EDataType -> EClassifier)
              . (supertype :: EEnum -> EDataType)
              
instance Extension EEnum EModelElement where
    supertype = (supertype :: ENamedElement -> EModelElement)
              . (supertype :: EClassifier -> ENamedElement)
              . (supertype :: EDataType -> EClassifier)
              . (supertype :: EEnum -> EDataType)
              
 
data EEnumLiteral = EEnumLiteral
        { eEnumLiteral_name :: Maybe EString
        , eEnumLiteral_value :: Maybe EInt
        , eEnumLiteral_instance :: Maybe EEnumerator
        , eEnumLiteral_literal :: Maybe EString
        , eEnumLiteral_eAnnotations :: [EAnnotation]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EEnumLiteral where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "name" e pos
        a1 <- optional $ getAttribute "value" e pos
        a2 <- optional $ getAttribute "instance" e pos
        a3 <- optional $ getAttribute "literal" e pos
        commit $ interior e $ return (EEnumLiteral a0 a1 a2 a3)
            `apply` many (parseSchemaType "eAnnotations")
    schemaTypeToXML s x@EEnumLiteral{} =
        toXMLElement s [ maybe [] (toXMLAttribute "name") $ eEnumLiteral_name x
                       , maybe [] (toXMLAttribute "value") $ eEnumLiteral_value x
                       , maybe [] (toXMLAttribute "instance") $ eEnumLiteral_instance x
                       , maybe [] (toXMLAttribute "literal") $ eEnumLiteral_literal x
                       ]
            [ concatMap (schemaTypeToXML "eAnnotations") $ eEnumLiteral_eAnnotations x
            ]
instance Extension EEnumLiteral ENamedElement where
    supertype v = ENamedElement_EEnumLiteral v
instance Extension EEnumLiteral EModelElement where
    supertype = (supertype :: ENamedElement -> EModelElement)
              . (supertype :: EEnumLiteral -> ENamedElement)
              
 
data EFactory = EFactory
        { eFactory_ePackage :: Maybe Xsd.IDREF
        , eFactory_eAnnotations :: [EAnnotation]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EFactory where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "ePackage" e pos
        commit $ interior e $ return (EFactory a0)
            `apply` many (parseSchemaType "eAnnotations")
    schemaTypeToXML s x@EFactory{} =
        toXMLElement s [ maybe [] (toXMLAttribute "ePackage") $ eFactory_ePackage x
                       ]
            [ concatMap (schemaTypeToXML "eAnnotations") $ eFactory_eAnnotations x
            ]
instance Extension EFactory EModelElement where
    supertype v = EModelElement_EFactory v
 
data EModelElement
        = EModelElement_ENamedElement ENamedElement
        | EModelElement_EFactory EFactory
        | EModelElement_EAnnotation EAnnotation
        
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EModelElement where
    parseSchemaType s = do
        (fmap EModelElement_ENamedElement $ parseSchemaType s)
        `onFail`
        (fmap EModelElement_EFactory $ parseSchemaType s)
        `onFail`
        (fmap EModelElement_EAnnotation $ parseSchemaType s)
        `onFail` fail "Parse failed when expecting an extension type of EModelElement,\n\
\  namely one of:\n\
\ENamedElement,EFactory,EAnnotation"
    schemaTypeToXML _s (EModelElement_ENamedElement x) = schemaTypeToXML "eNamedElement" x
    schemaTypeToXML _s (EModelElement_EFactory x) = schemaTypeToXML "eFactory" x
    schemaTypeToXML _s (EModelElement_EAnnotation x) = schemaTypeToXML "eAnnotation" x
 
data ENamedElement
        = ENamedElement_ETypeParameter ETypeParameter
        | ENamedElement_ETypedElement ETypedElement
        | ENamedElement_EPackage EPackage
        | ENamedElement_EEnumLiteral EEnumLiteral
        | ENamedElement_EClassifier EClassifier
        
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType ENamedElement where
    parseSchemaType s = do
        (fmap ENamedElement_ETypeParameter $ parseSchemaType s)
        `onFail`
        (fmap ENamedElement_ETypedElement $ parseSchemaType s)
        `onFail`
        (fmap ENamedElement_EPackage $ parseSchemaType s)
        `onFail`
        (fmap ENamedElement_EEnumLiteral $ parseSchemaType s)
        `onFail`
        (fmap ENamedElement_EClassifier $ parseSchemaType s)
        `onFail` fail "Parse failed when expecting an extension type of ENamedElement,\n\
\  namely one of:\n\
\ETypeParameter,ETypedElement,EPackage,EEnumLiteral,EClassifier"
    schemaTypeToXML _s (ENamedElement_ETypeParameter x) = schemaTypeToXML "eTypeParameter" x
    schemaTypeToXML _s (ENamedElement_ETypedElement x) = schemaTypeToXML "eTypedElement" x
    schemaTypeToXML _s (ENamedElement_EPackage x) = schemaTypeToXML "ePackage" x
    schemaTypeToXML _s (ENamedElement_EEnumLiteral x) = schemaTypeToXML "eEnumLiteral" x
    schemaTypeToXML _s (ENamedElement_EClassifier x) = schemaTypeToXML "eClassifier" x
instance Extension ENamedElement EModelElement where
    supertype v = EModelElement_ENamedElement v
 
data EObject = EObject
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EObject where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EObject
    schemaTypeToXML s x@EObject{} =
        toXMLElement s []
            []
 
data EOperation = EOperation
        { eOperation_name :: Maybe EString
        , eOperation_ordered :: Maybe EBoolean
        , eOperation_unique :: Maybe EBoolean
        , eOperation_lowerBound :: Maybe EInt
        , eOperation_upperBound :: Maybe EInt
        , eOperation_many :: Maybe EBoolean
        , eOperation_required :: Maybe EBoolean
        , eOperation_eType :: Maybe Xsd.AnyURI
        , eOperation_eExceptions :: Maybe Xsd.XsdString
        , eOperation_eAnnotations :: [EAnnotation]
        , eOperation_eGenericType :: Maybe EGenericType
        , eOperation_eTypeParameters :: [ETypeParameter]
        , eOperation_eParameters :: [EParameter]
        , eOperation_eGenericExceptions :: [EGenericType]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EOperation where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "name" e pos
        a1 <- optional $ getAttribute "ordered" e pos
        a2 <- optional $ getAttribute "unique" e pos
        a3 <- optional $ getAttribute "lowerBound" e pos
        a4 <- optional $ getAttribute "upperBound" e pos
        a5 <- optional $ getAttribute "many" e pos
        a6 <- optional $ getAttribute "required" e pos
        a7 <- optional $ getAttribute "eType" e pos
        a8 <- optional $ getAttribute "eExceptions" e pos
        commit $ interior e $ return (EOperation a0 a1 a2 a3 a4 a5 a6 a7 a8)
            `apply` many (parseSchemaType "eAnnotations")
            `apply` optional (parseSchemaType "eGenericType")
            `apply` many (parseSchemaType "eTypeParameters")
            `apply` many (parseSchemaType "eParameters")
            `apply` many (parseSchemaType "eGenericExceptions")
    schemaTypeToXML s x@EOperation{} =
        toXMLElement s [ maybe [] (toXMLAttribute "name") $ eOperation_name x
                       , maybe [] (toXMLAttribute "ordered") $ eOperation_ordered x
                       , maybe [] (toXMLAttribute "unique") $ eOperation_unique x
                       , maybe [] (toXMLAttribute "lowerBound") $ eOperation_lowerBound x
                       , maybe [] (toXMLAttribute "upperBound") $ eOperation_upperBound x
                       , maybe [] (toXMLAttribute "many") $ eOperation_many x
                       , maybe [] (toXMLAttribute "required") $ eOperation_required x
                       , maybe [] (toXMLAttribute "eType") $ eOperation_eType x
                       , maybe [] (toXMLAttribute "eExceptions") $ eOperation_eExceptions x
                       ]
            [ concatMap (schemaTypeToXML "eAnnotations") $ eOperation_eAnnotations x
            , maybe [] (schemaTypeToXML "eGenericType") $ eOperation_eGenericType x
            , concatMap (schemaTypeToXML "eTypeParameters") $ eOperation_eTypeParameters x
            , concatMap (schemaTypeToXML "eParameters") $ eOperation_eParameters x
            , concatMap (schemaTypeToXML "eGenericExceptions") $ eOperation_eGenericExceptions x
            ]
instance Extension EOperation ETypedElement where
    supertype v = ETypedElement_EOperation v
instance Extension EOperation ENamedElement where
    supertype = (supertype :: ETypedElement -> ENamedElement)
              . (supertype :: EOperation -> ETypedElement)
              
instance Extension EOperation EModelElement where
    supertype = (supertype :: ENamedElement -> EModelElement)
              . (supertype :: ETypedElement -> ENamedElement)
              . (supertype :: EOperation -> ETypedElement)
              
 
data EPackage = EPackage
        { ePackage_name :: Maybe EString
        , ePackage_nsURI :: Maybe EString
        , ePackage_nsPrefix :: Maybe EString
        , ePackage_eFactoryInstance :: Maybe Xsd.IDREF
        , ePackage_eAnnotations :: [EAnnotation]
        , ePackage_eClassifiers :: [EClassifier]
        , ePackage_eSubpackages :: [EPackage]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EPackage where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "name" e pos
        a1 <- optional $ getAttribute "nsURI" e pos
        a2 <- optional $ getAttribute "nsPrefix" e pos
        a3 <- optional $ getAttribute "eFactoryInstance" e pos
        commit $ interior e $ return (EPackage a0 a1 a2 a3)
            `apply` many (parseSchemaType "eAnnotations")
            `apply` many (parseSchemaType "eClassifiers")
            `apply` many (parseSchemaType "eSubpackages")
    schemaTypeToXML s x@EPackage{} =
        toXMLElement s [ maybe [] (toXMLAttribute "name") $ ePackage_name x
                       , maybe [] (toXMLAttribute "nsURI") $ ePackage_nsURI x
                       , maybe [] (toXMLAttribute "nsPrefix") $ ePackage_nsPrefix x
                       , maybe [] (toXMLAttribute "eFactoryInstance") $ ePackage_eFactoryInstance x
                       ]
            [ concatMap (schemaTypeToXML "eAnnotations") $ ePackage_eAnnotations x
            , concatMap (schemaTypeToXML "eClassifiers") $ ePackage_eClassifiers x
            , concatMap (schemaTypeToXML "eSubpackages") $ ePackage_eSubpackages x
            ]
instance Extension EPackage ENamedElement where
    supertype v = ENamedElement_EPackage v
instance Extension EPackage EModelElement where
    supertype = (supertype :: ENamedElement -> EModelElement)
              . (supertype :: EPackage -> ENamedElement)
              
 
data EParameter = EParameter
        { eParameter_name :: Maybe EString
        , eParameter_ordered :: Maybe EBoolean
        , eParameter_unique :: Maybe EBoolean
        , eParameter_lowerBound :: Maybe EInt
        , eParameter_upperBound :: Maybe EInt
        , eParameter_many :: Maybe EBoolean
        , eParameter_required :: Maybe EBoolean
        , eParameter_eType :: Maybe Xsd.AnyURI
        , eParameter_eAnnotations :: [EAnnotation]
        , eParameter_eGenericType :: Maybe EGenericType
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EParameter where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "name" e pos
        a1 <- optional $ getAttribute "ordered" e pos
        a2 <- optional $ getAttribute "unique" e pos
        a3 <- optional $ getAttribute "lowerBound" e pos
        a4 <- optional $ getAttribute "upperBound" e pos
        a5 <- optional $ getAttribute "many" e pos
        a6 <- optional $ getAttribute "required" e pos
        a7 <- optional $ getAttribute "eType" e pos
        commit $ interior e $ return (EParameter a0 a1 a2 a3 a4 a5 a6 a7)
            `apply` many (parseSchemaType "eAnnotations")
            `apply` optional (parseSchemaType "eGenericType")
    schemaTypeToXML s x@EParameter{} =
        toXMLElement s [ maybe [] (toXMLAttribute "name") $ eParameter_name x
                       , maybe [] (toXMLAttribute "ordered") $ eParameter_ordered x
                       , maybe [] (toXMLAttribute "unique") $ eParameter_unique x
                       , maybe [] (toXMLAttribute "lowerBound") $ eParameter_lowerBound x
                       , maybe [] (toXMLAttribute "upperBound") $ eParameter_upperBound x
                       , maybe [] (toXMLAttribute "many") $ eParameter_many x
                       , maybe [] (toXMLAttribute "required") $ eParameter_required x
                       , maybe [] (toXMLAttribute "eType") $ eParameter_eType x
                       ]
            [ concatMap (schemaTypeToXML "eAnnotations") $ eParameter_eAnnotations x
            , maybe [] (schemaTypeToXML "eGenericType") $ eParameter_eGenericType x
            ]
instance Extension EParameter ETypedElement where
    supertype v = ETypedElement_EParameter v
instance Extension EParameter ENamedElement where
    supertype = (supertype :: ETypedElement -> ENamedElement)
              . (supertype :: EParameter -> ETypedElement)
              
instance Extension EParameter EModelElement where
    supertype = (supertype :: ENamedElement -> EModelElement)
              . (supertype :: ETypedElement -> ENamedElement)
              . (supertype :: EParameter -> ETypedElement)
              
 
data EReference = EReference
        { eReference_name :: Maybe EString
        , eReference_ordered :: Maybe EBoolean
        , eReference_unique :: Maybe EBoolean
        , eReference_lowerBound :: Maybe EInt
        , eReference_upperBound :: Maybe EInt
        , eReference_many :: Maybe EBoolean
        , eReference_required :: Maybe EBoolean
        , eReference_eType :: Maybe Xsd.AnyURI
        , eReference_changeable :: Maybe EBoolean
        , eReference_volatile :: Maybe EBoolean
        , eReference_transient :: Maybe EBoolean
        , eReference_defaultValueLiteral :: Maybe EString
        , eReference_defaultValue :: Maybe EJavaObject
        , eReference_unsettable :: Maybe EBoolean
        , eReference_derived :: Maybe EBoolean
        , eReference_containment :: Maybe EBoolean
        , eReference_container :: Maybe EBoolean
        , eReference_resolveProxies :: Maybe EBoolean
        , eReference_eOpposite :: Maybe Xsd.AnyURI
        , eReference_eReferenceType :: Maybe Xsd.AnyURI
        , eReference_eKeys :: Maybe Xsd.XsdString
        , eReference_eAnnotations :: [EAnnotation]
        , eReference_eGenericType :: Maybe EGenericType
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EReference where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "name" e pos
        a1 <- optional $ getAttribute "ordered" e pos
        a2 <- optional $ getAttribute "unique" e pos
        a3 <- optional $ getAttribute "lowerBound" e pos
        a4 <- optional $ getAttribute "upperBound" e pos
        a5 <- optional $ getAttribute "many" e pos
        a6 <- optional $ getAttribute "required" e pos
        a7 <- optional $ getAttribute "eType" e pos
        a8 <- optional $ getAttribute "changeable" e pos
        a9 <- optional $ getAttribute "volatile" e pos
        a10 <- optional $ getAttribute "transient" e pos
        a11 <- optional $ getAttribute "defaultValueLiteral" e pos
        a12 <- optional $ getAttribute "defaultValue" e pos
        a13 <- optional $ getAttribute "unsettable" e pos
        a14 <- optional $ getAttribute "derived" e pos
        a15 <- optional $ getAttribute "containment" e pos
        a16 <- optional $ getAttribute "container" e pos
        a17 <- optional $ getAttribute "resolveProxies" e pos
        a18 <- optional $ getAttribute "eOpposite" e pos
        a19 <- optional $ getAttribute "eReferenceType" e pos
        a20 <- optional $ getAttribute "eKeys" e pos
        commit $ interior e $ return (EReference a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20)
            `apply` many (parseSchemaType "eAnnotations")
            `apply` optional (parseSchemaType "eGenericType")
    schemaTypeToXML s x@EReference{} =
        toXMLElement s [ maybe [] (toXMLAttribute "name") $ eReference_name x
                       , maybe [] (toXMLAttribute "ordered") $ eReference_ordered x
                       , maybe [] (toXMLAttribute "unique") $ eReference_unique x
                       , maybe [] (toXMLAttribute "lowerBound") $ eReference_lowerBound x
                       , maybe [] (toXMLAttribute "upperBound") $ eReference_upperBound x
                       , maybe [] (toXMLAttribute "many") $ eReference_many x
                       , maybe [] (toXMLAttribute "required") $ eReference_required x
                       , maybe [] (toXMLAttribute "eType") $ eReference_eType x
                       , maybe [] (toXMLAttribute "changeable") $ eReference_changeable x
                       , maybe [] (toXMLAttribute "volatile") $ eReference_volatile x
                       , maybe [] (toXMLAttribute "transient") $ eReference_transient x
                       , maybe [] (toXMLAttribute "defaultValueLiteral") $ eReference_defaultValueLiteral x
                       , maybe [] (toXMLAttribute "defaultValue") $ eReference_defaultValue x
                       , maybe [] (toXMLAttribute "unsettable") $ eReference_unsettable x
                       , maybe [] (toXMLAttribute "derived") $ eReference_derived x
                       , maybe [] (toXMLAttribute "containment") $ eReference_containment x
                       , maybe [] (toXMLAttribute "container") $ eReference_container x
                       , maybe [] (toXMLAttribute "resolveProxies") $ eReference_resolveProxies x
                       , maybe [] (toXMLAttribute "eOpposite") $ eReference_eOpposite x
                       , maybe [] (toXMLAttribute "eReferenceType") $ eReference_eReferenceType x
                       , maybe [] (toXMLAttribute "eKeys") $ eReference_eKeys x
                       ]
            [ concatMap (schemaTypeToXML "eAnnotations") $ eReference_eAnnotations x
            , maybe [] (schemaTypeToXML "eGenericType") $ eReference_eGenericType x
            ]
instance Extension EReference EStructuralFeature where
    supertype v = EStructuralFeature_EReference v
instance Extension EReference ETypedElement where
    supertype = (supertype :: EStructuralFeature -> ETypedElement)
              . (supertype :: EReference -> EStructuralFeature)
              
instance Extension EReference ENamedElement where
    supertype = (supertype :: ETypedElement -> ENamedElement)
              . (supertype :: EStructuralFeature -> ETypedElement)
              . (supertype :: EReference -> EStructuralFeature)
              
instance Extension EReference EModelElement where
    supertype = (supertype :: ENamedElement -> EModelElement)
              . (supertype :: ETypedElement -> ENamedElement)
              . (supertype :: EStructuralFeature -> ETypedElement)
              . (supertype :: EReference -> EStructuralFeature)
              
 
data EStructuralFeature
        = EStructuralFeature_EReference EReference
        | EStructuralFeature_EAttribute EAttribute
        
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EStructuralFeature where
    parseSchemaType s = do
        (fmap EStructuralFeature_EReference $ parseSchemaType s)
        `onFail`
        (fmap EStructuralFeature_EAttribute $ parseSchemaType s)
        `onFail` fail "Parse failed when expecting an extension type of EStructuralFeature,\n\
\  namely one of:\n\
\EReference,EAttribute"
    schemaTypeToXML _s (EStructuralFeature_EReference x) = schemaTypeToXML "eReference" x
    schemaTypeToXML _s (EStructuralFeature_EAttribute x) = schemaTypeToXML "eAttribute" x
instance Extension EStructuralFeature ETypedElement where
    supertype v = ETypedElement_EStructuralFeature v
 
data ETypedElement
        = ETypedElement_EStructuralFeature EStructuralFeature
        | ETypedElement_EParameter EParameter
        | ETypedElement_EOperation EOperation
        
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType ETypedElement where
    parseSchemaType s = do
        (fmap ETypedElement_EStructuralFeature $ parseSchemaType s)
        `onFail`
        (fmap ETypedElement_EParameter $ parseSchemaType s)
        `onFail`
        (fmap ETypedElement_EOperation $ parseSchemaType s)
        `onFail` fail "Parse failed when expecting an extension type of ETypedElement,\n\
\  namely one of:\n\
\EStructuralFeature,EParameter,EOperation"
    schemaTypeToXML _s (ETypedElement_EStructuralFeature x) = schemaTypeToXML "eStructuralFeature" x
    schemaTypeToXML _s (ETypedElement_EParameter x) = schemaTypeToXML "eParameter" x
    schemaTypeToXML _s (ETypedElement_EOperation x) = schemaTypeToXML "eOperation" x
instance Extension ETypedElement ENamedElement where
    supertype v = ENamedElement_ETypedElement v
 
newtype EBigDecimal = EBigDecimal Xsd.Decimal deriving (GHCG.Generic,Eq,Show)
instance Restricts EBigDecimal Xsd.Decimal where
    restricts (EBigDecimal x) = x
instance SchemaType EBigDecimal where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EBigDecimal x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EBigDecimal where
    acceptingParser = fmap EBigDecimal acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EBigDecimal x) = simpleTypeText x
 
newtype EBigInteger = EBigInteger Xsd.Integer deriving (GHCG.Generic,Eq,Show)
instance Restricts EBigInteger Xsd.Integer where
    restricts (EBigInteger x) = x
instance SchemaType EBigInteger where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EBigInteger x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EBigInteger where
    acceptingParser = fmap EBigInteger acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EBigInteger x) = simpleTypeText x
 
newtype EBoolean = EBoolean Xsd.XsdString deriving (GHCG.Generic,Eq,Show)
instance Restricts EBoolean Xsd.XsdString where
    restricts (EBoolean x) = x
instance SchemaType EBoolean where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EBoolean x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EBoolean where
    acceptingParser = fmap EBoolean acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EBoolean x) = simpleTypeText x
 
newtype EBooleanObject = EBooleanObject EBoolean deriving (GHCG.Generic,Eq,Show)
instance Restricts EBooleanObject EBoolean where
    restricts (EBooleanObject x) = x
instance SchemaType EBooleanObject where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EBooleanObject x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EBooleanObject where
    acceptingParser = fmap EBooleanObject acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EBooleanObject x) = simpleTypeText x
 
newtype EByte = EByte Xsd.Byte deriving (GHCG.Generic,Eq,Show)
instance Restricts EByte Xsd.Byte where
    restricts (EByte x) = x
instance SchemaType EByte where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EByte x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EByte where
    acceptingParser = fmap EByte acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EByte x) = simpleTypeText x
 
newtype EByteArray = EByteArray Xsd.HexBinary deriving (GHCG.Generic,Eq,Show)
instance Restricts EByteArray Xsd.HexBinary where
    restricts (EByteArray x) = x
instance SchemaType EByteArray where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EByteArray x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EByteArray where
    acceptingParser = fmap EByteArray acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EByteArray x) = simpleTypeText x
 
newtype EByteObject = EByteObject EByte deriving (GHCG.Generic,Eq,Show)
instance Restricts EByteObject EByte where
    restricts (EByteObject x) = x
instance SchemaType EByteObject where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EByteObject x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EByteObject where
    acceptingParser = fmap EByteObject acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EByteObject x) = simpleTypeText x
 
newtype EChar = EChar Xsd.XsdString deriving (GHCG.Generic,Eq,Show)
instance Restricts EChar Xsd.XsdString where
    restricts (EChar x) = x
instance SchemaType EChar where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EChar x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EChar where
    acceptingParser = fmap EChar acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EChar x) = simpleTypeText x
 
newtype ECharacterObject = ECharacterObject EChar deriving (GHCG.Generic,Eq,Show)
instance Restricts ECharacterObject EChar where
    restricts (ECharacterObject x) = x
instance SchemaType ECharacterObject where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (ECharacterObject x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ECharacterObject where
    acceptingParser = fmap ECharacterObject acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (ECharacterObject x) = simpleTypeText x
 
newtype EDate = EDate Xsd.XsdString deriving (GHCG.Generic,Eq,Show)
instance Restricts EDate Xsd.XsdString where
    restricts (EDate x) = x
instance SchemaType EDate where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EDate x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EDate where
    acceptingParser = fmap EDate acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EDate x) = simpleTypeText x
 
newtype EDiagnosticChain = EDiagnosticChain Xsd.XsdString deriving (GHCG.Generic,Eq,Show)
instance Restricts EDiagnosticChain Xsd.XsdString where
    restricts (EDiagnosticChain x) = x
instance SchemaType EDiagnosticChain where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EDiagnosticChain x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EDiagnosticChain where
    acceptingParser = fmap EDiagnosticChain acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EDiagnosticChain x) = simpleTypeText x
 
newtype EDouble = EDouble Xsd.Double deriving (GHCG.Generic,Eq,Show)
instance Restricts EDouble Xsd.Double where
    restricts (EDouble x) = x
instance SchemaType EDouble where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EDouble x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EDouble where
    acceptingParser = fmap EDouble acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EDouble x) = simpleTypeText x
 
newtype EDoubleObject = EDoubleObject EDouble deriving (GHCG.Generic,Eq,Show)
instance Restricts EDoubleObject EDouble where
    restricts (EDoubleObject x) = x
instance SchemaType EDoubleObject where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EDoubleObject x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EDoubleObject where
    acceptingParser = fmap EDoubleObject acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EDoubleObject x) = simpleTypeText x
 
newtype EEList = EEList Xsd.XsdString deriving (GHCG.Generic,Eq,Show)
instance Restricts EEList Xsd.XsdString where
    restricts (EEList x) = x
instance SchemaType EEList where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EEList x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EEList where
    acceptingParser = fmap EEList acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EEList x) = simpleTypeText x
 
newtype EEnumerator = EEnumerator Xsd.XsdString deriving (GHCG.Generic,Eq,Show)
instance Restricts EEnumerator Xsd.XsdString where
    restricts (EEnumerator x) = x
instance SchemaType EEnumerator where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EEnumerator x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EEnumerator where
    acceptingParser = fmap EEnumerator acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EEnumerator x) = simpleTypeText x
 
newtype EFeatureMap = EFeatureMap Xsd.XsdString deriving (GHCG.Generic,Eq,Show)
instance Restricts EFeatureMap Xsd.XsdString where
    restricts (EFeatureMap x) = x
instance SchemaType EFeatureMap where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EFeatureMap x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EFeatureMap where
    acceptingParser = fmap EFeatureMap acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EFeatureMap x) = simpleTypeText x
 
newtype EFeatureMapEntry = EFeatureMapEntry Xsd.XsdString deriving (GHCG.Generic,Eq,Show)
instance Restricts EFeatureMapEntry Xsd.XsdString where
    restricts (EFeatureMapEntry x) = x
instance SchemaType EFeatureMapEntry where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EFeatureMapEntry x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EFeatureMapEntry where
    acceptingParser = fmap EFeatureMapEntry acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EFeatureMapEntry x) = simpleTypeText x
 
newtype EFloat = EFloat Xsd.Float deriving (GHCG.Generic,Eq,Show)
instance Restricts EFloat Xsd.Float where
    restricts (EFloat x) = x
instance SchemaType EFloat where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EFloat x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EFloat where
    acceptingParser = fmap EFloat acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EFloat x) = simpleTypeText x
 
newtype EFloatObject = EFloatObject EFloat deriving (GHCG.Generic,Eq,Show)
instance Restricts EFloatObject EFloat where
    restricts (EFloatObject x) = x
instance SchemaType EFloatObject where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EFloatObject x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EFloatObject where
    acceptingParser = fmap EFloatObject acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EFloatObject x) = simpleTypeText x
 
newtype EInt = EInt Xsd.XsdString deriving (GHCG.Generic,Eq,Show)
instance Restricts EInt Xsd.XsdString where
    restricts (EInt x) = x
instance SchemaType EInt where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EInt x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EInt where
    acceptingParser = fmap EInt acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EInt x) = simpleTypeText x
 
newtype EIntegerObject = EIntegerObject EInt deriving (GHCG.Generic,Eq,Show)
instance Restricts EIntegerObject EInt where
    restricts (EIntegerObject x) = x
instance SchemaType EIntegerObject where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EIntegerObject x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EIntegerObject where
    acceptingParser = fmap EIntegerObject acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EIntegerObject x) = simpleTypeText x
 
newtype EJavaClass = EJavaClass Xsd.XsdString deriving (GHCG.Generic,Eq,Show)
instance Restricts EJavaClass Xsd.XsdString where
    restricts (EJavaClass x) = x
instance SchemaType EJavaClass where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EJavaClass x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EJavaClass where
    acceptingParser = fmap EJavaClass acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EJavaClass x) = simpleTypeText x
 
newtype EJavaObject = EJavaObject Xsd.XsdString deriving (GHCG.Generic,Eq,Show)
instance Restricts EJavaObject Xsd.XsdString where
    restricts (EJavaObject x) = x
instance SchemaType EJavaObject where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EJavaObject x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EJavaObject where
    acceptingParser = fmap EJavaObject acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EJavaObject x) = simpleTypeText x
 
newtype ELong = ELong Xsd.Long deriving (GHCG.Generic,Eq,Show)
instance Restricts ELong Xsd.Long where
    restricts (ELong x) = x
instance SchemaType ELong where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (ELong x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ELong where
    acceptingParser = fmap ELong acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (ELong x) = simpleTypeText x
 
newtype ELongObject = ELongObject ELong deriving (GHCG.Generic,Eq,Show)
instance Restricts ELongObject ELong where
    restricts (ELongObject x) = x
instance SchemaType ELongObject where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (ELongObject x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ELongObject where
    acceptingParser = fmap ELongObject acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (ELongObject x) = simpleTypeText x
 
newtype EMap = EMap Xsd.XsdString deriving (GHCG.Generic,Eq,Show)
instance Restricts EMap Xsd.XsdString where
    restricts (EMap x) = x
instance SchemaType EMap where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EMap x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EMap where
    acceptingParser = fmap EMap acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EMap x) = simpleTypeText x
 
newtype EResource = EResource Xsd.XsdString deriving (GHCG.Generic,Eq,Show)
instance Restricts EResource Xsd.XsdString where
    restricts (EResource x) = x
instance SchemaType EResource where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EResource x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EResource where
    acceptingParser = fmap EResource acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EResource x) = simpleTypeText x
 
newtype EResourceSet = EResourceSet Xsd.XsdString deriving (GHCG.Generic,Eq,Show)
instance Restricts EResourceSet Xsd.XsdString where
    restricts (EResourceSet x) = x
instance SchemaType EResourceSet where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EResourceSet x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EResourceSet where
    acceptingParser = fmap EResourceSet acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EResourceSet x) = simpleTypeText x
 
newtype EShort = EShort Xsd.Short deriving (GHCG.Generic,Eq,Show)
instance Restricts EShort Xsd.Short where
    restricts (EShort x) = x
instance SchemaType EShort where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EShort x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EShort where
    acceptingParser = fmap EShort acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EShort x) = simpleTypeText x
 
newtype EShortObject = EShortObject EShort deriving (GHCG.Generic,Eq,Show)
instance Restricts EShortObject EShort where
    restricts (EShortObject x) = x
instance SchemaType EShortObject where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EShortObject x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EShortObject where
    acceptingParser = fmap EShortObject acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EShortObject x) = simpleTypeText x
 
newtype EString = EString Xsd.XsdString deriving (GHCG.Generic,Eq,Show)
instance Restricts EString Xsd.XsdString where
    restricts (EString x) = x
instance SchemaType EString where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EString x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EString where
    acceptingParser = fmap EString acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EString x) = simpleTypeText x
 
data EStringToStringMapEntry = EStringToStringMapEntry
        { eStringToStringMapEntry_key :: Maybe EString
        , eStringToStringMapEntry_value :: Maybe EString
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EStringToStringMapEntry where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "key" e pos
        a1 <- optional $ getAttribute "value" e pos
        commit $ interior e $ return (EStringToStringMapEntry a0 a1)
    schemaTypeToXML s x@EStringToStringMapEntry{} =
        toXMLElement s [ maybe [] (toXMLAttribute "key") $ eStringToStringMapEntry_key x
                       , maybe [] (toXMLAttribute "value") $ eStringToStringMapEntry_value x
                       ]
            []
 
newtype ETreeIterator = ETreeIterator Xsd.XsdString deriving (GHCG.Generic,Eq,Show)
instance Restricts ETreeIterator Xsd.XsdString where
    restricts (ETreeIterator x) = x
instance SchemaType ETreeIterator where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (ETreeIterator x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ETreeIterator where
    acceptingParser = fmap ETreeIterator acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (ETreeIterator x) = simpleTypeText x
 
data EGenericType = EGenericType
        { eGenericType_eRawType :: Maybe Xsd.AnyURI
        , eGenericType_eTypeParameter :: Maybe Xsd.IDREF
        , eGenericType_eClassifier :: Maybe Xsd.AnyURI
        , eGenericType_eUpperBound :: Maybe EGenericType
        , eGenericType_eTypeArguments :: [EGenericType]
        , eGenericType_eLowerBound :: Maybe EGenericType
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType EGenericType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "eRawType" e pos
        a1 <- optional $ getAttribute "eTypeParameter" e pos
        a2 <- optional $ getAttribute "eClassifier" e pos
        commit $ interior e $ return (EGenericType a0 a1 a2)
            `apply` optional (parseSchemaType "eUpperBound")
            `apply` many (parseSchemaType "eTypeArguments")
            `apply` optional (parseSchemaType "eLowerBound")
    schemaTypeToXML s x@EGenericType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "eRawType") $ eGenericType_eRawType x
                       , maybe [] (toXMLAttribute "eTypeParameter") $ eGenericType_eTypeParameter x
                       , maybe [] (toXMLAttribute "eClassifier") $ eGenericType_eClassifier x
                       ]
            [ maybe [] (schemaTypeToXML "eUpperBound") $ eGenericType_eUpperBound x
            , concatMap (schemaTypeToXML "eTypeArguments") $ eGenericType_eTypeArguments x
            , maybe [] (schemaTypeToXML "eLowerBound") $ eGenericType_eLowerBound x
            ]
 
data ETypeParameter = ETypeParameter
        { eTypeParameter_name :: Maybe EString
        , eTypeParameter_eAnnotations :: [EAnnotation]
        , eTypeParameter_eBounds :: [EGenericType]
        }
        deriving (GHCG.Generic,Eq,Show)
instance SchemaType ETypeParameter where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "name" e pos
        commit $ interior e $ return (ETypeParameter a0)
            `apply` many (parseSchemaType "eAnnotations")
            `apply` many (parseSchemaType "eBounds")
    schemaTypeToXML s x@ETypeParameter{} =
        toXMLElement s [ maybe [] (toXMLAttribute "name") $ eTypeParameter_name x
                       ]
            [ concatMap (schemaTypeToXML "eAnnotations") $ eTypeParameter_eAnnotations x
            , concatMap (schemaTypeToXML "eBounds") $ eTypeParameter_eBounds x
            ]
instance Extension ETypeParameter ENamedElement where
    supertype v = ENamedElement_ETypeParameter v
instance Extension ETypeParameter EModelElement where
    supertype = (supertype :: ENamedElement -> EModelElement)
              . (supertype :: ETypeParameter -> ENamedElement)
              
 
newtype EInvocationTargetException = EInvocationTargetException Xsd.XsdString deriving (GHCG.Generic,Eq,Show)
instance Restricts EInvocationTargetException Xsd.XsdString where
    restricts (EInvocationTargetException x) = x
instance SchemaType EInvocationTargetException where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EInvocationTargetException x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EInvocationTargetException where
    acceptingParser = fmap EInvocationTargetException acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (EInvocationTargetException x) = simpleTypeText x
