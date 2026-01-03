{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ActualType

where

-- project imports
import Fqn

-- project (qualified) imports
import Location
import qualified Token

-- general imports
import Data.Map ( Map )
import Data.Set ( Set )

-- general (qualified) imports
import GHC.Generics

data ActualType
   = Any
   | NativeTypeInt
   | NativeTypeStr
   | NativeTypeBool
   | NativeTypeConstInt Int
   | NativeTypeConstStr String
   | NativeTypeConstBool Bool
   | NativeTypeConstNull
   | Class ClassContent
   | Lambda LambdaContent
   | Method MethodContent
   | Package PackageContent
   | Function FunctionContent
   | FirstPartyImport FirstPartyImportContent
   | ThirdPartyImport ThirdPartyImportContent
   deriving ( Show, Eq, Ord, Generic )

newtype Classes = Classes { actualClasses :: Map String ClassContent } deriving ( Show )

newtype Functions
   = Functions
     {
         actualFunctions :: Map String ClassContent
     }
     deriving ( Show, Eq, Ord )

-- addMethod :: Ast.DecMethodContent -> Ast.DecClassContent -> Classes -> Classes
-- addMethod m c classes = Classes { actualClasses = actualClasses' }
--    where
--        actualClasses' = alter c classes 

newtype Super
   = Super
     {
         actualSuper :: ClassContent
     }
     deriving ( Show, Eq, Ord, Generic )

newtype Supers
   = Supers
     {
         actualSupers :: Set Super
     }
     deriving ( Show, Eq, Ord, Generic )

data DataMember
   = DataMember
     {
         dataMemberName :: Token.MemberName,
         dataMemberType :: ActualType
     }
     deriving ( Show, Eq, Ord, Generic )

newtype DataMembers
   = DataMembers
     {
         actualDataMembers :: Set DataMember
     }
     deriving ( Show, Eq, Ord, Generic )

data ClassContent
   = ClassContent
     {
         className :: Token.ClassName,
         supers :: Supers,
         methods :: Methods,
         dataMembers :: DataMembers
     }
     deriving ( Show, Eq, Ord, Generic )

newtype Methods
   = Methods
     {
         actualMethods :: Map String MethodContent
     }
     deriving ( Show, Eq, Ord, Generic )

newtype Params
   = Params
     {
         actualParams :: [ Param ]
     }
     deriving ( Show, Eq, Ord, Generic )

data Param
   = Param
     {
         paramName :: Token.ParamName,
         paramSerialIdx :: Int,
         paramActualTypeHashed :: String
     }
     deriving ( Show, Eq, Ord, Generic )

data FunctionContent
   = FunctionContent
     {
         funcName :: Token.FuncName,
         params :: Params,
         returnType :: ActualType
     }
     deriving ( Show, Eq, Ord )

data MethodContent
   = MethodContent
     {
         methodName :: Token.MethodName,
         methodParams :: Params
     }
     deriving ( Show, Eq, Ord, Generic )

newtype LambdaContent
   = LambdaContent
     {
         lambdaLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic )

data FirstPartyImportContent
   = FirstPartyImportContent
     {
         firstPartyImportedContent :: FilePath,
         firstPartyImportedSpecific :: Maybe String
     }
     deriving ( Show, Eq, Ord, Generic )


newtype ThirdPartyImportContent
   = ThirdPartyImportContent
     {
         thirdPartyImportedName :: String
     }
     deriving ( Show, Eq, Ord, Generic )

data PackageContent
   = PackageContent
     {
     }
     deriving ( Show, Eq, Ord, Generic )

getFieldedAccess :: ActualType -> Token.FieldName -> ActualType
getFieldedAccess t f = let
    f' = Token.content (Token.getFieldNameToken f)
    t' = Fqn.content (toFqn t)
    thirdPartyImportContent = ThirdPartyImportContent (t' ++ "." ++ f')
    in ThirdPartyImport thirdPartyImportContent

toFqn :: ActualType -> Fqn
toFqn (ThirdPartyImport (ThirdPartyImportContent name)) = Fqn name
toFqn (NativeTypeStr) = Fqn.nativeStr
toFqn (Class c) = Fqn (Token.content (Token.getClassNameToken (className c)))
toFqn _ = Fqn.nativeInt
