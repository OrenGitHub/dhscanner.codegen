{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ActualType

where

-- project imports
import Fqn
import Location
import qualified Ast
import qualified Token

-- general imports
import Data.Map ( Map )
import Data.Set ( Set )

-- general (qualified) imports
import Data.Aeson
import GHC.Generics
import qualified Data.Map

data ActualType
   = Any
   | Require -- ^ for javascript
   | NativeTypeInt
   | NativeTypeStr
   | NativeTypeBool
   | NativeTypeConstInt Int
   | NativeTypeConstStr String
   | NativeTypeConstBool
   | Class ClassContent
   | Lambda LambdaContent
   | Method MethodContent
   | Package PackageContent
   | Function FunctionContent
   | ThirdPartyImport ThirdPartyImportContent
   deriving ( Show, Eq, Ord, Generic )

data Classes = Classes { actualClasses :: Map String ClassContent } deriving ( Show )

data Functions
   = Functions
     {
         actualFunctions :: Map String ClassContent
     }
     deriving ( Show, Eq, Ord )

-- addMethod :: Ast.DecMethodContent -> Ast.DecClassContent -> Classes -> Classes
-- addMethod m c classes = Classes { actualClasses = actualClasses' }
--    where
--        actualClasses' = alter c classes 

data Super
   = Super
     {
         actualSuper :: ClassContent
     }
     deriving ( Show, Eq, Ord, Generic )

data Supers
   = Supers
     {
         actualSupers :: Set Super
     }
     deriving ( Show, Eq, Ord, Generic )

data DataMember
   = DataMember
     {
         dataMemberName :: Token.MembrName,
         dataMemberType :: ActualType
     }
     deriving ( Show, Eq, Ord, Generic )

data DataMembers
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

data Methods
   = Methods
     {
         actualMethods :: Map String MethodContent
     }
     deriving ( Show, Eq, Ord, Generic )

data Params
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
         methodName :: Token.MethdName,
         methodParams :: Params
     }
     deriving ( Show, Eq, Ord, Generic )

data LambdaContent
   = LambdaContent
     {
         lambdaLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic )


data ThirdPartyImportContent
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
    content = ThirdPartyImportContent (t' ++ "." ++ f')
    in ThirdPartyImport content

toFqn :: ActualType -> Fqn
toFqn (ThirdPartyImport (ThirdPartyImportContent name)) = Fqn name
toFqn (NativeTypeStr) = Fqn.nativeStr
toFqn (Class c) = Fqn (Token.content (Token.getClassNameToken (className c)))
toFqn _ = Fqn.nativeInt
