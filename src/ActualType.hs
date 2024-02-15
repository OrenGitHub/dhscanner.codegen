{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ActualType

where

-- project imports
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
   = Class ClassContent
   | Method MethodContent
   | Package PackageContent
   | Function FunctionContent
   | NativeType NativeTypeContent
   | ThirdPartyImport ThirdPartyImportContent
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Classes
   = Classes
     {
         actualClasses :: Map String ClassContent
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- addMethod :: Ast.DecMethodContent -> Ast.DecClassContent -> Classes -> Classes
-- addMethod m c classes = Classes { actualClasses = actualClasses' }
--    where
--        actualClasses' = alter c classes 

data Super
   = Super
     {
         actualSuper :: ClassContent
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Supers
   = Supers
     {
         actualSupers :: Set Super
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data DataMember
   = DataMember
     {
         dataMemberName :: Token.MembrName,
         dataMemberType :: ActualType
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data DataMembers
   = DataMembers
     {
         actualDataMembers :: Set DataMember
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ClassContent
   = ClassContent
     {
         className :: Token.ClassName,
         supers :: Supers,
         methods :: Methods,
         dataMembers :: DataMembers
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Methods
   = Methods
     {
         actualMethods :: Map String MethodContent
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Params
   = Params
     {
         actualParams :: [ Param ]
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Param
   = Param
     {
         paramName :: Token.ParamName,
         paramSerialIdx :: Int,
         paramActualTypeHashed :: String
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

nativeTypeInt = NativeType NativeTypeInt

data NativeTypeContent
   = NativeTypeInt
   | NativeTypeStr
   | NativeTypeAny
   | NativeTypeBool
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data FunctionContent
   = FunctionContent
     {
         funcName :: Token.FuncName,
         params :: Params
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data MethodContent
   = MethodContent
     {
         methodName :: Token.MethdName,
         methodParams :: Params
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ThirdPartyImportContent
   = ThirdPartyImportContent
     {
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data PackageContent
   = PackageContent
     {
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )
