{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ActualType

where

-- project imports
import qualified Fqn

-- project (qualified) imports
import Location
import qualified Ast
import qualified Token

-- general imports
import Data.Map ( Map )

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
   | ClassName ClassNameContent
   | ClassInstance ClassInstanceContent
   | Lambda LambdaContent
   | Method MethodContent
   | CallMethodOfClass Location String Token.ClassName
   | Function FunctionContent
   | FirstPartyImport FirstPartyImportContent
   | ThirdPartyImport ThirdPartyImportContent
   | FieldedAccess ActualType Token.FieldName
   deriving ( Show, Eq, Ord, Generic )

newtype Classes = Classes { actualClasses :: Map String ClassNameContent } deriving ( Show )

newtype ClassNameContent
   = ClassNameContent
     {
         className :: Token.ClassName
     }
     deriving ( Show, Eq, Ord, Generic )

data InstanceName = This | Self deriving ( Show, Eq, Ord, Generic )

data ClassInstanceContent
   = ClassInstanceContent
     {
         instanceName :: InstanceName,
         instanceOfClass :: Token.ClassName
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

inferFromBinop :: ActualType -> ActualType -> Ast.Operator -> ActualType
inferFromBinop NativeTypeStr NativeTypeStr Ast.PLUS = NativeTypeStr
inferFromBinop NativeTypeInt NativeTypeInt Ast.PLUS = NativeTypeInt
inferFromBinop _ _ _ = Any

toFqn :: ActualType -> Fqn.Fqn
toFqn (ThirdPartyImport (ThirdPartyImportContent name)) = Fqn.Imported (Fqn.ImportedThirdParty (Fqn.ImportedThirdPartyContent name []))
toFqn NativeTypeStr = Fqn.NativeTypeString
toFqn (ClassName (ClassNameContent c)) = Fqn.ClassName (Fqn.ClassNameContent c)
toFqn (FieldedAccess t field) = Fqn.FieldedAccess (toFqn t) field
toFqn (ClassInstance (ClassInstanceContent This name)) = Fqn.ClassInstance (Fqn.ClassInstanceContent Fqn.This name)
toFqn (ClassInstance (ClassInstanceContent Self name)) = Fqn.ClassInstance (Fqn.ClassInstanceContent Fqn.Self name)
toFqn (CallMethodOfClass call method c) = Fqn.CallMethodOfClass call method c
toFqn _ = Fqn.Unknwon
