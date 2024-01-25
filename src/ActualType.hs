module ActualType

where

data Classes
   = Classes
     {
         actualClasses :: Map String Class
     }

data ActualType
   = ActualTypeClassCtor Class
   | ActualTypeMethod ActualTypeMethodContent
   | ActualTypePackage ActualTypePackageContent
   | ActualTypeFunction ActualTypeFunctionContent
   | ActualTypeNativeType ActualTypeNativeTypeContent
   | ActualTypeThirdPartyImport ActualTypeThirdPartyImportContent
   deriving ( Show )

addMethod :: Ast.Method -> Ast.Class -> Classes -> Classes
addMethod m c classes = Classes { actualClasses = actualClasses' }
    where
        actualClasses' = alter c classes 

data Class
   = Class
     {
         className :: Token.ClassName,
         supers :: Supers,
         methods :: Methods,
         dataMembers :: DataMembers
     }
     deriving ( Show )

data Methods
   = Methods
     {
         methods :: Map String Method
     }

data Params
   = Params
     {
         actualParams :: [ Param ]
     }

data Param
   = Param
     {
         paramName :: Token.ParamName
         paramAsKeyword :: Maybe Token.KeywordName
         paramSerialIdx :: Int
         paramActualTypeHashed :: String
     }

data Function
   = Function
     {
         funcName :: Token.FuncName,
         params :: Params
     }
     deriving ( Show )

data Method
   = Method
     {
         methodName :: Token.MethdName,
         methodParams :: Params
     }
     deriving ( Show )

