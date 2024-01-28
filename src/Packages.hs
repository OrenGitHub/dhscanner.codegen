module Packages

where

import Asts
import qualified Ast

data Packages
   = Packages
     {
         actualPackages :: Map Ast.Root Package
     }
     deriving ( Show )

data Package
   = PackageExplictlyDefined String PackageContent
   | PackageImplictlyDefinedFromFilename PackageContent
   deriving ( Show )

data PackageContent
   = PackageContent
     {
         ast :: Set Ast.Root,
         classes :: ActualType.Classes,
         functions :: ActualType.Functions,
         variables :: ActualType.Variables
     }
     deriving ( Show )

emptyPackageContent = PackageContent {
    classes = ActualType.emptyClasses,
    functions = ActualType.emptyFunctions,
    variables = ActualType.emptyVariables
}

packager :: Asts -> Packages
packager asts = let
    packages = getExplictlyDefinedPackages asts
    asts' = append1stAstDecPackage asts
    in
    zipThemTogether packages asts'

-- | Create an empty package
-- * either explictly defined in the Ast
--
-- * or implictly from the corresponding filename
--
packager'' :: Ast.Root -> Package
packager'' ast = let firstAstDecPackage = firstAstDecPackage (actualAst ast) in
    case firstAstDecPackage of
        Nothing -> implicitPackageFromFilename (filename ast)
        Just astDefinedPackage -> explicitPackage astDefinedPackage

firstAstDecPackage :: [ Ast.Dec ] -> Maybe Ast.DecPackage
firstAstDecPackage [] = Nothing
firstAstDecPackage ((Ast.DecPackageCtor astDecPackage):decs) = astDecPackage
firstAstDecPackage (dec:decs) = firstAstDecPackage decs

append1stAstDecPackage' :: Ast.Root -> (Ast.Root, Maybe Ast.DecPackage)
append1stAstDecPackage' ast = (ast, firstAstDecPackage (actualAst ast))

append1stAstDecPackage :: [ Ast.Root ] -> [(Ast.Root, Maybe Ast.DecPackage)]
append1stAstDecPackage asts = map append1stAstDecPackage' asts

mkEmptyExplicitPackage :: String -> Package
mkEmptyExplicitPackage s = PackageExplictlyDefined s emptyPackageContent

getExplictlyDefinedPackages :: Asts -> Map String Package
getExplictlyDefinedPackages asts = let
    asts' = astsContent asts
    astsDecPackages = catMaybes $ map firsAstDecPackageIfAny asts'
    astsDecPackagesAsStrings = map astDecPackageToString astDecPackages
    distinct = toList $ fromList astsDecPackagesAsStrings
    in
    Map.fromList $ map mkEmptyExplicitPackage distinct

implicitPackageFromFilename :: String -> Package
implicitPackageFromFilename filename = Package $
    PackageImplicitlyDefinedFromFilename $
        PackageContent {
            classes = emptyCollectionOfActualTypeClasses,
            functions = emptyCollectionOfActualTypeFunctions,
            variables = emptyCollectionOfActualTypeVariables
        }

