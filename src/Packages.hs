module Packages

where

import Asts
import qualified Ast

data Packages
   = Packages
     {
         explicitPackages :: Map Token.PackageName Package
         implicitPackages :: Map FilePath Package
     }
     deriving ( Show )

data Package
   = Package
     {
         classes :: ActualType.Classes,
         functions :: ActualType.Functions,
         variables :: ActualType.Variables
     }
     deriving ( Show )

-- | API for creating packages
packager :: Asts -> Packages
packager asts = packager' (decs asts)

packager' :: [ Ast.Root ] -> Packages
packager' [] = emptyCollectionOfPackages
packager' (ast:asts) = addPackage (packager'' ast) (packager' asts)

packager'' :: Ast.Root -> Packages
packager'' ast = let astDefinedPackages = filterAstDecPackages (decs ast) in
    case astDefinedPackages of
        Nothing -> fromSingleImplicitPackage (filename ast)
        Just astDefinedPackage -> fromSingleExplicitPackage astDefinedPackages

-- if more than one package was declared, return the first declaration
filterAstDecPackages :: [ Ast.Dec ] -> Maybe Ast.DecPackage
filterAstDecPackages [] = Nothing
filterAstDecPackages ((Ast.DecPackageCtor astDecPackage):decs) = astDecPackage
filterAstDecPackages (dec:decs) = filterAstDecPackages decs

fromSingleImplicitPackage :: String -> Packages
fromSingleImplicitPackage filename = Packages {
    explicitPackages = emptyCollectionOfExplicitPackages
    implicitPackages = fromList [ (filename, p) ]
}

