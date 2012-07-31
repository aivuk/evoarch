import qualified Data.Vector.Unboxed as UV

type Genotype  = UV.Vector Int
type Trait     = Int
type TraitLoci = UV.Vector Int
type TraitMap  = TraitLoci -> Genotype -> Trait
type PhenMap   = Genotype -> Phenotype
type Phenotype = UV.Vector Trait

data Population = 
    P { gens  :: [Genotype]
      , phens :: [Phenotype] }

createPop :: [Genotype] -> PhenMap -> Population
createPop g pm = P g (map pm g) 

mkPhenMap :: [TraitMap] -> [TraitLoci] -> PhenMap
mkPhenMap ts ti = UV.fromList.(zipWith ($) (zipWith ($) ts ti)).repeat
