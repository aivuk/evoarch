import qualified Data.Vector.Unboxed as UV
import qualified System.Random.MWC as R
import Control.Monad.Primitive (PrimMonad,PrimState)

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

randomGenotype :: PrimMonad m => R.Gen (PrimState m) -> Int -> m Genotype
randomGenotype g size = UV.replicateM size $ R.uniformR (-1,1) g

sumTrait :: TraitLoci -> Genotype -> Trait
sumTrait idx g = UV.sum $ UV.map (g UV.!) idx
