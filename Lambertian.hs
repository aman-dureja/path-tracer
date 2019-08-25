module Lambertian where
import qualified Vec3 as V
import qualified Ray as R
import qualified Hitable as H
import Sphere (randomInUnitSphere)
import System.Random

lambertian :: V.Vec3 -> H.Material
lambertian albedo = H.Material { H.scatter=scatter' }
    where
        scatter' r record =
            let randomNums = randoms $ mkStdGen 42 :: [Float]
                (randomVec, _) = randomInUnitSphere Nothing randomNums
                target = V.add (V.add (H.p record) (H.normal record)) randomVec 
                scattered = R.Ray ((H.p record), V.sub target (H.p record))
            in Just (albedo, scattered)

