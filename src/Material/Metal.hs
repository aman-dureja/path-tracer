module Material.Metal where
import qualified Vec3 as V
import qualified Ray as R
import qualified Hitable as H
import Sphere (randomInUnitSphere)
import System.Random

reflect :: V.Vec3 -> V.Vec3 -> V.Vec3
reflect v n = V.sub v (V.scalarMul n (2 * (V.dot v n)))

metal :: V.Vec3 -> Float -> H.Material
metal albedo fuzz = H.Material { H.scatter=scatter' }
    where
        scatter' r record =
            let reflected = reflect (V.unitVector (R.direction r)) (H.normal record)
                randomNums = randoms $ mkStdGen 42 :: [Float]
                (randomVec, _) = randomInUnitSphere Nothing randomNums
                scattered = R.Ray (H.p record, V.add reflected (V.scalarMul randomVec fuzz))
            in if V.dot (R.direction scattered) (H.normal record) <= 0 then Nothing
               else Just (albedo, scattered)

