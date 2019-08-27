module Camera where
import qualified Vec3 as V
import qualified Ray as R
import System.Random

data Camera = Camera { lowerLeftCorner :: V.Vec3
                     , horizontal :: V.Vec3
                     , vertical :: V.Vec3
                     , origin :: V.Vec3
                     , u :: V.Vec3
                     , v :: V.Vec3
                     , w :: V.Vec3
                     , lensRadius :: Float }

getRay :: Camera -> Float -> Float -> R.Ray
getRay c s t =
    let randomNums = randoms $ mkStdGen 42 :: [Float]
        (randomVec, _) = randomInUnitDisk Nothing randomNums
        rd = V.scalarMul randomVec (lensRadius c)
        offset = V.add (V.scalarMul (u c) (V.x rd)) (V.scalarMul (v c) (V.y rd))
    in R.Ray (V.add offset (origin c), foldl V.add (V.Vec3(0,0,0)) [lowerLeftCorner c, V.scalarMul (horizontal c) s, V.scalarMul (vertical c) t, V.scalarMul (origin c) (-1), V.scalarMul offset (-1)])

randomInUnitDisk :: Maybe V.Vec3 -> [Float] -> (V.Vec3, [Float])
randomInUnitDisk maybeVec randomNums =
    let randX = head randomNums
        randY = (head . tail) randomNums
        p = V.sub (V.scalarMul (V.Vec3(randX, randY, 0)) 2) (V.Vec3(1,1,0))
    in case maybeVec of Nothing -> randomInUnitDisk (Just p) ((tail . tail) randomNums)
                        Just vec -> if V.dot p p < 1 then (p, (tail . tail) randomNums)
                                    else randomInUnitDisk (Just p) ((tail . tail) randomNums)

camera :: V.Vec3 -> V.Vec3 -> V.Vec3 -> Float -> Float -> Float -> Float -> Camera
camera lookfrom lookat vup vfov aspect aperture focusDist =
    let theta = vfov * pi / 180
        halfHeight = tan (theta / 2)
        halfWidth = aspect * halfHeight
        lensRadius = aperture / 2
        w = V.unitVector (V.sub lookfrom lookat)
        u = V.unitVector (V.cross vup w)
        v = V.cross w u
        llc = V.sub (V.sub lookfrom (V.scalarMul u (halfWidth*focusDist))) (V.add (V.scalarMul v (halfHeight*focusDist)) (V.scalarMul w focusDist))
    in Camera { lowerLeftCorner = llc 
              , horizontal = V.scalarMul u (2 * halfWidth * focusDist)
              , vertical = V.scalarMul v (2 * halfHeight * focusDist)
              , origin = lookfrom
              , Camera.u = u
              , Camera.v = v
              , Camera.w = w
              , Camera.lensRadius = lensRadius }

