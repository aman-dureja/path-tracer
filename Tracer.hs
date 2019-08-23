import qualified Vec3 as V
import qualified Ray as R
import qualified Sphere as S
import qualified HitableList as HL
import qualified Hitable as H 
import qualified Camera as C
import Control.Monad
import System.Random
import Data.Time.Clock.POSIX (getPOSIXTime)

color :: H.Hitable a => R.Ray -> a -> [Float] -> (V.Vec3, [Float])
color r world randomNums =
    let (randSphereVec, newRandomNums) = randomInUnitSphere Nothing randomNums 
    in case H.hit world r 0.001 ((fromIntegral (maxBound :: Int)) :: Float) of
        Nothing -> (V.add (V.scalarMul (V.Vec3(1.0,1.0,1.0)) (1.0-t)) (V.scalarMul (V.Vec3(0.5,0.7,1.0)) t), newRandomNums) 
        Just record -> 
            let target = foldl V.add (V.Vec3(0,0,0)) [H.p record, H.normal record, randSphereVec]
                (col, newRandomNums1) = color (R.Ray ((H.p record), (V.sub target (H.p record)))) world newRandomNums 
            in (V.scalarMul col 0.5, newRandomNums1) 
    where t = 0.5 * (1.0 + (V.y $ V.unitVector $ R.direction r))

randomInUnitSphere :: Maybe V.Vec3 -> [Float] -> (V.Vec3, [Float]) 
randomInUnitSphere maybeVec randomNums = 
    let randX = head randomNums 
        randY = (head . tail) randomNums 
        randZ = (head . tail . tail) randomNums 
        newVec = Just (V.Vec3(randX, randY, randZ))
    in case maybeVec of
        Nothing -> randomInUnitSphere newVec ((tail . tail . tail) randomNums) 
        Just vec -> if V.squaredLength vec < 1 then (vec, (tail . tail . tail) randomNums) 
                    else randomInUnitSphere newVec ((tail . tail . tail) randomNums) 

rgbRows :: H.Hitable a => Int -> Int -> Int -> Int -> Int -> a -> C.Camera -> [Float] -> String
rgbRows j i nx ny ns world cam randomNums 
    | j < 0 = ""
    | otherwise =
        if i >= nx then rgbRows (j-1) 0 nx ny ns world cam randomNums 
        else
            let (randCol, newRandomNums) = randomRaysColor i j 0 ns nx ny cam world (V.Vec3(0,0,0)) randomNums
                col = V.transform sqrt (V.scalarDiv randCol (fromIntegral ns))
                ir = floor $ 255.99 * (V.r col)
                ig = floor $ 255.99 * (V.g col)
                ib = floor $ 255.99 * (V.b col)
            in show ir ++ " " ++ show ig ++ " " ++ show ib ++ "\n" ++ rgbRows j (i+1) nx ny ns world cam newRandomNums 

randomRaysColor :: H.Hitable a => Int -> Int -> Int -> Int -> Int -> Int -> C.Camera -> a -> V.Vec3 -> [Float] -> (V.Vec3, [Float])
randomRaysColor i j s ns nx ny cam world col randomNums =
    if s >= ns then (col, randomNums)
    else 
        let u = (fromIntegral i + head randomNums) / (fromIntegral nx)
            v = (fromIntegral j + (head . tail) randomNums) / (fromIntegral ny) 
            r = C.getRay cam u v
            (randomColor, newRandomNums) = color r world ((tail . tail) randomNums)
        in randomRaysColor i j (s+1) ns nx ny cam world (V.add col randomColor) newRandomNums

main :: IO ()
main = do
    let nx = 200
        ny = 100
        ns = 100
        camera = C.Camera {C.lowerLeftCorner=V.Vec3(-2,-1,-1), C.horizontal=V.Vec3(4,0,0), C.vertical=V.Vec3(0,2,0), C.origin=V.Vec3(0,0,0)}
        world = [S.Sphere (V.Vec3(0,0,-1)) 0.5, S.Sphere (V.Vec3(0,-100.5,-1)) 100]
        randomNums = randoms $ mkStdGen 42 :: [Float]
    putStrLn $ "P3\n" ++ show nx ++ " " ++ show ny ++ "\n255"
    putStr $ rgbRows (ny - 1) 0 nx ny ns world camera randomNums 

