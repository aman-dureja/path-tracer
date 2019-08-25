import qualified Vec3 as V
import qualified Ray as R
import qualified Sphere as S
import qualified HitableList as HL
import qualified Hitable as H 
import qualified Camera as C
import qualified Lambertian as L
import qualified Metal as Metal
import Control.Monad
import System.Random

color :: R.Ray -> HL.HitableList -> Int -> V.Vec3
color r world depth =
    case HL.hit world r 0.001 ((fromIntegral (maxBound :: Int)) :: Float) of
        Nothing -> V.add (V.scalarMul (V.Vec3(1.0,1.0,1.0)) (1.0-t)) (V.scalarMul (V.Vec3(0.5,0.7,1.0)) t)
        Just record ->
            if depth < 50 then
                case (H.scatter $ H.material record) r record of
                    Nothing -> V.Vec3(0,0,0) 
                    Just (attenuation, scattered) -> V.vecMul (color scattered world (depth+1)) attenuation
            else V.Vec3(0,0,0)
    where t = 0.5 * (1.0 + (V.y $ V.unitVector $ R.direction r))

rgbRows :: Int -> Int -> Int -> Int -> Int -> HL.HitableList -> C.Camera -> [Float] -> String
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

randomRaysColor :: Int -> Int -> Int -> Int -> Int -> Int -> C.Camera -> HL.HitableList -> V.Vec3 -> [Float] -> (V.Vec3, [Float])
randomRaysColor i j s ns nx ny cam world col randomNums =
    if s >= ns then (col, randomNums)
    else 
        let u = (fromIntegral i + head randomNums) / (fromIntegral nx)
            v = (fromIntegral j + (head . tail) randomNums) / (fromIntegral ny) 
            r = C.getRay cam u v
            randomColor = color r world 0
        in randomRaysColor i j (s+1) ns nx ny cam world (V.add col randomColor) ((tail . tail) randomNums)

main :: IO ()
main = do
    let nx = 200
        ny = 100
        ns = 100
        camera = C.Camera {C.lowerLeftCorner=V.Vec3(-2,-1,-1), C.horizontal=V.Vec3(4,0,0), C.vertical=V.Vec3(0,2,0), C.origin=V.Vec3(0,0,0)}
        world = HL.hitableList [ S.sphere (V.Vec3(0,0,-1)) 0.5 (L.lambertian (V.Vec3(0.8,0.3,0.3))) 
                               , S.sphere (V.Vec3(0,-100.5,-1)) 100 (L.lambertian (V.Vec3(0.8,0.8,0.0)))
                               , S.sphere (V.Vec3(1,0,-1)) 0.5 (Metal.metal (V.Vec3(0.8,0.6,0.2)) 0.3)
                               , S.sphere (V.Vec3(-1,0,-1)) 0.5 (Metal.metal (V.Vec3(0.8,0.8,0.8)) 1.0) ]
        randomNums = randoms $ mkStdGen 42 :: [Float]
    putStrLn $ "P3\n" ++ show nx ++ " " ++ show ny ++ "\n255"
    putStr $ rgbRows (ny - 1) 0 nx ny ns world camera randomNums 

