import qualified Vec3 as V
import qualified Ray as R
import qualified Sphere as S
import qualified HitableList as HL
import qualified Hitable as H 
import qualified Camera as C
import Control.Monad
import System.Random
import Data.Time.Clock.POSIX (getPOSIXTime)

color :: H.Hitable a => R.Ray -> a -> V.Vec3
color r world = 
    case H.hit world r 0.0 ((fromIntegral (maxBound :: Int)) :: Float) of
        Just rec -> V.scalarMul (V.Vec3(1 + (V.x $ H.normal rec), 1 + (V.y $ H.normal rec), 1 + (V.z $ H.normal rec))) 0.5
        Nothing -> V.add (V.scalarMul (V.Vec3(1.0,1.0,1.0)) (1.0-t)) (V.scalarMul (V.Vec3(0.5,0.7,1.0)) t) 
    where t = 0.5 * (1.0 + (V.y $ V.unitVector $ R.direction r))

rgbRows :: H.Hitable a => Int -> Int -> Int -> Int -> Int -> a -> C.Camera -> Int -> Int -> String
rgbRows j i nx ny ns world cam uSeed vSeed
    | j < 0 = ""
    | otherwise =
        if i >= nx then rgbRows (j-1) 0 nx ny ns world cam (10+uSeed) (10+vSeed)
        else
            let col = V.scalarDiv (randomRaysColor i j 0 ns nx ny cam world (V.Vec3(0,0,0)) uSeed vSeed) (fromIntegral ns)
                ir = floor $ 255.99 * (V.r col)
                ig = floor $ 255.99 * (V.g col)
                ib = floor $ 255.99 * (V.b col)
            in show ir ++ " " ++ show ig ++ " " ++ show ib ++ "\n" ++ rgbRows j (i+1) nx ny ns world cam (20+uSeed) (20+vSeed)

randomRaysColor :: H.Hitable a => Int -> Int -> Int -> Int -> Int -> Int -> C.Camera -> a -> V.Vec3 -> Int -> Int -> V.Vec3
randomRaysColor i j s ns nx ny cam world col uSeed vSeed =
    if s >= ns then col
    else 
        let u = (fromIntegral i + (head $ (take 1 $ (randoms $ mkStdGen uSeed) :: [Float]))) / (fromIntegral nx)
            v = (fromIntegral j + (head $ (take 1 $ (randoms $ mkStdGen vSeed) :: [Float]))) / (fromIntegral ny) 
            r = C.getRay cam u v
        in randomRaysColor i j (s+1) ns nx ny cam world (V.add col (color r world)) (1+uSeed) (1+vSeed)

main :: IO ()
main = do
    uSeed <- (round . (*1000)) <$> getPOSIXTime 
    vSeed <- (round . (*1000)) <$> getPOSIXTime 
    let nx = 200
        ny = 100
        ns = 100
        camera = C.Camera {C.lowerLeftCorner=V.Vec3(-2,-1,-1), C.horizontal=V.Vec3(4,0,0), C.vertical=V.Vec3(0,2,0), C.origin=V.Vec3(0,0,0)}
        world = [S.Sphere (V.Vec3(0,0,-1)) 0.5, S.Sphere (V.Vec3(0,-100.5,-1)) 100]
    putStrLn $ "P3\n" ++ show nx ++ " " ++ show ny ++ "\n255"
    putStr $ rgbRows (ny - 1) 0 nx ny ns world camera uSeed vSeed

