import qualified Vec3 as V
import qualified Ray as R
import qualified Sphere as S
import qualified HitableList as HL
import qualified Hitable as H 

color :: H.Hitable a => R.Ray -> a -> V.Vec3
color r world = 
    case H.hit world r 0.0 ((fromIntegral (maxBound :: Int)) :: Float) of
        Just rec -> V.scalarMul (V.Vec3(1 + (V.x $ H.normal rec), 1 + (V.y $ H.normal rec), 1 + (V.z $ H.normal rec))) 0.5
        Nothing -> V.add (V.scalarMul (V.Vec3(1.0,1.0,1.0)) (1.0-t)) (V.scalarMul (V.Vec3(0.5,0.7,1.0)) t) 
    where t = 0.5 * (1.0 + (V.y $ V.unitVector $ R.direction r))

main :: IO ()
main = do
    let nx = 200
        ny = 100
        world = [S.Sphere (V.Vec3(0,0,-1)) 0.5, S.Sphere (V.Vec3(0,-100.5,-1)) 100]
    putStrLn $ "P3\n" ++ show (round nx) ++ " " ++ show (round ny) ++ "\n255"
    putStr $ rgbRows (ny - 1) 0 nx ny world
    where
    rgbRows j i nx ny wrld
        | j >= 0 =
            if i >= nx
                then rgbRows (j - 1) 0 nx ny wrld
            else 
                let u = i / nx
                    v = j / ny
                    lowerLeftCorner = V.Vec3 (-2.0, -1.0, -1.0)
                    horizontal = V.Vec3 (4.0, 0.0, 0.0)
                    vertical = V.Vec3 (0.0, 2.0, 0.0)
                    origin = V.Vec3 (0.0, 0.0, 0.0)
                    r = R.Ray (origin, foldl V.add (V.Vec3 (0,0,0)) [lowerLeftCorner, (V.scalarMul horizontal u), (V.scalarMul vertical v)]) 
                    col = color r wrld
                    ir = floor $ 255.99 * (V.r col)
                    ig = floor $ 255.99 * (V.g col)
                    ib = floor $ 255.99 * (V.b col)
                in show ir ++ " " ++ show ig ++ " " ++ show ib ++ "\n" ++ rgbRows j (i + 1) nx ny wrld
        | otherwise = ""

