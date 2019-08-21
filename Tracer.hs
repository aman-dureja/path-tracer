import qualified Vec3 as V
import qualified Ray as R

main :: IO ()
main = do
    let nx = 200
        ny = 100
    putStrLn $ "P3\n" ++ show (round nx) ++ " " ++ show (round ny) ++ "\n255"
    putStr $ rgbRows (ny - 1) 0 nx ny
    where
    rgbRows j i nx ny
        | j >= 0 =
            if i >= nx
                then rgbRows (j - 1) 0 nx ny
            else 
                let u = i / nx
                    v = j / ny
                    lowerLeftCorner = V.Vec3 (-2.0, -1.0, -1.0)
                    horizontal = V.Vec3 (4.0, 0.0, 0.0)
                    vertical = V.Vec3 (0.0, 2.0, 0.0)
                    origin = V.Vec3 (0.0, 0.0, 0.0)
                    r = R.Ray (origin, foldl V.add (V.Vec3 (0,0,0)) [lowerLeftCorner, (V.scalarMul horizontal u), (V.scalarMul vertical v)]) 
                    col = R.color r 
                    ir = floor $ 255.99 * (V.r col)
                    ig = floor $ 255.99 * (V.g col)
                    ib = floor $ 255.99 * (V.b col)
                in show ir ++ " " ++ show ig ++ " " ++ show ib ++ "\n" ++ rgbRows j (i + 1) nx ny
        | otherwise = ""

