import qualified Vec3 as V

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
                let col = V.Vec3 (i/nx, j/ny, 0.2)
                    ir = floor $ 255.99 * (V.r col)
                    ig = floor $ 255.99 * (V.g col)
                    ib = floor $ 255.99 * (V.b col)
                in show ir ++ " " ++ show ig ++ " " ++ show ib ++ "\n" ++ rgbRows j (i + 1) nx ny
        | otherwise = ""

