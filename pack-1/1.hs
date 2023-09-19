solveQuadratic :: Float -> Float -> Float -> (Float, Float)

solveQuadratic a b c = do
    let d = b * b - 4 * a * c
    if (d < 0) then error"negative d" else ((-(b) - sqrt(d))/(2 * a), (-(b) + sqrt(d))/(2 * a))
