{- A better version of:

    data Point = Pt Float Float
    pointx :: Point -> Float
    pointx (Pt x _) = x
    pointy :: Point -> Float
    pointy (Pt _ y) = y

    is...-}

data Point = Pt { pointx, pointy :: Float}

--can construct as: Pt {pointx = 1, pointy = 2}
--which is equivalent to: Pt 1 2

absPoint :: Point -> Float
absPoint (Pt {pointx = x, pointy = y}) = sqrt (x*x + y*y)