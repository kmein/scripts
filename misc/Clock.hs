module Clock where

newtype Angle = Angle
  { degrees :: Double
  }

instance Show Angle where
  show alpha = show (degrees alpha) ++ "°"

data Clock = Clock
  { hourHand :: Angle
  , minuteHand :: Angle
  }

instance Show Clock where
  show (Clock h m) = show (h, m)

data Time = Time
  { hour :: Int
  , minute :: Int
  }

time :: Int -> Int -> Time
time h m = Time (h `mod` 12) (m `mod` 60)

instance Show Time where
  show (Time h m) = show h ++ ":" ++ (if m < 10 then "0" else "") ++ show m

toClock :: Time -> Clock
toClock (Time h m) = Clock (Angle $ 30 * h' + m' / 2) (Angle $ m' * 6)
  where
    h' = fromIntegral h
    m' = fromIntegral m

fromClock :: Clock -> Time
fromClock (Clock (Angle h) (Angle m)) =
  Time (round $ (h - m / 2) / 30) (round $ m / 6)
