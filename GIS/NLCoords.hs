{-|
Module      : GIS.NLCoords
Description : Code for conversion between WGS and Rijksdriehoek coordinates.
              Adapted from http://thomasv.nl/2014/03/rd-naar-gps/ based on
              http://media.thomasv.nl/2015/07/Transformatieformules.pdf
Author      : Sam van Herwaarden <samvherwaarden@gmail.com>
-}

module GIS.NLCoords ( WGS(..),
                      wgsLat,
                      wgsLon,
                      RD(..),
                      rdX,
                      rdY,
                      rd2wgs,
                      wgs2rd
                    ) where

newtype WGS = WGS { unWGS :: (Double, Double) }
              deriving (Eq, Ord, Show)

wgsLat :: WGS -> Double
wgsLat = fst . unWGS

wgsLon :: WGS -> Double
wgsLon = snd . unWGS

wgs0 :: WGS
wgs0 = WGS (52.15517440, 5.38720621)

newtype RD = RD { unRD :: (Double, Double) }
             deriving (Eq, Ord, Show)

rdX :: RD -> Double
rdX = fst . unRD

rdY :: RD -> Double
rdY = snd . unRD

rd0 :: RD
rd0 = RD (155000, 463000)

transterm :: Double -> Double -> Double -> Double -> Double -> Double
transterm da db t_pq t_p t_q = t_pq * da**t_p * db**t_q

rd2wgs :: RD -> WGS
rd2wgs rdIn = WGS (lat, lon)
  where k_p = [0,2,0,2,0,2,1,4,2,4,1]
        k_q = [1,0,2,1,3,2,0,0,3,1,1]
        k_pq = [3235.65389,-32.58297,-0.24750,-0.84978,-0.06550,-0.01709,
                -0.00738,0.00530,-0.00039,0.00033,-0.00012]
        l_p = [1,1,1,3,1,3,0,3,1,0,2,5]
        l_q = [0,1,2,0,3,1,1,2,4,2,0,0]
        l_pq = [5260.52916,105.94684,2.45656,-0.81885,0.05594,-0.05607,
                0.01199,-0.00256,0.00128,0.00022,-0.00022,0.00026]
        dX = 1e-5 * ( rdX rdIn - rdX rd0 )
        dY = 1e-5 * ( rdY rdIn - rdY rd0 )
        f = transterm dX dY
        lat = wgsLat wgs0 + (sum $ zipWith3 f k_pq k_p k_q)
        lon = wgsLon wgs0 + (sum $ zipWith3 f l_pq l_p l_q)

wgs2rd :: WGS -> RD
wgs2rd wgsIn = RD (x, y)
  where r_p = [0,1,2,0,1,3,1,0,2]
        r_q = [1,1,1,3,0,1,3,2,3]
        r_pq = [190094.945,-11832.228,-114.221,-32.391,-0.705,
                -2.340,-0.608,-0.008,0.148]
        s_p = [1,0,2,1,3,0,2,1,0,1]
        s_q = [0,2,0,2,0,1,2,1,4,4]
        s_pq = [309056.544,3638.893,73.077,-157.984,59.788,
                0.433,-6.439,-0.032,0.092,-0.054]
        dLat = 0.36 * (wgsLat wgsIn - wgsLat wgs0)
        dLon = 0.36 * (wgsLon wgsIn - wgsLon wgs0)
        f = transterm dLat dLon
        x = rdX rd0 + (sum $ zipWith3 f r_pq r_p r_q)
        y = rdY rd0 + (sum $ zipWith3 f s_pq s_p s_q)
