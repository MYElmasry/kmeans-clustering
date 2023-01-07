import Data.List (genericLength)
import System.IO

kmeans :: (Floating a, Ord a) => [[a]] -> Int -> [Int]
kmeans xs k = _kmeans xs $ clusters xs (take k xs)

_kmeans :: (Floating a, Ord a) => [[a]] -> [Int] -> [Int]
_kmeans xs asgn
  | newAsgn == asgn = asgn
  | otherwise = _kmeans xs newAsgn
  where
    newAsgn = clusters xs $ calcMeans xs asgn

calcMeans :: (Floating a) => [[a]] -> [Int] -> [[a]]
calcMeans xs asgn = map listMean [[x | (x, i) <- zip xs asgn, i == j] | j <- [0 .. k]]
  where
    k = maximum asgn

listMean :: (Floating a) => [[a]] -> [a]
listMean [] = error "mean of empty list "
listMean xs = map (/ k) (foldl1 (zipWith (+)) xs)
  where
    k = genericLength xs

clusters :: (Floating a, Ord a) => [[a]] -> [[a]] -> [Int]
clusters xs ms = map minIndex dists
  where
    dists = [map (euclideanDist x) ms | x <- xs]

euclideanDist :: Floating a => [a] -> [a] -> a
euclideanDist xs ys = sqrt (sum [(x - y) ^ 2 | (x, y) <- zip xs ys])

minIndex :: Ord a => [a] -> Int
minIndex (x : xs) = _minIndex xs x 0 0

_minIndex :: (Ord a) => [a] -> a -> Int -> Int -> Int
_minIndex [x] val cix ix
  | x < val = cix + 1
  | otherwise = ix
_minIndex (x : xs) val cix ix
  | x < val = _minIndex xs x (cix + 1) (cix + 1)
  | otherwise = _minIndex xs val (cix + 1) ix

testData =
  [ [19.44228191, 10.01954739],
    [19.53244135, 9.27926943],
    [19.47813524, 10.11702067],
    [19.53707403, 10.34821127],
    [20.22351335, 10.15144988],
    [19.28171056, 9.97469793],
    [20.02206671, 10.44740153],
    [20.50197584, 10.21343973],
    [19.21130219, 10.12152178],
    [20.12801058, 9.72609094],
    [19.83785357, 10.26330778],
    [20.41310726, 9.46206339],
    [19.45552404, 9.40547838],
    [19.89360546, 10.44014115],
    [20.36125385, 9.90567986],
    [20.35849112, 10.48796981],
    [20.88746346, 10.35747471],
    [20.49813333, 10.65385512],
    [20.62931967, 10.31353257],
    [19.71910217, 10.11721853],
    [23.88999494, 9.49780921],
    [23.53417881, 10.22200125],
    [24.20515374, 10.20352821],
    [24.27042841, 8.94589297],
    [24.33017732, 10.46796864],
    [24.1848164, 9.39154275],
    [24.25760608, 10.19827234],
    [24.14990874, 9.95071613],
    [23.79546847, 9.40557273],
    [24.12813273, 9.45732106],
    [24.88221126, 10.67626518],
    [24.24725526, 9.38441264],
    [23.70951565, 9.48528594],
    [23.84057452, 10.4090588],
    [25.30846271, 10.06039913],
    [23.88388447, 9.42833493],
    [23.77114657, 9.98375689],
    [23.40834507, 10.49376609],
    [23.98728878, 10.22910904],
    [22.96215117, 9.97424991],
    [19.58555678, 11.8102289],
    [20.03125014, 10.6541982],
    [20.61266943, 11.811877],
    [19.81763777, 12.87517492],
    [19.70835143, 11.64469843],
    [20.08416497, 11.49784966],
    [19.82591625, 11.04150988],
    [19.28604287, 12.57047196],
    [19.60033648, 12.37376337],
    [19.73600258, 11.13350926],
    [20.6532054, 12.40243406],
    [19.88155388, 11.5102145],
    [20.16608498, 11.52421444],
    [19.84189865, 12.16468441],
    [20.07146249, 11.9723343],
    [19.80540283, 12.25633198],
    [20.36592307, 12.04606745],
    [19.76154363, 12.04326451],
    [21.13411946, 11.97343609],
    [20.00825767, 12.22197451]
  ]

convert :: Show a => [a] -> [String]
convert xs = [show x ++ "," | x <- xs]

concat_ :: [String] -> String
concat_ [x] = x
concat_ (x : xs) = x ++ concat_ xs

main = do
  let res = concat_ (convert (kmeans testData 3))
  writeFile "res.txt" (init res)
