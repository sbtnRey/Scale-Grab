> import qualified Data.Text    as Text
> import qualified Data.Text.IO as Text
> import Data.List.Split
> import Data.List
> import System.IO.Unsafe
> import System.Random
> import Euterpea


--------------------------------------------------------------------------------
STRING TO PITCH CLASS
--------------------------------------------------------------------------------

Euterpea lacks conversion between notes in the form of a string to the pitch
class that it utilizes. This simply converts each note to the pitch class.

> strToPc s = case s of
>  "C" -> C; "Cs" -> Cs; "Css" -> Css; "Cf" -> Cf; "Cff" -> Cff;
>  "D" -> D; "Ds" -> Ds; "Dss" -> Dss; "Df" -> Df; "Dff" -> Dff;
>  "E" -> E; "Es" -> Es; "Ess" -> Ess; "Ef" -> Ef; "Eff" -> Eff;
>  "F" -> F; "Fs" -> Fs; "Fss" -> Fss; "Ff" -> Ff; "Fff" -> Fff;
>  "G" -> G; "Gs" -> Gs; "Gss" -> Gss; "Gf" -> Gf; "Gff" -> Gff;
>  "A" -> A; "As" -> As; "Ass" -> Ass; "Af" -> Af; "Aff" -> Aff;
>  "B" -> B; "Bs" -> Bs; "Bss" -> Bss; "Bf" -> Bf; "Bff" -> Bff;

--------------------------------------------------------------------------------
NOTE LIST CREATOR
--------------------------------------------------------------------------------

> noteList :: Dur -> Octave -> [[Char]] -> Music Pitch
> noteList d o l = line $ map ((\x -> note d (x,o::Octave)) . strToPc) l


--------------------------------------------------------------------------------
READ/CLEAN EXTERNAL SCALE DATA
--------------------------------------------------------------------------------

> scaleShow = do
>   init (chunksOf 1  $ splitOn ";" $ unsafePerformIO . readFile $ "scaleData.txt")

> sData = [ splitOn ":" $ c | x <- scaleShow, c <- x]

--------------------------------------------------------------------------------
SEARCHES/GRABS
--------------------------------------------------------------------------------

Grabs ID and outputs the list of notes
to utilize with play, ex: play $ forever $ noteList qn 3 $ idGrab "6-35-1"

> idGrab :: String -> [String]
> idGrab id = splitOn "." ((head $ dropWhile (\xs -> id /= xs !! 0 ) sData) !! 3)


Search data for scales at specified length/number of notes, the function will
output all found matches. ex: scaleLen 3

> scaleLen :: Integer -> IO ()
> scaleLen noteL = do
>   let y = [xs | xs <- sData, xs !! 2 == (show noteL)]
>   mapM_ print y


Search data for a random scale given the length of a scale. ex: ranScale 3

> ranScale :: Integer -> IO [[Char]]
> ranScale noteL = do
>   let y = [xs | xs <- sData, xs !! 2 == (show noteL)]
>   fmap (y !!) $ randomRIO (0, length y - 1)
