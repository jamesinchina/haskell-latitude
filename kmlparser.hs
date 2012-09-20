import Data.List
import Data.Time.Format
import System.Locale
import Control.Applicative
import Text.XML.Light
import Data.Time.LocalTime
import System.Environment (getArgs)

main :: IO ()
main = do
  (filename:_) <- getArgs
  kml <- readFile filename
  putStr $ show (extractData kml)

extractData :: String -> [(ZonedTime, (Double, Double))]
extractData input = zip whens wheres 
  where Just doc = parseXMLDoc input
        whens    = convertTime   <$> extractAll "when"
        wheres   = convertCoords <$> extractAll "coord"
        convertTime = readTime defaultTimeLocale "%FT%T%Q%z"
        convertCoords cString = (x, y)
           where [x, y, _] = read <$> words cString
        extractAll name = strContent <$> filterElementsName ((==name) . qName) doc

