-- A more elegant way is to use traverse/mapM.
getPINs :: String -> [String]
getPINs = func . map trans

trans :: Char -> String
trans '0' = "08"
trans '1' = "124"
trans '2' = "1235"
trans '3' = "236"
trans '4' = "1457"
trans '5' = "24568"
trans '6' = "3569"
trans '7' = "478"
trans '8' = "05789"
trans '9' = "689"

func :: [String] -> [String]
func [] = [""]
func (s:ss) = [c:str | c <- s, str <- func ss]