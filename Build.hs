import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

blogliterately :: String
blogliterately = "BlogLiterately"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
  want [
         "_build/hungarian_algorithm.html"
       , "_build/schensted.html"
       ]

  blogWithFigs ["hungarian_fig1"] "hungarian_algorithm"
  blogWithFigs ["schensted_fig1"] "schensted"

-- Utils

  "_build/gen_figs" <.> exe *> \out -> do
    let inp = "figs/gen_figs.hs"
    need [inp]
    cmd "ghc" "--make" "-o" out inp

blogWithFigs figs fp = do

  (map (\x -> "_build" </> x <.> "png") figs) |*>  \fig -> do
    need ["_build/gen_figs"]
    cmd "_build/gen_figs" "--selection" (takeBaseName fig) "-o" fig "-w 400"

  ("_build" </> fp <.> "html") *> \out -> do
    let inp = fp <.> "lhs"
        figures = map (\x -> "_build" </> x <.> "png") figs
    need (inp : figures)
    Stdout x <- cmd blogliterately "--html-only -g" inp
    liftIO $ writeFile out x
