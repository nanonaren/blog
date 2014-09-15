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

-- BLOG: Schensted operation

  "_build/schensted.html" *> \out -> do
    let inp = "schensted.lhs"
    need [inp,"_build/schensted_fig1.png"]
    Stdout x <- cmd blogliterately "--html-only -g" inp
    liftIO $ writeFile out x

  "_build/schensted_fig1.png" *> \out -> do
    need ["_build/gen_figs"]
    cmd "_build/gen_figs" "--selection" (takeBaseName out) "-o" out "-w 400"

-- Utils

  "_build/gen_figs" <.> exe *> \out -> do
    let inp = "figs/gen_figs.hs"
    need [inp]
    cmd "ghc" "--make" "-o" out inp
