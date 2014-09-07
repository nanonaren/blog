import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

blogliterately :: String
blogliterately = "BlogLiterately"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
  want [
         "_build/schensted.html"
       ]

-- BLOG: Schensted operation

  "_build/schensted.html" *> \out -> do
    let inp = "schensted.lhs"
    need [inp,"_build/schensted_fig1.png"]
    Stdout x <- cmd blogliterately "--html-only -g" inp
    liftIO $ writeFile out x

  "_build/schensted_figs" <.> exe *> \out -> do
    let inp = "figs/schensted_figs.hs"
    need [inp]
    cmd "ghc" "--make" "-o" out inp

  "_build/schensted_fig1.png" *> \out -> do
    need ["_build/schensted_figs"]
    cmd "_build/schensted_figs" "-o" out "-w 400"
