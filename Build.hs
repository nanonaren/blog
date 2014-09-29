import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory (getCurrentDirectory)

blogliterately :: String
blogliterately = "BlogLiterately"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
  want [
         "_build/hungarian_algorithm.html"
       , "_build/schensted.html"
       , "_build/short_circuiting.html"
       ]

  blogWithFigs "" ["hungarian_fig1"] "hungarian_algorithm"
  blogWithFigs "" ["schensted_fig1"] "schensted"
  blogWithFigs "short_circuiting" ["short_circuiting_fig1"] "short_circuiting"

-- Utils

  "_build/gen_figs" <.> exe *> \out -> do
    let inp = "figs" </> takeBaseName out <.> "hs"
    need [inp]
    () <- cmd "cabal" "build" (takeBaseName out)
    cmd "mv" "dist/build/gen_figs/gen_figs" "_build/"

blogWithFigs ghciPath figs fp = do

  (map (\x -> "_build" </> x <.> "png") figs) |*>  \fig -> do
    need ["_build/gen_figs"]
    cmd "_build/gen_figs" "--selection" (takeBaseName fig) "-o" fig "-w 400"

  ("_build" </> fp <.> "html") *> \out -> do
    let inp = fp <.> "lhs"
        figures = map (\x -> "_build" </> x <.> "png") figs
    need (inp : figures)
    ghci <- if ghciPath == "" then addPath [] []
            else do
              dir <- liftIO getCurrentDirectory
              () <- cmd Shell "echo \\#\\!/bin/bash > ghci"
              () <- cmd Shell "echo cabal repl" ghciPath " >> ghci"
              () <- cmd "chmod" "+x" "ghci"
              addPath [dir] []
              
    Stdout x <- cmd ghci blogliterately "--html-only -g" inp
    liftIO $ writeFile out x
  
