module Main where

import Options.Applicative
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Exit (exitFailure)
import System.Process (callCommand)
import Control.Exception (try, IOException)

import OntoLogic
import Parser
import Linter
import Emitters.Java (emitJava)
import Emitters.Python (emitPython)
import Emitters.Neo4j (emitCypher)
import Emitters.Owl (emitOwl)
import Emitters.Mermaid (emitMermaid)
import Emitters.D2 (emitD2)

data Args = Args
  { input  :: String
  , target :: String
  , output :: String
  , render :: Bool
  }

argsParser :: ParserInfo Args
argsParser = info (helper <*> opts) fullDesc
  where
    opts = Args
      <$> strOption (long "input" <> short 'i' <> help "Path to .logic file")
      <*> strOption (long "target" <> short 't' <> help "Target: java, python, neo4j, owl, mermaid, d2, lint")
      <*> strOption (long "output" <> short 'o' <> help "Output directory or file")
      <*> switch (long "render" <> help "Render output to image (requires mmdc or d2 CLI installed)")

main :: IO ()
main = do
    args <- execParser argsParser
    content <- TIO.readFile (input args)
    
    case parse parseRegistry (input args) content of
        Left err -> do
            putStrLn $ errorBundlePretty err
            exitFailure
        Right registry -> do
            let errors = lintRegistry registry
            if not (null errors)
                then mapM_ putStrLn errors >> error "Validation Failed."
                else processTarget (target args) (output args) (render args) registry

processTarget :: String -> String -> Bool -> Registry -> IO ()
processTarget "lint" _ _ _ = putStrLn "✅ Logic Validated successfully."

processTarget "java" out _ reg = do
    writeFile (out ++ "/Model.java") (emitJava reg)
    putStrLn $ "Generated Java models in " ++ out

processTarget "python" out _ reg = do
    writeFile (out ++ "/models.py") (emitPython reg)
    putStrLn $ "Generated Python models in " ++ out

processTarget "neo4j" out _ reg = do
    writeFile (out ++ "/graph.cypher") (emitCypher reg)
    putStrLn $ "Generated Neo4j Cypher in " ++ out

processTarget "owl" out _ reg = do
    writeFile (out ++ "/ontology.owl") (emitOwl reg)
    putStrLn $ "Generated OWL ontology in " ++ out

processTarget "mermaid" out doRender reg = do
    let outFile = out ++ "/class_diagram.mmd"
    writeFile outFile (emitMermaid reg)
    putStrLn $ "Generated Mermaid diagram in " ++ outFile
    if doRender
        then renderMermaid outFile
        else putStrLn "Skipping render (use --render to generate SVG)."

processTarget "d2" out doRender reg = do
    let outFile = out ++ "/diagram.d2"
    writeFile outFile (emitD2 reg)
    putStrLn $ "Generated D2 diagram in " ++ outFile
    if doRender
        then renderD2 outFile
        else putStrLn "Skipping render (use --render to generate SVG)."

processTarget t _ _ _ = putStrLn $ "Unknown target: " ++ t

-- Helpers for rendering
renderMermaid :: String -> IO ()
renderMermaid src = do
    let svg = src ++ ".svg"
    putStrLn $ "Rendering to " ++ svg ++ " using mmdc..."
    result <- try (callCommand $ "mmdc -i " ++ src ++ " -o " ++ svg) :: IO (Either IOException ())
    case result of
        Left _ -> putStrLn "⚠️  Error: 'mmdc' (Mermaid CLI) not found or failed. Install with 'npm install -g @mermaid-js/mermaid-cli'"
        Right _ -> putStrLn "✅ Rendered SVG successfully."

renderD2 :: String -> IO ()
renderD2 src = do
    let svg = src ++ ".svg"
    putStrLn $ "Rendering to " ++ svg ++ " using d2..."
    result <- try (callCommand $ "d2 " ++ src ++ " " ++ svg) :: IO (Either IOException ())
    case result of
        Left _ -> putStrLn "⚠️  Error: 'd2' CLI not found or failed. Install from https://d2lang.com"
        Right _ -> putStrLn "✅ Rendered SVG successfully."
