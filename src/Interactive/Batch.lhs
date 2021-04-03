> module Interactive.Batch (batchExecution) where

> import Core.Parser
> import Core.Pretty
> import Core.Syntax
> import Core.TypeChecker

> import Data.Aeson

> import System.FilePath.Posix

Definition of batch execution mode for libertas
-----------------------------------------------

In batch mode, the libertas system will execute the
proof checker on an input file and produce, as a result,
a file clf, which is essentially a JSON format with
the checked term and its type.


> batchExecution :: FilePath -> IO ()
> batchExecution file
>       = do
>           content <- readFile file
>           case parseModule content of
>               Left err -> print err
>               Right m  -> case checkModule m of
>                             Left errs -> showErrors errs
>                             Right ts  -> writeCLF file ts

Functions for error reporting

> showErrors :: [Error] -> IO ()
> showErrors = mapM_ showError

> showError :: Error -> IO ()
> showError ExpectedFunction
>       = putStrLn "Expected a function."
> showError (UndefinedVariable n)
>       = putStrLn $ "Undefined variable:" ++ (pretty n)
> showError (MatchingError t t')
>       = putStrLn $ "Cannot match\n" ++ (pretty t) ++ "\nwith\n" ++ (pretty t')
> showError (InferenceError e)
>       = putStrLn $ "Cannot check the proof of\n" ++ (pretty e)

Writing a type checked value as a JSON type.

> writeCLF :: FilePath -> [Theorem] -> IO ()
> writeCLF file ts
>       = do
>           let fname = fst $ splitExtension file
>               file' = addExtension fname "clf"
>               tdata = unlines $ map (show . encode) ts
>           mapM_ (putStrLn . show . pprint) ts
>           writeFile file' tdata
