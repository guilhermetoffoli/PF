module Main (main) where
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))


main = do hSetBuffering stdout NoBuffering
          putStrLn "=============================="
          putStrLn "Banco Guilherme Duarte Toffoli"
          putStrLn "=============================="
          putStrLn "Opções:"
          putStrLn "[1] Saldo"
          putStrLn "[2] Extrato"
          putStrLn "[3] Depósito"
          putStrLn "[4] Saque"
          putStrLn "[5] Fim"
          putStrLn "Escolha uma opção:"
          op <- readLn
          case op of
            1 -> do putStr "Seu saldo é: "
                    imprime "saldo.txt"
            2 -> do putStr"Seu extrato é: "
                    imprime "extrato.txt"
            3 -> do putStrLn "Digite o valor a ser depositado: "
                    valor <- readLn
                    deposito valor
            4 -> do putStrLn "Digite o valor a ser sacado: "
                    valor <- readLn
                    saque valor
            5 -> putStrLn "Obrigado por usar banco Guilherme"
            _ -> putStrLn "op invalida"
          if not (op == 5) then main else putStrLn "Volte sempre!"


imprime :: String -> IO()
imprime string = do conteudo <- readFile string
                    putStrLn conteudo 

deposito :: Float -> IO()
deposito dep = do saldo <- readFile "saldo.txt"
                  putStrLn saldo
                  writeFile "saldo.txt" (show(dep+(read saldo)))
                  appendFile "extrato.txt" ("+" ++ (show(dep))) 

saque :: Float -> IO()
saque saq = do saldo <- readFile "saldo.txt"
               putStrLn saldo
               writeFile "saldo.txt" (show((read saldo)-saq))
               appendFile "extrato.txt" ("-" ++ (show(saq))) 



