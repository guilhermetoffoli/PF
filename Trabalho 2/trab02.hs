type Codigo = Int
type Nome = String
type Preco = Float

type Produto = (Codigo, Nome, Preco)

tabelaProdutos :: [Produto]
tabelaProdutos=[(001,"Chocolate",5.25)
               , (002,"Biscoito",10.1)
               , (003,"Laranja",4.6)
               , (004,"Sabao",2.10)
               , (005,"Batata Chips",6.9)
               , (006,"Doritos",8.9)]

getCodigo (codigo,_,_)=codigo
getNome (_,nome,_)=nome
getPreco (_,_,preco)=preco

isCodigo:: Codigo -> Produto -> Bool
isCodigo codigo produto
  |getCodigo produto == codigo = True
  |otherwise = False

buscaPrecoPorCodigo :: Int ->Preco
buscaPrecoPorCodigo codigo  = head (map getPreco(filter (isCodigo codigo) tabelaProdutos))

buscaNomePorCodigo :: Int  ->Nome
buscaNomePorCodigo codigo = head (map getNome(filter (isCodigo codigo) tabelaProdutos))

calculaPrecos :: [Codigo]-> Preco
calculaPrecos x = foldr (+) 0 (map (buscaPrecoPorCodigo) x)

formataStrProduto :: Codigo -> String 
formataStrProduto codigo = (buscaNomePorCodigo codigo) ++  replicate (30 -(length (buscaNomePorCodigo codigo)) - (length (show (buscaPrecoPorCodigo codigo)))) '.' ++(show(buscaPrecoPorCodigo codigo )) ++ ("\n")

geraNotaFiscalaux:: [Codigo] -> String
geraNotaFiscalaux codigos
    |codigos == [] = "\n"
    |otherwise = formataStrProduto (head codigos) ++ geraNotaFiscalaux (tail codigos)

geraNotaFiscal :: [Codigo] -> IO()
geraNotaFiscal codigos = do writeFile "notaFiscal.txt" "---Nota Fiscal Supermercado do Guilherme---\n\n"
                            appendFile "notaFiscal.txt" (geraNotaFiscalaux codigos) 
                            appendFile "notaFiscal.txt" ("Total: " ++ replicate 19 '.'++(show (calculaPrecos codigos))) 