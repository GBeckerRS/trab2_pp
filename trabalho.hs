module TrabalhoHaskell where
import System.IO

type Origem = String
type Destino = String
type Distancia = Double
type Valor = Double
type Trecho = [(Origem, Destino, Distancia, Valor)]

trechoViagem = [("Porto Alegre", "Florianopolis", 376.18, 188.09), 
				("Porto Alegre", "Curitiba", 547.51, 273.76), 
				("Porto Alegre", "São Paulo", 853.30, 426.65), 
				("Porto Alegre", "Rio De Janeiro", 1127.20, 563.60), 
				("Belo Horizonte", "Sao Paulo", 491.40, 245.70), 
				("Belo Horizonte", "Rio De Janeiro", 342.40, 171.05), 
				("Curitiba", "Rio De Janeiro", 679.52, 339.77), 
				("Brasilia", "Belo Horizonte", 620.85, 310.43), 
				("Rio De Janeiro", "São Paulo", 361.15, 180.58)]
				

destino :: Trecho -> Origem -> [(Destino, Distancia,Valor)]
destino ((org, dest, dist, val):r) origem
    | r == [] && org == origem = [(dest, dist,val)]
	| r == [] && org /= origem = []
    | org == origem = (dest, dist,val) : destino r origem
    | otherwise = destino r origem

origens :: Trecho -> Destino -> [(Origem, Distancia,Valor)]
origens ((org, dest, dist, val):r) destinoViagem
    | r == [] && dest == destinoViagem = [(org, dist, val)]
	| r == [] && dest /= destinoViagem = []
    | dest == destinoViagem = (org, dist,val) : origens r destinoViagem
    | otherwise = origens r destinoViagem 

cabecalho::String
cabecalho = "Origem\t\tDestino\t\tDistancia (Km)\t\tValor (R$)\n"

lViagem :: Trecho->String
lViagem [] = " "
lViagem ((orig, dest, dist, val) : tail) = 
    orig ++ "\t\t" ++ dest ++ "\t\t" ++ show dist ++ "\t\t" ++ show val ++ "\n"
    ++ lViagem tail

