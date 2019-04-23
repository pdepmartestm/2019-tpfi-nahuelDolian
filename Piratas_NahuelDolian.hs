data Pirata = Pirata {nombre:: String, botin :: [Tesoro]} deriving (Show)

type Tesoro = (String, Int)

cantidadDeTesoros :: Pirata -> Int
cantidadDeTesoros  =  length.botin 

esAfortunado :: Pirata -> Bool
esAfortunado  = (>10000).suBotin    -- any >10000 (valor (botin pirata))

suBotin :: Pirata -> Int
suBotin pirata = sum (map snd (botin pirata))

suTesoroMasValioso :: Pirata -> Int
suTesoroMasValioso pirata =  maximum (map snd (botin pirata)) 

esValioso :: Tesoro -> Bool
esValioso  = (>100). snd 

agregarTesoro :: Pirata -> Tesoro -> Pirata
agregarTesoro pirata unTesoro = pirata {botin = (botin pirata)++ [unTesoro]}

susTesorosValiosos :: [Tesoro]->[Tesoro]
susTesorosValiosos botin = filter esValioso botin

--perderTesorosValiosos :: Pirata -> Pirata
perderTesorosValiosos  = (filter (not.esValioso)).botin  


perderElTesoro :: Pirata->String->Pirata
perderElTesoro pirata nombre = pirata{botin= filter (tesoroIgualNombre nombre) (botin pirata)}

tesoroIgualNombre :: String ->Tesoro->Bool
tesoroIgualNombre nombre1 tesoro = nombre1== (fst tesoro)

adquirirTesoro :: Pirata->Tesoro->Pirata
adquirirTesoro unPirata unTesoro = unPirata{botin = ((++ [unTesoro]).botin) unPirata}




jackSparrow = Pirata { nombre = "Jack Sparrow",
			botin = [ ("brujula", 10000), ("frasco De Arena", 0)]}

davidJones = Pirata { nombre= "David Jones", botin= [("cajita Musical",1)]}

anneBonny = Pirata{
	nombre="Anne Bonny",
	botin=[("doblones",100),("frasco de arena",1)]
}
			-- para fijarte si dos piratas tienen el mismo coso,. zipwith
			-- para perder los mas valiosos filter >100
			-- agregar un elemento a una lista -> [a] ++ [b,x] = [a,b,x]

-- temporada de saqueos bbcita

--type FormaDeSaqueo = String
--saquear :: Pirata->FormaDeSaqueo->[Tesoro]->Pirata
--saquear unPirata unFormaDeSaqueo tesoros = fromaDeSaqueo unaformaDeSaqueo unPirata tesoros

saqueoValioso :: Pirata->[Tesoro]->Pirata
saqueoValioso unPirata unBotin= unPirata{botin= susTesorosValiosos unBotin ++ botin unPirata}

saqueoEspecifico :: Pirata->[Tesoro]->String->Pirata
saqueoEspecifico unPirata botin nombre = agregarTesoro (filter (tesoroIgualNombre nombre) botin) unPirata 

