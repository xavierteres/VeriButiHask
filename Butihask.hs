module Butihask where

------------------------------------------------------- TIPUS -------------------------------------------------------

data TipusCarta = Manilla | As | Rei | Cavall | Sota | Vuit | Set | Sis | Cinc | Quatre | Tres | Dos deriving (Show, Enum, Eq, Ord)

data Pal = Oros | Copes | Espases | Bastos deriving (Show, Eq, Ord)

data Carta = Carta {tipusCarta :: TipusCarta, pal :: Pal} deriving (Eq, Ord)

data Trumfu = Trumfu {palTrumfu :: Pal} | Butifarra deriving (Show, Eq)

instance Show Carta where
  show (Carta t p)
    | p == Espases || p == Oros = show t ++ " d'" ++ show p
    | otherwise = show t ++ " de " ++ show p

---------------------------------------------------- PRINCIPALS ----------------------------------------------------

-- comprovarBases: funció d'immersió per fer la recursivitat de trampa
comprovarBases :: [[Carta]] -> Trumfu -> [[Carta]] -> Int -> Int -> Maybe ([Carta],Int, Int)
comprovarBases _ _ [] _ _ = Nothing
comprovarBases cj t (bAct:xs) p nBasa =
  case bc of
    Just x -> Just (bAct, nBasa, x) -- hi ha trampa
    Nothing -> comprovarBases cjNext t xs next (nBasa + 1)
  where
    ordreTirada = [seguent x | x <- [p - 1 .. (p + 2)]] -- els jugadors per ordre de tirada
    bc = basaCorrecta cj t p bAct -- ens dius si la basa actual és correcta
    next = quiSortira p (snd(quiGuanya bAct t) + 1) -- jugador que començarà la següent basa
    cjNext = [borrarElement (cj !! x) (bAct !! posicioLlista ordreTirada (x + 1)) | x <- [0..3]] -- cartes dels jugadors a la seguent basa

-- trampa: Donades les cartes dels quatre jugadors, donat el trumfu (el pal que mana) de la partida,
-- la llista de cartes tirades per ordre (de la primera a l'última) i el número de jugador que
-- ha tirat la primera carta, ens retorna (si efectivament hi ha hagut trampa) la basa i el
-- número de basa on s’ha produït la trampa i el jugador que l’ha feta
trampa:: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe ([Carta],Int, Int)
trampa cj t tirades p = comprovarBases cj t bases p 1
  where
    bases = [take 4 (drop (x*4) tirades) | x <- [0 .. nombreBases - 1]] -- cartes separades per basa
    nombreBases = length tirades `div` 4

-- gunyadorBases: funció d'immersió per fer la recursivitat de cartesGuanyades
guanyadorBases :: Trumfu -> [[Carta]] -> Int -> ([Carta], [Carta])
guanyadorBases _ [] _ = ([], [])
guanyadorBases t (bAct:xs) p
  | jg == 0 || jg == 2 = (bAct ++ fst gb, snd gb) -- primer equip
  | otherwise = (fst gb, bAct ++ snd gb) -- segon equip
  where
    ordreTirada = [seguent x | x <- [p - 1 .. (p + 2)]] -- els jugadors per ordre de tirada
    jg = ordreTirada !! snd(quiGuanya bAct t) - 1 -- jugador que ha guanyat la basa
    next = quiSortira p (snd(quiGuanya bAct t) + 1) -- jugador que començarà la següent basa
    gb = guanyadorBases t xs next -- cartes guanyades en les següents bases

-- cartesGuanyades: Donat el trumfu de la partida, la llista de cartes tirades per ordre (de la primera a la
-- última) i el número de jugador que ha tirat la primera carta, ens retorna una tupla amb
-- les llistes de cartes guanyades per cada parella
cartesGuanyades:: Trumfu -> [Carta] -> Int -> ([Carta],[Carta])
cartesGuanyades t ct p = guanyadorBases t bases p
  where
    bases = [take 4 (drop (x*4) ct) | x <- [0 .. nombreBases - 1]] -- cartes separades per basa
    nombreBases = length ct `div` 4

-- punts: Donada una llista de cartes, en conta els punts que hi ha
punts :: [Carta] -> Int
punts [] = 0
punts (x : xs)
  | p >= 5 = punts xs
  | otherwise = (5 - p) + punts xs
  where
    p = fromEnum (tipusCarta x)

-- puntsParelles: Donades les cartes dels quatre jugadors, donat el trumfu de la partida (el pal que mana),
-- la llista de cartes tirades per ordre (de la primera a l'última) i el número de jugador que
-- ha tirat la primera carta, ens retorna (si no hi ha hagut trampa) una tupla amb els punts
-- fets per la primera i per la segona parella
puntsParelles:: [[Carta]] -> Trumfu -> [Carta] -> Int -> Maybe (Int, Int)
puntsParelles cj t tirades p
  | trampa cj t tirades p == Nothing = Just (p1, p2)
  | otherwise = Nothing
  where
    p1 = punts (fst cg) + length(fst cg) `div` 4
    p2 = punts (snd cg) + length(snd cg) `div` 4
    cg = cartesGuanyades t tirades p

---------------------------------------------------- ADICIONALS ----------------------------------------------------

-- cartesPal: donada una llista de cartes i un pal, torni les que són del pal donat
-- utilitza el predicat per filtrar on el pal de la carta és igual al paràmetre
cartesPal :: [Carta] -> Pal -> [Carta]
cartesPal xs p = filter (\c -> pal c == p) xs

-- hiHaPal: donada una llista de cartes i un pal retorna cert si en la llista de cartes
-- hi ha alguna que és del pal del paràmetre
hiHaPal :: [Carta] -> Pal -> Bool
hiHaPal xs p = length (cartesPal xs p) > 0

-- palGuanyadorBasa: donada una llista de cartes (en ordre de tirada), i el pal del trumfu,
-- ens indiqui quin és el pal que està guanyant la basa
palGuanyadorBasa :: [Carta] -> Trumfu -> Pal
palGuanyadorBasa [] _ = error "llista buida"
palGuanyadorBasa (x : xs) t
  | t == Butifarra = pal x
  | hiHaPal (x : xs) (palTrumfu t) = palTrumfu t
  | otherwise = pal x

-- posicioLlista: donada una llista i un element, retorna la posició de l'element
posicioLlista :: Eq a => [a] -> a -> Int
posicioLlista [] _ = error "llista buida o element no hi pertany"
posicioLlista (x : xs) y
  | x == y = 0
  | otherwise = 1 + posicioLlista xs y

-- quiGuanya: donada una llista de cartes (en ordre de tirada) i el pal del trumfu,
-- ens retorni la carta guanyadora i la seva posició a la llista
quiGuanya :: [Carta] -> Trumfu -> (Carta, Int)
quiGuanya xs t = (c, posicioLlista xs c)
  where
    c = minimum (cartesPal xs (palGuanyadorBasa xs t)) -- carta guanyadora

-- quiSortira: donat el nombre de jugador que ha començat la basa, i la posició del
-- que ha guanyat la basa, ens digui quin és el nombre de jugador que començarà la següent basa
-- (és a dir, el que ha guanyat la basa actual). Per exemple, si ha començat tirant
-- el jugador 2 i guanya la basa el que ha tirat segon, començarà la següent basa el jugador 3
quiSortira :: Int -> Int -> Int
quiSortira x y
  | x + y - 1 > 4 = x + y - 5
  | otherwise = x + y - 1

-- jugadesCompany: donades les cartes del jugador i les cartes tirades ens retorna les cartes que es poden tirar en el cas que sigui el nostre company el que vagi guanyant
jugadesCompany :: [Carta] -> [Carta] -> [Carta]
jugadesCompany cartes [] = cartes
jugadesCompany cartes (sortida : xs)
  -- si hi ha pal sortida, hem de tirar-ne una d'aquestes
  | hiHaPal cartes (pal sortida) = cartesPal cartes (pal sortida)
  -- si hi ha pal sortida, hem de tirar-ne una d'aquestes
  | otherwise = cartes

-- cartesMaten: donada una llista de cartes, la carta que volem matar i el trumfu de la partida, ens retorna les cartes que maten a la carta
cartesMaten :: [Carta] -> Carta -> Trumfu -> [Carta]
-- maten les cartes del pal victima superiors a la victma + les del pal del trumfu (en cas que sigui diferent al de la victima)
cartesMaten cartes victima t = filter (\c -> pal c == pal victima && (tipusCarta c < tipusCarta victima)) cartes ++ extres
  where
    extres
      | t == Butifarra || pal victima == palTrumfu t = []
      | otherwise = filter (\c -> pal c == palTrumfu t) cartes -- afegim també les que són del Trumfu

-- jugadesCompany: donades les cartes del jugador, el trumfu de la partida i les cartes tirades ens
-- retorna les cartes que es poden tirar en el cas que el rival vagi guanyant
jugadesNoCompany :: [Carta] -> Trumfu -> [Carta] -> [Carta]
jugadesNoCompany [] _ _ = []
jugadesNoCompany _ _ [] = []
jugadesNoCompany cartes t (sortida : xs)
  -- si el jugador té cartes del pal de sortida haurà de jugar-ne una d'aquest pal i sempre que pugui haurà de matar-la
  | hiHaPal cartes (pal sortida) && hiHaPal maten (pal sortida) = cartesPal maten (pal sortida)
  | hiHaPal cartes (pal sortida) = cartesPal cartes (pal sortida)
  -- si el jugador no té cap carta del pal de sortida però en té d'altres que guanyen la basa (triomf) haurà de jugar una carta que guanyi la basa
  | length maten > 0 = maten
  -- si el jugador no té ni cap carta del pal de sortida ni cap carta que guanyi la basa, podrà jugar la carta que vulgui
  | otherwise = cartes
  where
    guanyant = fst (quiGuanya (sortida : xs) t) -- millor carta de la basa
    maten = cartesMaten cartes guanyant t -- totes les cartes que poden matar a la carta guanyadora

-- jugades: donades les cartes que té un jugador, el pal de la partida i les cartes tirades fins al
-- moment en la basa actual, ens retorna la llista de cartes que pot tirar (d’acord amb les normes del joc)
jugades :: [Carta] -> Trumfu -> [Carta] -> [Carta]
jugades [] _ _ = []
jugades cartes _ [] = cartes -- som els primers en tirar
jugades cartes t tirades
  | tirs >= 2 && snd (quiGuanya tirades t) == tirs - 2 = jugadesCompany cartes tirades -- el nostre company ha tirat la millor carta de la basa
  | otherwise = jugadesNoCompany cartes t tirades
  where
    tirs = length tirades

-- seguent: donat el numero del jugador retorna el següent que tirara en la basa actual
seguent :: Int -> Int
seguent x = x `mod` 4 + 1

-- basaCorrecta: donades la llista de llistes de cartes dels jugadors, donat el pal de la partida,
-- donat el jugador que ha tirat primer a la basa, i donada la llista de cartes de la basa, ens digui,
-- si hi ha hagut trampa, qui ha fet la trampa
basaCorrecta :: [[Carta]] -> Trumfu -> Int -> [Carta] -> Maybe Int
basaCorrecta cj t p basa
 | elem True tramposos = Just (ordreJugadors !! posicioLlista tramposos True)
 | otherwise = Nothing
  where
    -- la basa en cada canvi començant per llista buida
    bases = [take x basa | x <- [0..length basa]]

    -- llista dels jugadors per ordre de tirada
    ordreJugadors = [seguent x | x <- [p - 1 .. (p + 2)]]

    -- llista de cartes que pot jugar cada jugador (en ordre de tirada)
      -- (ordreJugadors !! x) - 1 => ens retorna l'index del jugador que ha tirat al torn x [0..3]
      -- (bases !! x) => ens retorna la basa abans que tiri el jugador actual
    cartesJugades = [jugades (cj !! ((ordreJugadors !! x) - 1)) t (bases !! x) | x <- [0..3]]

    -- llista que ens diu si els jugadors han fet trampes (en ordre de tirada)
      -- (cartesJugades !! x) => cartes que podia jugar el jugador
      -- ((bases !! (x + 1)) !! x) => ens diu la carta que ha tirat el jugador
    tramposos = [notElem ((bases !! (x + 1)) !! x) (cartesJugades !! x) | x <- [0..3]]

-- borrarElement: donada una llista i un element, retornem la llista sense aquest element
borrarElement :: Eq a => [a] -> a -> [a]
borrarElement xs x = filter (/= x) xs
