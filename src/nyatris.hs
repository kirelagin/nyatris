----------------
--
-- Kawaii Tetris
--
-- | Kirill Elagin <kirelagin@gmail.com>
--
--------

import qualified Data.List as L
import Control.Arrow
import Graphics.UI.WX hiding (Event, value)
import Reactive.Banana hiding (empty)
import Reactive.Banana.WX

import Nyatris.Shape
import Nyatris.Random


cellSize = 20
(fieldW, fieldH) = (10, 20)
(nextWpx, nextHpx) = (cellSize * (maxW+1), cellSize * (maxH+1))
    where maxW = maximum $ map shapeWidth shapes
          maxH = maximum $ map shapeHeight shapes

shapes = tetroI : tetroJ : tetroZ : tetroT : tetroO : tetroL : tetroS :[]
    where
        x = FilledCell
        o = EmptyCell

        tetroI = shape [[x,x,x,x]]
        tetroJ = shape [[o,o,x,o,o]
                       ,[o,o,x,x,x]
                       ,[o,o,o,o,o]]
        tetroZ = shape [[x,x,o]
                       ,[o,x,x]
                       ,[o,o,o]]
        tetroT = shape [[o,x,o]
                       ,[x,x,x]
                       ,[o,o,o]]
        tetroO = shape [[x,x]
                       ,[x,x]]
        tetroL = shapeMirrorH tetroJ
        tetroS = shapeMirrorH tetroZ

shapeColours = [cyan, magenta, yellow, green, blue, rgb 139 0 255]

main = start $ do
    f       <- frameFixed       [text := "Nyatris"]
    p       <- panel f          [border := BorderSunken
                                ]
    bPause  <- checkBox f       [text := "Pause"]
    bNew    <- button f         [text := "New game"]
    t       <- timer f          [interval := 500]
    score   <- staticText f     []
    next    <- panel f          [border := BorderSunken
                                ]

    -- FIXME: border widths!?
    set f [layout := margin 10 $ row 15 $
            [minsize (sz (cellSize * fieldW + 10) (cellSize * fieldH + 5)) $ widget p
            ,column 10 $ [widget score, minsize (sz (nextWpx + 5) (nextHpx + 5)) $ widget next, vfill empty, widget bPause, widget bNew]]
          ]
    focusOn bNew

    (handleNew, fireNew) <- newAddHandler
    (handlePaint, firePaint) <- newAddHandler
    (handlePaintNext, firePaintNext) <- newAddHandler

    --
    -- FIXME: I hate this generator separate from logicsNet
    let rndShapeSrc = chooseRIO shapes
    firstShape <- rndShapeSrc
    let rndShapeColourSrc = chooseRIO shapeColours
    firstShapeColour <- rndShapeColourSrc

    logicsNet  <- compile $ do
        etimer <- event0 t command
        ekey   <- event1 p keyboard
        enew   <- fromAddHandler handleNew
        epaint <- fromAddHandler handlePaint
        epaintnext <- fromAddHandler handlePaintNext
        brndshape <- fromPoll rndShapeSrc
        brndshapecolour <- fromPoll rndShapeColourSrc

        let
            eleft   = () <$ filterE ((`elem` p) . keyKey) ekey
                       where p = [KeyLeft, KeyChar 'a']
            eright  = () <$ filterE ((`elem` p) . keyKey) ekey
                       where p = [KeyRight, KeyChar 'd']
            erotcw  = () <$ filterE ((`elem` p) . keyKey) ekey
                       where p = [KeyUp, KeyChar 'e']
            erotccw = () <$ filterE ((`elem` p) . keyKey) ekey
                       where p = [KeyDown, KeyChar 'q']
            espace  = () <$ filterE ((`elem` p) . keyKey) ekey
                       where p = [KeySpace, KeyChar 's']
            eret    = () <$ filterE ((`elem` p) . keyKey) ekey
                       where p = [KeyReturn]

            etick = etimer `union` espace

            espawn = enew `union` ecollide

            bnextshape = stepper firstShape $ brndshape <@ espawn
            bnextshapecolour = stepper firstShapeColour $ brndshapecolour <@ espawn

            bfalling = accumB Nothing $
                             ((>>= Just . fall) <$ etick)
                            `union`
                             (spawn <$> bnextshape <*> bnextshapecolour <@ espawn)
                            `union`
                             (tryChange mvL <$> bfield <@ eleft)
                            `union`
                             (tryChange mvR <$> bfield <@ eright)
                            `union`
                             (tryChange rotCW <$> bfield <@ erotcw)
                            `union`
                             (tryChange rotCCW <$> bfield <@ erotccw)
                            `union`
                             (const Nothing <$ efail)
                where fall = blockMove (0, -1)
                      mvL = blockMove (-1, 0)
                      mvR = blockMove (1, 0)
                      rotCW b  = b {blockRotation = rotateCW $ blockRotation b}
                      rotCCW b = b {blockRotation = rotateCCW $ blockRotation b}
                      tryChange _ _ Nothing  = Nothing
                      tryChange t f (Just b) = Just $ if intersecting b' ((map snd f)++fieldBorder) then b else b'
                        where b' = t b
                      spawn n c = const . Just $  Block n (pt (div fieldW 2) fieldH) RotN c


            bfield = accumB [] $ (updField <$> bfalling <@ ecollide) `union` (const [] <$ enew) `union` (clear <$> eclear)
                where updField Nothing f = f
                      updField (Just b) f = (blockToFieldCol b) ++ f
                      clear rows f = dropLines rows $ filter (not . \(_,p) -> (pointY p `elem` rows)) f
                        where dropLines [] f = f
                              dropLines (r:rows) f = fmap (\(c,p) -> (c, if pointY p > r then pointSub p (pt 0 1) else p)) f'
                                where f' = dropLines rows f

            ecollide = () <$ filterE id (cd <$> bfalling <*> bfield <@ etick)
                where cd Nothing _ = False
                      cd (Just b) f = intersecting (blockMove (0, -1) b) ((map snd f)++fieldBBorder)

            eclear = filterE (not . null) $ f <$> bfalling <*> bfield <@ ecollide
                where f Nothing _ = []
                      f (Just b) field = filter fullLine [(pointY $ blockPos b) - blockDHeight b .. (pointY $ blockPos b) + blockUHeight b]
                        where fullLine n = fieldW == (length $ filter (\p -> (pointY $ p) == n) ((map snd field) ++ blockToField b))

            efail = () <$ (filterE id $ tooHigh <$> bfalling <@ ecollide)
                where tooHigh Nothing = False
                      tooHigh (Just b) = (pointY $ blockPos b) + blockUHeight b >= fieldH

            bscore = accumB 0 $
                 ((\k -> (+) $ k * k) <$> (length <$> eclear))
                `union`
                 (const 0 <$ enew)

            ballcells = (++) <$> blf <*> bfield
                where blf = bltof <$> bfalling
                      bltof Nothing = []
                      bltof (Just b) = blockToFieldCol b

            drawred = stepper False $ (False <$ enew) `union` (True <$ efail)

        erepaint <- changes bfalling
        erepaintnext <- changes bnextshape

        reactimate $ do {firePaint (); repaint p} <$ erepaint
        sink p [on paint :== stepper (\_ _ -> return ()) $ (drawField <$> ballcells <*> drawred) <@ epaint]

        reactimate $ do {firePaintNext (); repaint next} <$ erepaintnext
        sink next [on paint :== stepper (\_ _ -> return ()) $ ((drawNext <$> bnextshape <*> bnextshapecolour) <@ epaintnext) `union` ((\_ _ -> return ()) <$ efail)]

        sink score [text :== ((++) "Score: " . show) <$> bscore]


    controlNet <- compile $ do
        ebnew       <- event0 bNew command
        epause      <- event0 bPause command
        epauseKey   <- event1 bPause keyboard
        epkey       <- event1 p keyboard
        bpauseCh    <- behavior bPause checked

        let
            ekeyp = () <$ filterE ((== KeyChar 'p') . keyKey) (epkey `union` epauseKey)

            bpaused = accumB True $
                (const <$> bpauseCh <@ epause) `union` (not <$ ekeyp) `union` (const False <$ ebnew)

        sink p [enabled :== not <$> bpaused]
        sink bPause [enabled :== stepper False $ True <$ ebnew, checked :== bpaused]

        let reactuate n = do {pause n; actuate n} -- uh =(

        let stSw pe = do
                        if pe then
                            do focusOn p
                               reactuate logicsNet
                        else
                            do pause logicsNet
                               focusOn bPause

        epaused <- changes bpaused
        reactimate $ stSw <$> bpaused <@ epaused
        reactimate $ do {reactuate logicsNet; fireNew (); focusOn p} <$ ebnew

    actuate controlNet

shapePoints :: Shape -> [Position]
shapePoints = map (uncurry pt) . shapeCells

type Position = Point2 Int

data Rotation = RotN | RotE | RotS | RotW
rotateCW RotN = RotE
rotateCW RotE = RotS
rotateCW RotS = RotW
rotateCW RotW = RotN
rotateCCW RotE = RotN
rotateCCW RotS = RotE
rotateCCW RotW = RotS
rotateCCW RotN = RotW

data Block = Block {blockShape :: Shape, blockPos :: Position, blockRotation :: Rotation, blockColour :: Color}
blockToField (Block s p r _) = (pointAdd p) <$> fmap rot (shapePoints s)
    where rot c = let (x,y) = (pointX c, pointY c) in
                   uncurry pt $ case r of
                                    RotN -> (x,y)
                                    RotE -> (y,-x)
                                    RotS -> (-x,-y)
                                    RotW -> (-y,x)

blockToFieldCol b = fmap ((,) $ blockColour b) $ blockToField b

blockUHeight (Block s _ r _) = (case r of
                                  RotN -> shapeUpL
                                  RotE -> shapeLeftL
                                  RotS -> shapeDownL
                                  RotW -> shapeRightL
                               ) s
blockDHeight b = blockUHeight $ b {blockRotation = rotateCW . rotateCW $ blockRotation b}

blockMove (dx,dy) b = b {blockPos = pointAdd (pt dx dy) $ blockPos b}

intersecting b f = not . null $ L.intersect (blockToField b) f

fieldBBorder = uncurry pt <$>
    fmap (flip (,) $ 0) [1..fieldW]
fieldLBorder = uncurry pt <$>
    fmap ((,) 0) [1..fieldH]
fieldRBorder = uncurry pt <$>
    fmap ((,) (fieldW+1)) [1..fieldH]
fieldBorder = fieldLBorder ++ fieldRBorder

drawField field drawRed dc rect = do
    set dc [brushColor := red, brushKind := BrushSolid]
    mapM_ (\(cellCol, r) -> drawRect dc r $ if drawRed then [] else [brushColor := cellCol]) (map (second cellToRect) field)

drawNext shape col dc rect = do
    set dc [brushColor := col, brushKind := BrushSolid]
    mapM_ (\r -> drawRect dc r []) (nextShapeToRects shape)

cellToRect :: Position -> Rect
cellToRect p = let
                 x = (pointX p) - 1
                 y = fieldH - (pointY p)
                in
                 rect (point (x * cellSize) (y * cellSize)) (sz (cellSize-1) (cellSize-1))

-- Preview window works completely different, so drawing shapes in it is a bit complicated.
nextShapeToRects :: Shape -> [Rect]
nextShapeToRects s = map (flip rect (sz (cellSize-1) (cellSize-1))
                         . pointAdd (pt (div (nextWpx - rectW) 2) (div (nextHpx - rectH) 2))
                         . (flip pointScale cellSize)
                         . (pointAdd $ pt (shapeLeftL s) (shapeUpL s))
                         ) . shapePoints . shapeMirrorV $ s
    where rectW = (shapeWidth s) * cellSize
          rectH = (shapeHeight s) * cellSize
