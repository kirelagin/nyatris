%include polycode.fmt


Импортирование необходимых модулей.

\begin{code}
import qualified Data.List as L
import Control.Arrow
import Graphics.UI.WX hiding (Event, value)
import Reactive.Banana hiding (empty)
import Reactive.Banana.WX

import Nyatris.Shape
import Nyatris.Random
\end{code}


Размеры клеток, составляющих фигуры, игрового поля и окна предпросмотра следующей фигуры.

\begin{code}
cellSize            = 20
(fieldW, fieldH)    = (10, 20)
(nextWpx, nextHpx)  = (cellSize * (maxW+1), cellSize * (maxH+1))
    where  maxW  = maximum $ map shapeWidth shapes
           maxH  = maximum $ map shapeHeight shapes
\end{code}


Всего существует восемь различных тетрамино. Здесь они заданы с помощью графической нотации
(крестик означает заполненную клетку фигуры, нолик — пустую). Каждая падающая фигура
будет случайным образом раскрашена в один из шести перечисленных цветов.

\begin{code}
shapes = tetroI : tetroJ : tetroZ : tetroT : tetroO : tetroL : tetroS :[]
    where
        x  = FilledCell
        o  = EmptyCell

        tetroI  = shape  [[x,x,x,x]]
        tetroJ  = shape  [[o,o,x,o,o]
                         ,[o,o,x,x,x]
                         ,[o,o,o,o,o]]
        tetroZ  = shape  [[x,x,o]
                         ,[o,x,x]
                         ,[o,o,o]]
        tetroT  = shape  [[o,x,o]
                         ,[x,x,x]
                         ,[o,o,o]]
        tetroO  = shape  [[x,x]
                         ,[x,x]]
        tetroL  = shapeMirrorH tetroJ
        tetroS  = shapeMirrorH tetroZ

shapeColours = [cyan, magenta, yellow, green, blue, rgb 139 0 255]
\end{code}


@main@ — основная функция. В ней создаётся пользовательский интерфейс
и описываются реактивные сети, обеспечивающие работу программы.

Всего этих сетей две: @logicsNet@ — сеть логики, реализующая
непосредственно игру, и @controlNet@ — сеть, обеспечивающая возможность
запустить или перезапустить игру, а также поставить её на паузу.

\begin{code}
main = start $ do
\end{code}

Следующий код создаёт графический интерфейс пользователя средствами
библиотеки \emph{wxWidgets}.

\begin{code}
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
            ,column 10 $  [widget score
                          ,minsize (sz (nextWpx + 5) (nextHpx + 5)) $ widget next
                          ,vfill empty
                          ,widget bPause
                          ,widget bNew
                          ]
            ]
          ]
    focusOn bNew
\end{code}

Создание обработчиков событий: нажатие кнопки «Новая игра»,
рисование на игровом поле, рисование в окне предпросмотра следующий фигуры.

\begin{code}
    (handleNew, fireNew)              <- newAddHandler
    (handlePaint, firePaint)          <- newAddHandler
    (handlePaintNext, firePaintNext)  <- newAddHandler
\end{code}

Генераторы случайных последовательностей, определяющих форму и цвет следующей фигуры.

\begin{code}
    --
    -- FIXME: I hate this generator separate from logicsNet
    let rndShapeSrc = chooseRIO shapes
    firstShape <- rndShapeSrc
    let rndShapeColourSrc = chooseRIO shapeColours
    firstShapeColour <- rndShapeColourSrc
\end{code}

К сети логики подключены таймер, генерирующий событие каждые 500 миллисекунд,
клавиатура, а также созданные выше обработчики событий (начало игры и рисование) и
генераторы случайных последовательностей.

\begin{code}
    logicsNet  <- compile $ do
        etimer           <- event0 t command
        ekey             <- event1 p keyboard
        enew             <- fromAddHandler handleNew
        epaint           <- fromAddHandler handlePaint
        epaintnext       <- fromAddHandler handlePaintNext
        brndshape        <- fromPoll rndShapeSrc
        brndshapecolour  <- fromPoll rndShapeColourSrc
\end{code}

С помощью клавиатуры падающую фигуру можно двигать по полю влево и вправо,
вращать, усорять её падение.

\begin{code}
        let
            eleft    = () <$ filterE ((`elem` p) . keyKey) ekey
                        where p = [KeyLeft, KeyChar 'a']
            eright   = () <$ filterE ((`elem` p) . keyKey) ekey
                        where p = [KeyRight, KeyChar 'd']
            erotcw   = () <$ filterE ((`elem` p) . keyKey) ekey
                        where p = [KeyUp, KeyChar 'e']
            erotccw  = () <$ filterE ((`elem` p) . keyKey) ekey
                        where p = [KeyDown, KeyChar 'q']
            espace   = () <$ filterE ((`elem` p) . keyKey) ekey
                        where p = [KeySpace, KeyChar 's']
            eret     = () <$ filterE ((`elem` p) . keyKey) ekey
                        where p = [KeyReturn]
\end{code}

@etick@ — событие, происходящее при «тике» таймера, либо при нажатии клавиши
«пробел». Таким образом с помощью клавиши «пробле» можно ускорять падение фигуры.

\begin{code}
            etick = etimer `union` espace
\end{code}

Событие @espawn@ происходит когда необходимо создать новую падающую фигуру,
а именно при начале новой игры, либо при столкновении падающей фигуры с игровым полем.

\begin{code}
            espawn = enew `union` ecollide
\end{code}

Сигнал @bnextshape@ задаёт форму следующей фигуры. Он представляет собой
кусочно-постоянную функцию, значение которой при событии @espawn@ заменяется на
значение сигнала @brndshape@ (подключенного к генератору случайной последовательности)
на момент события.

\begin{code}
            bnextshape = stepper firstShape $ brndshape <@ espawn
\end{code}

Аналогично устроен сигнал @bnextshapecolour@, определяющий цвет следующей фигуры.

\begin{code}
            bnextshapecolour = stepper firstShapeColour $ brndshapecolour <@ espawn
\end{code}

Сигнал @bfalling@ описывает падающую на данный момент фигуру. Она сдвигается вниз на один блок
по событию @etick@, перемещается влево, либо вправо, а также вращается по событиям клавиатуры
@eleft@, @eright@, @erotcw@, @erotccw@. Если происходит событие @efail@, падающая фигура
исчезает, и появляется при событии @espawn@.

\begin{code}
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
                where  fall = blockMove (0, -1)
                       mvL = blockMove (-1, 0)
                       mvR = blockMove (1, 0)
                       rotCW b  = b {blockRotation = rotateCW $ blockRotation b}
                       rotCCW b = b {blockRotation = rotateCCW $ blockRotation b}
                       tryChange _ _ Nothing  = Nothing
                       tryChange t f (Just b) = Just $  if intersecting b' ((map snd f)++fieldBorder)
                                                        then b
                                                        else b'
                         where b' = t b
                       spawn n c = const . Just $  Block n (pt (div fieldW 2) fieldH) RotN c
\end{code}

Сигнал @bfield@ описывает уже упавшие ранее блоки. Изначально он представляет собой пустой список.
Затем в этот список при столкновении (@ecollide@) добавляются блоки падающей фигуры @bfalling@.
Событие @eclear@ удаляет заполнившиеся горизонтальные ряды.
Событие @enew@ очищает поле.

\begin{code}
            bfield =  accumB [] $ (updField <$> bfalling <@ ecollide) `union`
                      (const [] <$ enew) `union` 
                      (clear <$> eclear)
                where  updField Nothing f = f
                       updField (Just b) f = (blockToFieldCol b) ++ f
                       clear rows f = dropLines rows $ filter (not . \(_,p) -> (pointY p `elem` rows)) f
                         where  dropLines [] f = f
                                dropLines (r:rows) f = fmap (\(c,p) -> (c,  if pointY p > r
                                                                            then pointSub p (pt 0 1)
                                                                            else p)) f'
                                  where f' = dropLines rows f
\end{code}

Событие @ecollide@ происходит когда падающая фигура сталкивается с блоками, уже находящимися на поле.

\begin{code}
            ecollide = () <$ filterE id (cd <$> bfalling <*> bfield <@ etick)
                where  cd Nothing _ = False
                       cd (Just b) f = intersecting (blockMove (0, -1) b) ((map snd f)++fieldBBorder)
\end{code}

Событие @eclear@ происходит сразу после события @ecollide@ в том случае, если после столкновения
фигуры с блоками игрового поля хотя бы один ряд оказывается заполнен. Это событие имеет параметр —
номера заполненных рядов.

\begin{code}
            eclear = filterE (not . null) $ f <$> bfalling <*> bfield <@ ecollide
                where  f Nothing _ = []
                       f (Just b) field = filter fullLine  [(pointY $ blockPos b) - blockDHeight b ..
                                                            (pointY $ blockPos b) + blockUHeight b]
                         where fullLine n = fieldW == (length $ filter  (\p -> (pointY $ p) == n)
                                                                        ((map snd field) ++ blockToField b))
\end{code}

Событие @efail@ означает, что «стакан», в который падают фигуры, переполнен.

\begin{code}
            efail = () <$ (filterE id $ tooHigh <$> bfalling <@ ecollide)
                where  tooHigh Nothing = False
                       tooHigh (Just b) = (pointY $ blockPos b) + blockUHeight b >= fieldH
\end{code}

@bscore@ — целочисленный сигнал, содержащий текущий игровой счет. Очки прибавляются при событии
@eclear@. Событие @enew@ сбрасывает счет на ноль.

\begin{code}
            bscore = accumB 0 $
                 ((\k -> (+) $ k * k) <$> (length <$> eclear))
                `union`
                 (const 0 <$ enew)
\end{code}

@ballcells@ — объединение блоков, уже находящихся на игровом поле, и блоков, образующий падающую фигуру.
Этот сигнал используется при отрисовывании поля.

\begin{code}
            ballcells = (++) <$> blf <*> bfield
                where  blf = bltof <$> bfalling
                       bltof Nothing = []
                       bltof (Just b) = blockToFieldCol b
\end{code}

Булев сигнал, определяющий, определяющий, будут ли блоки на поле раскрашены красным цветом. Событие
@enew@ устанавливает значение @False@, событие @efail@ — @True@.

\begin{code}
            drawred = stepper False $ (False <$ enew) `union` (True <$ efail)
\end{code}

События, сигнализирующие о необходимости перерисовать игровое поле и окно предпросмотра следующий фигуры.

\begin{code}
        erepaint <- changes bfalling
        erepaintnext <- changes bnextshape
\end{code}

Подключение выводов сети логики к виджетам библиотеки \emph{wxWidgets}.

\begin{code}
        reactimate $ do {firePaint (); repaint p} <$ erepaint
        sink p [on paint :== stepper  (\_ _ -> return ()) $
                                      (drawField <$> ballcells <*> drawred) <@ epaint]

        reactimate $ do {firePaintNext (); repaint next} <$ erepaintnext
        sink next [on paint  :== stepper  (\_ _ ->
            return ()) $  ((drawNext <$> bnextshape <*> bnextshapecolour) <@ epaintnext)
                          `union`
                          ((\_ _ -> return ()) <$ efail)]

        sink score [text :== ((++) "Score: " . show) <$> bscore]
\end{code}


К сети управления игрой подключена кнопка «Новая игра», галочка «Пауза», а также клавиатуры.

\begin{code}
    controlNet <- compile $ do
        ebnew       <- event0 bNew command
        epause      <- event0 bPause command
        epauseKey   <- event1 bPause keyboard
        epkey       <- event1 p keyboard
        bpauseCh    <- behavior bPause checked
\end{code}

Значение сигнала @bpaused@ изменяется нажатием клавиши «p» на клавиатуре, либо щелчком по галочке
«Пауза».

\begin{code}
        let
            ekeyp = () <$ filterE ((== KeyChar 'p') . keyKey) (epkey `union` epauseKey)

            bpaused = accumB True $
                (const <$> bpauseCh <@ epause) `union` (not <$ ekeyp) `union` (const False <$ ebnew)
\end{code}

В зависимости от значения сигнала @bpaused@ изменяется состояние галочки.

\begin{code}
        sink p [enabled :== not <$> bpaused]
        sink bPause [enabled :== stepper False $ True <$ ebnew, checked :== bpaused]
\end{code}

Изменение сигнала @bpaused@ запускает либо приостанавливает сеть логики.

\begin{code}
        let reactuate n = do {pause n; actuate n} -- uh =(

        let stSw pe = do
                        if pe then
                            do  focusOn p
                                reactuate logicsNet
                        else
                            do  pause logicsNet
                                focusOn bPause

        epaused <- changes bpaused
        reactimate $ stSw <$> bpaused <@ epaused
        reactimate $ do {reactuate logicsNet; fireNew (); focusOn p} <$ ebnew
\end{code}


На этом заканчивается описание реактивных сетей @logicsNet@ и @controlNet@.
Последнее, что делает функция @main@, — запуск управляющей сети.

\begin{code}
    actuate controlNet
\end{code}


\ignore{
\begin{code}
shapePoints :: Shape -> [Position]
shapePoints = map (uncurry pt) . shapeCells

type Position = Point2 Int

data Rotation = RotN | RotE | RotS | RotW
rotateCW RotN   = RotE
rotateCW RotE   = RotS
rotateCW RotS   = RotW
rotateCW RotW   = RotN
rotateCCW RotE  = RotN
rotateCCW RotS  = RotE
rotateCCW RotW  = RotS
rotateCCW RotN  = RotW

data Block = Block {  blockShape :: Shape, blockPos :: Position,
                      blockRotation :: Rotation, blockColour :: Color}
blockToField (Block s p r _) = (pointAdd p) <$> fmap rot (shapePoints s)
    where rot c = let (x,y) = (pointX c, pointY c) in
                   uncurry pt $ case r of
                                    RotN -> (x,y)
                                    RotE -> (y,-x)
                                    RotS -> (-x,-y)
                                    RotW -> (-y,x)

blockToFieldCol b = fmap ((,) $ blockColour b) $ blockToField b

blockUHeight (Block s _ r _) =  (case r of
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
    mapM_ (\(cellCol, r) ->
        drawRect dc r $  if drawRed
                         then []
                         else [brushColor := cellCol]) (map (second cellToRect) field)

drawNext shape col dc rect = do
    set dc [brushColor := col, brushKind := BrushSolid]
    mapM_ (\r -> drawRect dc r []) (nextShapeToRects shape)

cellToRect :: Position -> Rect
cellToRect p =  let
                  x = (pointX p) - 1
                  y = fieldH - (pointY p)
                 in
                  rect (point (x * cellSize) (y * cellSize)) (sz (cellSize-1) (cellSize-1))

-- Preview window works completely different, so drawing shapes in it is a bit complicated.
nextShapeToRects :: Shape -> [Rect]
nextShapeToRects s = map  (flip rect (sz (cellSize-1) (cellSize-1))
                          . pointAdd (pt (div (nextWpx - rectW) 2) (div (nextHpx - rectH) 2))
                          . (flip pointScale cellSize)
                          . (pointAdd $ pt (shapeLeftL s) (shapeUpL s)))
                          . shapePoints . shapeMirrorV $ s
    where  rectW = (shapeWidth s) * cellSize
           rectH = (shapeHeight s) * cellSize
\end{code}
}
