-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main (main) where
-----------------------------------------------------------------------------
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Miso hiding ((!!))
import           Miso.Html
import           Miso.Html.Property
import qualified Miso.CSS as CSS
-----------------------------------------------------------------------------
histCap :: Int
histCap = 200
-----------------------------------------------------------------------------
data Action
  = GetArrows !Arrows
  | Time !Double
  | Start
  | TogglePause
  | Scrub !Int
-----------------------------------------------------------------------------
spriteFrames :: [MisoString]
spriteFrames =
  [ "0 0"
  , "-74px 0"
  , "-111px 0"
  , "-148px 0"
  , "-185px 0"
  , "-222px 0"
  , "-259px 0"
  , "-296px 0"
  ]
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
data Mario = Mario
  { x, y       :: !Double
  , vx, vy     :: !Double
  , time, delta :: !Double
  , dir         :: !Direction
  , arrows      :: !Arrows
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
data Direction = L | R deriving (Show, Eq)
-----------------------------------------------------------------------------
defaultMario :: Mario
defaultMario = Mario
  { x = 0, y = 0, vx = 0, vy = 0
  , dir = R, time = 0, delta = 0
  , arrows = Arrows 0 0
  }
-----------------------------------------------------------------------------
data Model = Model
  { marioState :: !Mario
  , paused     :: !Bool
  , history    :: !(Seq (Int, Int))
  , sliderPos  :: !Int
  , pauseMario :: !Mario
  } deriving (Eq)
-----------------------------------------------------------------------------
main :: IO ()
main = do
  t <- now
  let mario0 = defaultMario { time = t }
      m = Model
        { marioState = mario0
        , paused     = False
        , history    = Seq.empty
        , sliderPos  = 0
        , pauseMario = mario0
        }
  startApp mempty (component m updateModel viewModel)
    { subs = [ arrowsSub GetArrows, rAFSub Time ] }
-----------------------------------------------------------------------------
updateModel :: Action -> Effect parent props Model Action
updateModel Start = pure ()
updateModel TogglePause = modify $ \m ->
  if paused m
  then let mario' = (pauseMario m) { delta = 0 }
           n      = Seq.length (history m)
       in m { paused = False, marioState = mario', sliderPos = max 0 (n - 1) }
  else m { paused = True, pauseMario = marioState m }
updateModel (Scrub i) = modify $ \m ->
  let hist = history m
      n    = Seq.length hist
  in if n == 0 || not (paused m)
     then m
     else let i'       = max 0 (min i (n - 1))
              (px, py) = Seq.index hist i'
              mario'   = (marioState m) { x = fromIntegral px, y = fromIntegral py }
          in m { sliderPos = i', marioState = mario' }
updateModel (GetArrows arrs) = do
  m <- get
  if paused m then pure () else do
    modify $ \mdl ->
      let m0 = (marioState mdl) { arrows = arrs }
      in mdl { marioState = m0 }
    stepAndRecord
updateModel (Time newTime) = do
  m <- get
  if paused m then pure () else do
    modify $ \mdl ->
      let m0 = marioState mdl
          m1 = m0 { delta = (newTime - time m0) / 20, time = newTime }
      in mdl { marioState = m1 }
    stepAndRecord
-----------------------------------------------------------------------------
stepAndRecord :: Effect parent props Model Action
stepAndRecord = do
  modify $ \mdl ->
    let m    = marioState mdl
        m'   = physics (delta m)
               . walk (arrows m)
               . jump (arrows m)
               . gravity (delta m)
               $ m
        ix   = round (x m') :: Int
        iy   = round (y m') :: Int
        hist = history mdl
        changed = case Seq.viewr hist of
          Seq.EmptyR       -> True
          _ Seq.:> (px,py) -> px /= ix || py /= iy
        hist' = if changed
                then let h = hist Seq.|> (ix, iy)
                     in if Seq.length h > histCap then Seq.drop 1 h else h
                else hist
        sp = if changed then Seq.length hist' - 1 else sliderPos mdl
    in mdl { marioState = m', history = hist', sliderPos = sp }
-----------------------------------------------------------------------------
gravity :: Double -> Mario -> Mario
gravity dt m@Mario{..} = m { vy = if y > 0 then vy - (dt / 4) else 0 }
-----------------------------------------------------------------------------
jump :: Arrows -> Mario -> Mario
jump Arrows{..} m@Mario{..}
  | arrowY > 0 && vy == 0 = m { vy = 6 }
  | otherwise = m
-----------------------------------------------------------------------------
walk :: Arrows -> Mario -> Mario
walk Arrows{..} m@Mario{..}
  = m
  { vx = fromIntegral arrowX
  , dir = if | arrowX < 0 -> L
             | arrowX > 0 -> R
             | otherwise  -> dir
  }
-----------------------------------------------------------------------------
physics :: Double -> Mario -> Mario
physics dt m@Mario{..} = m { x = x + dt * vx, y = max 0 (y + dt * vy) }
-----------------------------------------------------------------------------
viewModel :: props -> Model -> View model Action
viewModel _ m =
  let mario' = marioState m
      n      = Seq.length (history m)
      groundY = -400
  in div_
     [ CSS.style_
         [ CSS.display "flex"
         , CSS.flexDirection "column"
         , CSS.alignItems "center"
         , CSS.fontFamily "sans-serif"
         ]
     ]
     [ style_ [] "@keyframes play { 100% { background-position: -296px; } }"
     , div_ [ CSS.style_ (marioStyle mario' groundY) ] []
     , div_
         [ CSS.style_
             [ CSS.display "flex"
             , CSS.gap "8px"
             , CSS.alignItems "center"
             , CSS.marginTop "8px"
             ]
         ]
         [ button_ [ onClick TogglePause ]
             [ text (if paused m then "Unpause" else "Pause") ]
         , input_
             ([ type_ "range"
              , min_ "0"
              , max_ (toMisoString (max 0 (n - 1)))
              , value_ (toMisoString (sliderPos m))
              , onInput (\v -> Scrub (fromMisoString v))
              ] ++ [ disabled_ | not (paused m) ])
         , text (toMisoString (sliderPos m) <> " / " <> toMisoString (max 0 (n - 1)))
         ]
     ]
-----------------------------------------------------------------------------
marioStyle :: Mario -> Double -> [CSS.Style]
marioStyle Mario{..} gy =
  [ CSS.transform $ matrix dir x $ abs (y + gy)
  , CSS.display "block"
  , CSS.width (CSS.px 37)
  , CSS.height (CSS.px 37)
  , CSS.backgroundColor CSS.transparent
  , CSS.backgroundImage (CSS.url "assets/mario.png")
  , CSS.backgroundRepeat "no-repeat"
  , CSS.backgroundPosition (spriteFrames !! frame)
  ] ++
  [ CSS.animation "play 0.8s steps(8) infinite"
  | y == 0 && vx /= 0
  ] where
      frame
        | y > 0    = 1
        | otherwise = 0
-----------------------------------------------------------------------------
matrix :: Direction -> Double -> Double -> MisoString
matrix dir x y = CSS.matrix (if dir == L then -1 else 1) 0 0 1 x y
-----------------------------------------------------------------------------
