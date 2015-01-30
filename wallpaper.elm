import Text (..)
import Graphics.Element as G
import List as L
import Graphics.Collage (..)
import Time (..)
import Color (..)
import Signal (..)
import Graphics.Input (..)
import Easing as E
import Transform2D as T

type Keys = Tx | Ty | R6 | Nill

keys : Channel Keys
keys = channel Nill

symmetry : Signal Keys
symmetry = subscribe keys

--USE EASING
--customFunc : G.Element -> G.Element -> G.Element
--customFunc a b = G.flow G.down [a,b]

main = map2 view (foldp updating 0 <| fpsWhen 30 <| since (2*second) symmetry) symmetry

updating : Time -> Time -> Time
updating delt total = if delt == 0 then 0 else delt + total

view : Time -> Keys -> G.Element
view t k = if t < 1.7*second then transform t k else transform 0 Nill

transform : Time -> Keys -> G.Element
transform t k = 
    let quant = case k of
            Nill -> 0
            Tx   -> 1
            Ty   -> 1
            R6   -> pi/3
        adjustTime t = case k of
            R6 -> if t < 0.75*second 
                then E.ease E.easeInQuart E.float 0 (quant/2) (0.75*second) t 
                else if (t >= 0.75*second) && (t < 1.5*second)
                    then E.ease E.easeOutQuart E.float (-quant/2) 0 (0.75*second) (t-0.75*second)
                    else 0
            _  -> if t < 1.5*second
                then E.ease E.easeOutQuart E.float 0 (quant) (1.5*second) t
                else 0
        basicLayout = case k of
            Nill -> tiling::borders
            Tx   -> (move ((175/2)*(adjustTime t), 50*(adjustTime t)) tiling)::borders
            Ty   -> (move (0, 100*(adjustTime t)) tiling)::borders
            R6   -> ([tiling] |> groupTransform (T.rotation (adjustTime t)))::borders
    in case t of
        0 -> basicLayout ++ tButtons
            |> collage 900 490
        _ -> basicLayout
            |> collage 900 490

--timeSoFar : Signal Time
--timeSoFar = foldp (+) 0 (fps 20)

--------------------- Graphics Elements and Forms ---------------------

tButtons : List Form
tButtons = 
    [ isoButton (   0,-225) "X Translate"      Tx,
      isoButton (-200,-225) "Y Translate"      Ty,
      isoButton ( 200,-225) "Order 6 Rotation" R6 ]

borders : List Form
borders = 
    [ trimY         |> move (-280,0),
      trimY         |> move ( 280,0),
      trimX         |> move (0,-200),
      trimX         |> move (0, 200),
      blankBordersY |> move ( 385,0),
      blankBordersY |> move (-385,0),
      blankBordersX |> move (0,-405),
      blankBordersX |> move (0, 405) ]

tile : G.Element
tile = G.image 175 200 "Fractal-Plane-Symmetry-Group-051.jpg"

tiling : Form
tiling = move (-59,0) <| toForm <| G.flow G.down <| L.repeat 4 <| G.flow G.right <| L.repeat 6 tile

blankBordersX : Form
blankBordersX = G.spacer 1000 400
    |> G.color white
    |> toForm

blankBordersY : Form
blankBordersY = G.spacer 200 1000
    |> G.color white
    |> toForm

trimX : Form
trimX = G.spacer 800 10
    |> G.color black
    |> toForm

trimY : Form
trimY = G.spacer 10 820
    |> G.color black
    |> toForm

isoButton : (Float, Float) -> String -> Keys -> Form
isoButton (x,y) str k = button (send keys k) str
    |> toForm
    |> move (x,y)



