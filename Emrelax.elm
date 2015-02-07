import Graphics.Element (..)
import Signal (Signal)
import Text
import Time
import Window
import List (head, tail, (::))

-- Models
type alias Node = {charge:Float}
type alias Zipper = {front:List Node, focus:Node, back:List Node}

mvForward : Zipper -> Zipper
mvForward x =
  let
    focus' = head x.front
    front' = tail x.front
    back' = x.focus::x.back
  in
    {front = front', focus = focus', back = back'}

mvBack : Zipper -> Zipper
mvBack x =
  let
    focus' = head x.back
    front' = x.focus::x.front
    back' = tail x.back
  in
    {front = front', focus = focus', back = back'}
