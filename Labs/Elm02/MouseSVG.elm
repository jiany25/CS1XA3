module MouseSVG exposing (..)

import Html as Html
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Mouse as Mouse
import Svg exposing (..)
import Svg.Attributes exposing (..)

main : Program Never Model Msg
main = Html.program
          { init = init,
            view = view,
            update = update,
            subscriptions = subscriptions
            }

type alias Model = { position :  { x : Int, y : Int } }

init : (Model,Cmd.Cmd Msg)
init = ({ position = {x = 300, y = 300}},Cmd.none)

type Msg = MouseMsg Mouse.Position

update : Msg -> Model -> (Model,Cmd.Cmd Msg)
update (MouseMsg pos) model = ({ position = {x = pos.x, y=pos.y} },Cmd.none)

view : Model -> Html.Html Msg
view model = let
      posX = toString model.position.x
      posY = toString model.position.y        
  in svg [width "600", height "600"]
   [ellipse [cx posX, cy posY, rx "20", ry "10", fill "blue"][]]

subscriptions : Model -> Sub Msg
subscriptions model =  Mouse.moves MouseMsg