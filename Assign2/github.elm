import Html exposing (..)
import Html.Attributes exposing (..)

headerStyle = style [("text-align","center"),
                     ("background-image","url(\"back.jpg\")"),
                     ("background-position","center"),
                     ("background-repeat","no-repeat"),
                     ("width","100%")
                     ]
ulStyle = style[ ("list-stlye-type","none"),
                 ("margin","0"),
                 ("padding","0"),
                 ("background-color","rgba(0,0,0,0.4)"),
                 ("position","fixed"),
                 ("width","100%")]
liStyle = style[("float","right"),
                 ("margin","0"),
                 ("padding","0"),
                ("overflow","hidden")]
aStyle = style[ ("display","block"),
                ("color","white"),
                ("text-align","center"),
                ("padding","1px 25px"),
                ("text-decoration","none")]
photoStyle1 = style [("text-align","right"),
                    ("padding","0"),
                    ("margin","0")
                    ]
photoStyle2 = style [("font-size","60%"),
                     ("color","grey")]
gitStyle = style [
                  ("text-align","center"),
                  ("width","100%")]                           
main : Html msg
main = div []
        [
         header [headerStyle]
                [
                 nav [] [ul [ulStyle] [li [liStyle] [a [href "webapp.html",aStyle][text "Project"]],
                               li [liStyle] [a [href "github.html",aStyle][text "Github"]],
                               li [liStyle] [a [href "index.html",aStyle][text "Home"]]
                               ]],
                 br [] [],
                 br [] [],
                 br [] [],
                 br [] [],
                 br [] [],
                 br [] [],
                 br [] [],
                 h1 [style[("color","white")]] [text "My Github"],
                 br [] []
                ],
         p [photoStyle1] [a [href "https://unsplash.com/photos/oMneOBYhJxY",photoStyle2] [text "Photo by John Lee/ Unsplash"]],
         div[gitStyle] [
         h1 [] [text "View my "],
         a [href "https://github.com/jiany25"] [img [src "github.png"][]],
        br [] [],
        br [] [],
        br [] [],
        br [] [],
        br [] []
         ],
         p [style[("color","grey"),
                  ("text-align","center"),
                  ("font-size","50%")]] [text "Copyright © 2018 Yifan Jiang, All rights reserved"]]