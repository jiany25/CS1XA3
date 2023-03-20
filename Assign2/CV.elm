
import Html exposing (..)
import Html.Attributes exposing (..)

headerStyle = style [("text-align","center"),
                     ("background-image","url(\"back.jpg\")"),
                     ("background-position","center"),
                     ("background-repeat","no-repeat"),
                     ("width","100%")
                     ]
sectionStyle = style [ ("width","60%"),
                  ("text-align", "center"),
                  ("float","left")
                  ]
asideStyle = style [("border-left","1px solid lightgrey"),
                    ("width","35%")
                      ]
sideStyle1 = style [("font-size", "75%"),
                    ("margin","0"),
                    ("text-indent","10%")]
sideStyle2 = style [("color", "rgb(65,154,188)"),
                    ("margin","0"),
                    ("text-indent","10%")]
mainStyle = style [("color", "rgb(65,154,188)"),
                   ("font-family","Times New Roman")]
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
                 img [src "face.jpg",style[("height","150px"),
                                             ("width","150px")]] [],
                 h1 [style[("color","white")]] [text "Yifan Jiang"],
                 b  [style[("color","rgb(50,50,50)"),
                            ("font-family","Times New Roman")]] [text "Hamilton, L8R1E2, Canada"]
                ],
         p [photoStyle1] [a [href "https://unsplash.com/photos/oMneOBYhJxY",photoStyle2] [text "Photo by John Lee/ Unsplash"]],
         div [style[("display","flex")]]
        [
         section[style[("width","5%")]] [br[][]],
         section [sectionStyle] 
                     [br [] [],
                      img [src "education.jpg"] [],
                      h1 [] [text "Ontario Secondary School Diploma,"],
                      h1 [] [text "Bond International College, Toronto"],
                      b [mainStyle] [text "September 2016 - June 2017"],
                      h1 [] [text "Math & Statistics First Year"],
                      h1 [] [text "Mcmaster University, Hamiltion"],
                      b [mainStyle] [text "September 2017 - April 2018"],
                      br [] [],
                      br [] [],
                      br [] [],
                      img [src "activities.jpg"] [],
                      h1 [] [text "Mrs. Margareta's Physics Club"],
                      b [mainStyle] [text "January 2017 - June 2017"],
                      br [] [],
                      br [] [],
                      br [] []--,
                      --h1 [style[("color","SlateBlue")]] [text "↓GO TO MY WEB APP↓"],
                      --a [href "MouseSVG.html"] [text "Assignment 2: Web App"]
                      ],
        aside [asideStyle]
                [br [] [],
                 h1 [style[("color","gray"),
                            ("font-family","Times New Roman"),
                            ("text-indent","5%")]] [text "Details"],
                 p [style[("color","rgb(65,154,188)"),
                          ("margin","0"),
                          ("text-indent","10%"),
                          ("font-size","75%")]] [text "  jiany25@mcmaster.ca"], 
                 img [src "skills.jpg"] [],
                 p [sideStyle1] [text "Java Basics"],
                 p [sideStyle2] [text "● ○ ○ ○ ○"],
                 p [sideStyle1] [text "Haskell Basics"],
                 p [sideStyle2] [text "● ○ ○ ○ ○"],
                 p [sideStyle1] [text "Bash Basics"],
                 p [sideStyle2] [text "● ○ ○ ○ ○"],
                 p [sideStyle1] [text "Elm Basics"],
                 p [sideStyle2] [text "● ○ ○ ○ ○"],
                 img [src "language.jpg"] [],
                 p [sideStyle1] [text "English"],
                 p [sideStyle2] [text "● ○ ○ ○ ○"],
                 p [sideStyle1] [text "Chinese"],
                 p [sideStyle2] [text "● ● ● ● ●"],
                 img [src "hobbies.jpg"] [],
                 p [sideStyle1] [text "Literatures"],
                 p [sideStyle1] [text "Calligraphy"],
                 p [sideStyle1] [text "Movies"],
                 p [sideStyle1] [text "Gaming"]
                 ]
                 ],
                 br [] [],
                 br [] [],
                 br [] [],
                 br [] [],
                 br [] [],
                  p [style[("color","grey"),
                  ("text-align","center"),
                  ("font-size","50%")]] [text "Copyright © 2018 Yifan Jiang, All rights reserved"]
                 ]
                 