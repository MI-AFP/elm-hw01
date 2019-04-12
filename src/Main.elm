module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, a, button, div, h1, p, text)
import Html.Attributes exposing (href, style, target)
import Html.Events exposing (onClick)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type Msg
    = NoOp


type alias Model =
    {}


init : Model
init =
    {}


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    div pageStyle
        [ h1 [] [ text "Sunny Philadelphia" ]
        , p subheadlineStyle
            [ text "A short story generated by "
            , a [ href "https://www.plot-generator.org.uk/story/", target "_blank" ] [ text "Plot Generator" ]
            , text "."
            ]
        , div tabsStyle
            [ a (tabStyle ++ tabStyleActive ++ []) [ text "Introduction" ]
            , a (tabStyle ++ []) [ text "Plot" ]
            , a (tabStyle ++ []) [ text "Conclusion" ]
            ]
        , div []
            [ introductionContent ]
        ]


introductionContent : Html Msg
introductionContent =
    div []
        [ p [] [ text "Jenny Giantbulb had always loved sunny Philadelphia with its frightened, fragile fields. It was a place where she felt relaxed." ]
        , p [] [ text "She was a noble, predatory, tea drinker with red lips and fragile feet. Her friends saw her as a hungry, hilarious hero. Once, she had even helped a puny kitten recover from a flying accident. That's the sort of woman he was." ]
        ]


plotContent : Html Msg
plotContent =
    div []
        [ p [] [ text "Jenny walked over to the window and reflected on her idyllic surroundings. The sun shone like walking horses." ]
        , p [] [ text "Then she saw something in the distance, or rather someone. It was the figure of John Thunder. John was an optimistic giant with beautiful lips and short feet." ]
        , p [] [ text "Jenny gulped. She was not prepared for John." ]
        , p [] [ text "As Jenny stepped outside and John came closer, she could see the super glint in his eye." ]
        , p [] [ text "John gazed with the affection of 9175 spiteful broad bears. He said, in hushed tones, \"I love you and I want justice.\"" ]
        , p [] [ text "Jenny looked back, even more cross and still fingering the squidgy newspaper. \"John, hands up or I'll shoot,\" she replied." ]
        ]


conclusionContent : Html Msg
conclusionContent =
    div []
        [ p [] [ text "They looked at each other with surprised feelings, like two faffdorking, faithful frogs cooking at a very cowardly Halloween party, which had piano music playing in the background and two stingy uncles bouncing to the beat." ]
        , p [] [ text "Jenny regarded John's beautiful lips and short feet. \"I feel the same way!\" revealed Jenny with a delighted grin." ]
        , p [] [ text "John looked irritable, his emotions blushing like a sleepy, squashed sausage." ]
        , p [] [ text "Then John came inside for a nice cup of tea." ]
        ]


pageStyle : List (Attribute Msg)
pageStyle =
    [ style "width" "45rem"
    , style "margin" "auto"
    ]


subheadlineStyle : List (Attribute Msg)
subheadlineStyle =
    [ style "font-style" "italic"
    ]


tabsStyle : List (Attribute Msg)
tabsStyle =
    [ style "display" "flex"
    , style "width" "100%"
    , style "border-bottom" "1px solid #000"
    ]


tabStyle : List (Attribute Msg)
tabStyle =
    [ style "flex-grow" "1"
    , style "text-align" "center"
    , style "cursor" "pointer"
    , style "padding" "0.5rem"
    , style "color" "blue"
    ]


tabStyleActive : List (Attribute Msg)
tabStyleActive =
    [ style "font-weight" "bold"
    , style "color" "black"
    ]
