module Sample.Page

open Fulma
open Fable.React
open Fable.FontAwesome

module Utils =

    let loader =
        Hero.hero [ Hero.IsLarge ]
            [ Hero.body [ CustomClass "has-text-centered" ]
                [ Content.content []
                    [ Icon.icon [ Icon.Size IsLarge ]
                        [ Fa.i [ Fa.Solid.Spinner
                                 Fa.Pulse
                                 Fa.Size Fa.Fa3x ][]]
                      br []
                      br []
                      Text.p []
                        [ str "Loading..." ]]]]