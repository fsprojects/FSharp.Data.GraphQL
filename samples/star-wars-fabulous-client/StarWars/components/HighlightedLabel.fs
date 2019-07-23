namespace StarWars

open Xamarin.Forms
open Fabulous.Core
open Fabulous.DynamicViews

type HighlightedLabel() =
    inherit Frame()

[<AutoOpen>]
module DynamicViewsStatusView =
    type Fabulous.DynamicViews.View with
        static member HighlightedLabel (?text, ?textColor, ?backgroundColor) =
            let attrs =
                let label =
                    View.Label(horizontalOptions = LayoutOptions.FillAndExpand,
                               horizontalTextAlignment = TextAlignment.Center,
                               fontSize = 13.,
                               fontAttributes = FontAttributes.Bold,
                               ?text = text,
                               ?textColor = textColor)

                ViewBuilders.BuildFrame(0,
                                        hasShadow = false,
                                        content = label,
                                        cornerRadius = 5.,
                                        padding = Thickness(0., 0., 0., 0.),
                                        ?backgroundColor = backgroundColor)

            let update (prevOpt: ViewElement voption) (source: ViewElement) (target: HighlightedLabel) =
                ViewBuilders.UpdateFrame(prevOpt, source, target)

            ViewElement.Create(HighlightedLabel, update, attrs)