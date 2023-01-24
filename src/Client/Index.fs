module Index

open System.Text
open Elmish
open Fable.Core.JS
open Fable.Core.JsInterop
open Feliz
open Feliz.Bulma
open Feliz.style

type Annotation = {Name: string; colour: string; position: int; length: int}
type Model = { RawData: byte array; Annotations: Annotation list}

type Msg =
    | LoadData of ArrayBuffer


let init () : Model * Cmd<Msg> =
    let model = { RawData = [||]; Annotations = []}

    model, Cmd.none

let convertToHex (rawData: byte array) =
    let builder = StringBuilder(rawData.Length * 2 + rawData.Length / 8 * 3)
    for x in 0..rawData.Length do
        builder.AppendFormat("{0:x2}",rawData[x]) |> ignore
        match x % 0x10 with
        | 0xF -> builder.Append "\r\n" |> ignore
        | 0x7 -> builder.Append " " |> ignore
        | _ -> builder.Append " " |> ignore
    builder.ToString()


let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | LoadData data ->
        {model with RawData = Array.from(Uint8Array.Create(data))},Cmd.none


let handleFileEvent onLoad (fileEvent:Browser.Types.Event) =
    let files:Browser.Types.FileList = !!fileEvent.target?files
    if files.length > 0 then
        let reader = Browser.Dom.FileReader.Create()
        reader.onload <- (fun _ -> reader.result |> unbox |> onLoad)
        reader.readAsArrayBuffer(files[0])

let createFileUpload onLoad =
    Bulma.file [
        Bulma.fileLabel.label [
            Bulma.fileInput [
                prop.onChange (fun (e:Browser.Types.Event) -> handleFileEvent onLoad e)
            ]
            Bulma.fileCta [
                Bulma.fileLabel.label "Choose a file..."
            ]
        ]
    ]
let view (model: Model) (dispatch: Msg -> unit) =
    let currentPadding = 1
    let tdStyle =
        List.append [
            style.padding currentPadding
        ]
    Html.table [
        Html.td [
            prop.width 10
            prop.style [
                style.borderColor "red"
                style.borderWidth 2
                style.borderStyle.solid
                style.width 50
            ]
            prop.children [
                createFileUpload (LoadData >> dispatch)
            ]
        ]
        Html.td [
            prop.style [
                style.borderColor "red"
                style.borderWidth 2
                style.borderStyle.solid
                style.fontFamily "monospace"
            ]
            prop.children [
                Html.table [
                    prop.style [
                        style.backgroundColor "#2020C0"
                        style.color "#C0C0C0"
                    ]
                    prop.children [
                        Html.thead [
                            Html.th []
                            for i in 0..0xF do
                                Html.th [
                                    prop.style <| tdStyle [style.userSelect.none; style.color "#C0C0C0"]
                                    prop.text (i.ToString("X2"))
                                ]
                        ]
                        Html.tbody [
                        for line in 0..(model.RawData.Length/16) do
                            Html.tr [

                                prop.children [
                                    Html.td [
                                        prop.style <| tdStyle [style.userSelect.none; style.backgroundColor (if line % 2 = 0 then "#202060" else "#2020C0")]
                                        prop.text ((line * 0x10).ToString("X10"))
                                    ]
                                    for value in model.RawData[line * 0x10..line * 0x10 + 0xF] do
                                        Html.td [
                                            prop.style <| tdStyle [style.backgroundColor (if line % 2 = 0 then "#202060" else "#2020C0")]
                                            prop.text (value.ToString("X2"))
                                        ]
                                    ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
        Bulma.column [
        ]
    ]