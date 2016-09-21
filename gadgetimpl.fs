module GadgetImpl

open Util
open CSS
open Html
open Panel
open Gadget
open Measure
open Input

type Panel = Panel.Panel
type RenderMsg = Measure.RenderMsg

type FlexDirection =
  | FlexColumn
  | FlexRow

let updateWithGadgets panel =
  List.fold
    (fun (p : Panel) (g : Gadget<Panel,RenderMsg>) -> g.modify p)
    panel
    panel.layout

let stringOfFlexDirection fd =
  match fd with
  | FlexColumn -> "column"
  | FlexRow -> "row"

let flexDirectionOfString fds =
  match fds with
  | "row" -> FlexRow
  | _ -> FlexColumn

let layoutMgrView editors (html : Gadget.Msg Html) panel =
  html.div
    []
    []
    (Input.map 
       (fun n ed -> 
         html.div 
           [html.className "control-row-gang"] []
           [
             html.div
               [html.className "control-row-label"] []
               [
                 html.text n ;
                 ed.view (Html.map (fun m -> InputMsg m) html) n
               ]
           ]
       ) 
       editors
    )

let layoutMgrInputUpdate m editors' lm f =
  let updatedEditors = Input.update m editors' in
    List.fold
      (fun lm ((name,editor) : (string * EditorInstance)) ->
        let cv = editor.currentValue () in
        f lm updatedEditors name cv
      )
      lm
      (Input.map Util.tuple2 updatedEditors)

let layoutMgrUpdate msg editors lm upd =
  match msg with
  | InputMsg m ->
     let result = 
       layoutMgrInputUpdate m editors lm upd
     in
     result :> Gadget<Panel, RenderMsg>
      | _ -> lm :> Gadget<Panel, RenderMsg>

let createSelector name value vlist =
  new InputSelector(
        value,
        (Set<string> vlist)
      )

let createTextEditor name value =
  new InputEditor(
        value,
        (fun value -> true),
        ("numeric-input-good", "numeric-input-good")
      )

let createNumEditor name value =
  new InputEditor(
        value, 
        (fun value -> (Util.parseFloat value) <> None), 
        ("numeric-input-good", "numeric-input-bad")
      )

type FlexGadget(flexDirection : FlexDirection, editors' : EditorSet option) =
  let editors = 
    editors'
    |> Util.maybeWithDefault
         (Input.init ()
          |> Input.create "flex-direction"
                          (createSelector 
                             "flex-direction" 
                             (stringOfFlexDirection flexDirection) 
                             ["row" ; "column"]
                          )
         )
  in
  interface Gadget<Panel, RenderMsg> with
    member self.name () = "Flex Layout"
    member self.childStyles idx panel =
      [ ("display", "flex") ]
    member self.parentStyles panel =
      [ ("display", "flex") ;
        ("flex-direction", stringOfFlexDirection flexDirection)
      ]
    member self.modify panel = panel
    member self.view html panel = 
      layoutMgrView editors html panel
    member self.update msg = 
      layoutMgrUpdate msg editors self
         (fun lm e n v ->
           match n with
           | "flex-direction" -> 
              new FlexGadget(flexDirectionOfString v, Some e)
           | _ -> self
         )
    member self.serialize panel =
      SerializeData.map 
        [ ("type", SerializeData.string "FlexGadget") ;
          ("flex-direction", 
           SerializeData.string (stringOfFlexDirection flexDirection))
        ]

type TextGadget(text : string, editors' : EditorSet option) =
  let editors = 
    editors'
    |> Util.maybeWithDefault
         (Input.init ()
          |> Input.create "text" (createTextEditor "text" text)
         )
  in
  interface Gadget<Panel, RenderMsg> with
    member self.name () = "Text"
    member self.childStyles idx panel = []
    member self.parentStyles panel = []
    member self.modify panel = { panel with text = text }
    member self.view html panel = 
      layoutMgrView editors html panel
    member self.update msg =
      layoutMgrUpdate msg editors self
         (fun lm e n v ->
           match n with
           | "text" -> 
              new TextGadget(v, Some e)
           | _ -> self
         )
    member self.serialize panel =
      SerializeData.map 
        [ ("type", SerializeData.string "TextGadget") ;
          ("text", SerializeData.string text)
        ]

type SidebarGadget(split : float, editors' : EditorSet option) =
  let editors =
    editors'
    |> Util.maybeWithDefault
         (Input.init ()
          |> Input.create "split" (createNumEditor "split" (Util.toString split))
         )
  in
  interface Gadget<Panel, RenderMsg> with
    member self.name () = "Sidebar"
    member self.childStyles idx panel =
      if idx = 0 then
        [ ("display", "flex") ;
          ("flex-shrink", "0.8") ;
          ("flex-grow", "0.8")
        ]
      else
        [ ("display", "flex") ;
          ("flex-shrink", "0.2") ;
          ("flex-grow", "0.2")
        ]
    member self.parentStyles panel = []
    member self.modify panel =
      { panel with
        children =
          match panel.children with
          | [] -> 
             [ 
               { panel with 
                 id = Util.genId () ;
                 text = "" ;
                 children = [] ;
                 layout = []
               } ;
               { panel with
                 id = Util.genId () ;
                 text = "" ;
                 children = [] ;
                 layout = []
               }
             ]
          | hd :: [] ->
             [
               hd ;
               { panel with
                 id = Util.genId () ;
                 text = "" ;
                 children = [] ;
                 layout = []
               }               
             ]
          | a :: b :: tl ->
             [ a ; b ]
      }
    member self.view html panel = 
      layoutMgrView editors html panel
    member self.update msg =
      layoutMgrUpdate msg editors self
         (fun lm e n v ->
           match (n, Util.parseFloat v) with
           | ("split", Some f) -> 
              new SidebarGadget(f, Some e)
           | _ -> self
         )
    member self.serialize panel =
      SerializeData.map 
        [ ("type", SerializeData.string "SidebarGadget") ;
          ("split", SerializeData.num split)
        ]

type HeaderGadget(split : float, editors' : EditorSet option) =
  let editors =
    editors'
    |> Util.maybeWithDefault
         (Input.init ()
          |> Input.create "split" (createNumEditor "split" (Util.toString split))
         )
  in
  interface Gadget<Panel, RenderMsg> with
    member self.name () = "Header"
    member self.childStyles idx panel =
      if idx = 0 then
        [ ("display", "flex") ;
          ("flex-shrink", "0.8") ;
          ("flex-grow", "0.8")
        ]
      else
        [ ("display", "flex") ;
          ("flex-shrink", "0.2") ;
          ("flex-grow", "0.2")
        ]
    member self.parentStyles panel = []
    member self.modify panel =
      { panel with
        children =
          match panel.children with
          | [] -> 
             [ 
               { panel with 
                 id = Util.genId () ;
                 text = "" ;
                 children = [] ;
                 layout = []
               } ;
               { panel with
                 id = Util.genId () ;
                 text = "" ;
                 children = [] ;
                 layout = []
               }
             ]
          | hd :: [] ->
             [
               hd ;
               { panel with
                 id = Util.genId () ;
                 text = "" ;
                 children = [] ;
                 layout = []
               }               
             ]
          | a :: b :: tl ->
             [ a ; b ]
      }
    member self.view html panel = 
      layoutMgrView editors html panel
    member self.update msg =
      layoutMgrUpdate msg editors self
         (fun lm e n v ->
           match (n, Util.parseFloat v) with
           | ("split", Some f) -> 
              new HeaderGadget(f, Some e)
           | _ -> self
         )
    member self.serialize panel =
      SerializeData.map 
        [ ("type", SerializeData.string "HeaderGadget") ;
          ("split", SerializeData.num split)
        ]

type PaddingGadget(padding : float, editors' : EditorSet option) =
  let editors =
    editors'
    |> Util.maybeWithDefault
         (Input.init ()
          |> Input.create "padding" (createNumEditor "padding" (Util.toString padding))
         )
  in
  interface Gadget<Panel, RenderMsg> with
    member self.name () = "Padding"
    member self.childStyles idx panel = []
    member self.parentStyles panel =
      [
        ("padding", String.concat "" [Util.toString padding; "px"])
      ]
    member self.modify panel =
      panel
    member self.view html panel = 
      layoutMgrView editors html panel
    member self.update msg =
      layoutMgrUpdate msg editors self
         (fun lm e n v ->
           match (n, Util.parseFloat v) with
           | ("padding", Some f) -> 
              new PaddingGadget(f, Some e)
           | _ -> self
         )
    member self.serialize panel =
      SerializeData.map 
        [ ("type", SerializeData.string "PaddingGadget") ;
          ("padding", SerializeData.num padding)
        ]
