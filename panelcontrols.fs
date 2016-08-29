module PanelControls

open Util
open VDom
open Html
open Input
open Panel
open ControlInterface

type Panel = Panel.Panel

type Msg =
  | NoOp
  | EditorMsg of Input.Msg

let updatePanelWithValue name current value panel =
  match Util.expose "updatePanelWithValue" (name,current,value) with
  | ("Left",_,Some v) -> Panel.setLeft v panel
  | ("Right",_,Some v) -> Panel.setRight v panel
  | ("Top",_,Some v) -> Panel.setTop v panel
  | ("Bottom",_,Some v) -> Panel.setBottom v panel
  | ("Horizontal Pos",cv,_) -> 
     let ul = Panel.upperLeft panel in
     let lr = Panel.lowerRight panel in
     let measure = Panel.xAxisStringToGravity cv ul.x lr.x in
     Panel.setLRMeasure measure panel
  | ("Vertical Pos",cv,_) ->
     let ul = Panel.upperLeft panel in
     let lr = Panel.lowerRight panel in
     let measure = Panel.yAxisStringToGravity cv ul.y lr.y in
     Panel.setTBMeasure measure panel
  | _ -> panel

let makeEditors panel =
  let ul = Panel.upperLeft panel in
  let lr = Panel.lowerRight panel in
  let createEditor name value = 
    new InputEditor(
          value, 
          (fun value -> (Util.parseFloat value) <> None), 
          ("numeric-input-good", "numeric-input-bad")
        )
  in
  let createSelector name value vlist =
    new InputSelector(
          value,
          vlist
        )
  in
  (Input.init ()) |>
    foldInto
      (fun editors (name,value) -> 
        Input.create name (createEditor name value) editors
      )
      [
        ("Left", Util.toString ul.x);
        ("Top", Util.toString ul.y);
        ("Right", Util.toString lr.x);
        ("Bottom", Util.toString lr.y);
      ] |>
    foldInto
      (fun editors (name,value,vlist) -> 
        let selector = createSelector name value (new Set<string>(vlist)) in
        Input.create name selector editors
      )
      [
        ("Horizontal Pos", 
         Panel.xAxisPositionString panel.lr,
         List.map Panel.xAxisPositionString Panel.gravityList);
        ("Vertical Pos",
         Panel.yAxisPositionString panel.tb,
         List.map Panel.yAxisPositionString Panel.gravityList);
      ]

type 'msg PanelControls(panel : Panel, dirty : bool, editors : Input.EditorSet, hmap : ('msg Html -> Msg Html), select : 'msg -> Msg option) =
  interface ControlInterface.ControlInterface<Panel,'msg> with
    member self.view html' =
      let html = hmap html' in
      let labeledInput html name (inp : Input.EditorInstance) =
        [
          html.div
            [html.className "control-row"] []
            [html.text name];
          html.div
            [html.className "control-row"] []
            [inp.view html name]
        ]
      in
      let inputs =
        Input.map (labeledInput (Html.map (fun msg -> EditorMsg msg) html)) editors
      in
      html.div [] [] (List.concat inputs)

    member self.update msg =
        match select msg with
        | Some (EditorMsg em) -> 
           let updatedEditors = Input.update em editors in
           let updatePanelFromEditors =
             List.fold 
               (fun panel ((name,editor) : (string * Input.EditorInstance)) ->
                 let cv = Util.log "cv" (editor.currentValue ()) in
                 let v = Util.log "v" (Util.parseFloat cv) in
                 updatePanelWithValue (Util.log "name" name) cv v panel
               )
               panel
               (Input.map Util.tuple2 updatedEditors)
           in
           let result =
             new PanelControls<'msg>(updatePanelFromEditors, true, updatedEditors, hmap, select)
           in
           result :> ControlInterface.ControlInterface<Panel,'msg>
        | _ -> self :> ControlInterface.ControlInterface<Panel,'msg>
    member self.dirty () = dirty
    member self.take () =
      let result =
        new PanelControls<'msg>(panel, false, editors, hmap, select)
      in
      (panel, result :> ControlInterface.ControlInterface<Panel,'msg>)
    
