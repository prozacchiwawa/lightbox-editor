module PanelSerialize

open LayoutMgr
open Panel
open SerializeData
open LayoutMgrLoad
open Measure

let saveAxisPosition ap =
  match ap with
  | LowGrav (l,w) ->
     map
       [ ("type", string "LowGrav") ;
         ("l", num l) ;
         ("w", num w) ]
  | HighGrav (w,r) ->
     map
       [ ("type", string "HighGrav") ;
         ("w", num w) ;
         ("r", num r) ]
  | MidCover (l,r) ->
     map
       [ ("type", string "MidCover") ;
         ("l", num l) ;
         ("r", num r) ]

let loadAxisPosition aps =
  let matchTuple =
    (Map.tryFind "type" aps,
     Map.tryFind "l" aps,
     Map.tryFind "w" aps,
     Map.tryFind "r" aps)
  in
  match matchTuple with
  | ( Some (String "LowGrav"), 
      Some (Float l), 
      Some (Float w), 
      _
    ) -> 
      Some (LowGrav (l,w))
  | ( Some (String "HighGrav"),
      _,
      Some (Float w),
      Some (Float r) 
    ) -> Some (HighGrav (w,r))
  | ( Some (String "MidCover"),
      Some (Float l),
      _,
      Some (Float r)
    ) -> Some (MidCover (l,r))
  | _ -> None

let savePosition p =
  string (positionString p)

let loadPosition p =
  match p with
  | String s -> stringToPosition s
  | _ -> Panel.Relative

let rec save panel =
  map
    [ ("lr", saveAxisPosition panel.lr) ;
      ("tb", saveAxisPosition panel.tb) ;
      ("position", savePosition panel.position) ;
      ("background", string panel.background) ;
      ("id", string panel.id) ;
      ("children", list (List.map save panel.children)) ;
      ("layout", panel.layout.serialize panel)
    ]

let rec load ser =
  let emptyPanel =
    {
      lr = LowGrav (0.,1.) ;
      tb = LowGrav (0.,1.) ;
      position = Absolute ;
      background = "" ;
      id = "" ;
      children = [] ;
      layout = LayoutMgrImpl.FreeLayoutMgr() :> LayoutMgr<Panel,RenderMsg>
    }
  in
  match ser with
  | Dict d ->
     Map.fold
       (fun panel key v ->
         match (key,v) with
         | ("lr",Dict lr) ->
            (match loadAxisPosition lr with
             | Some v -> { panel with lr = v }
             | _ -> panel)
         | ("tb",Dict tb) ->
            (match loadAxisPosition tb with
             | Some v -> { panel with tb = v }
             | _ -> panel)
         | ("position",pos) ->
            { panel with position = loadPosition pos }
         | ("background",String bkg) ->
            { panel with background = bkg }
         | ("id",String id) ->
            { panel with id = id }
         | ("children",List l) ->
            { panel with children = List.map load l }
         | ("layout",lm) ->
            { panel with layout = LayoutMgrLoad.load lm }
         | _ -> panel)
       emptyPanel
       d
  | _ -> emptyPanel

