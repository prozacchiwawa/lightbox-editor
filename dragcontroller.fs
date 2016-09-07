module DragController

(*

Given a way to find draggable objects or draggable surfaces, the drag
controller does these things:

- It dispatches clicks with the appropriate message when a non-drag
  occurs.
- It remembers what object a drag originated on.
- It issues a move command to the grabbed object for each mouse move.
- It renders an appropriate drag metaphor during the drag.
- It cancels a drag on keystroke or other mouse button.
- It responds to mouse positioned over a drag target by notifying the other
  object.
- It responds to drag end by issuing a drag order with source and target 
  coordinates and objects.

 *)

open Point

(* Our messages *)
type MouseMsg =
  | MouseDown of Point
  | MouseMove of Point
  | MouseUp

(* The object that remembers drags and produces messages. *)
type ('object,'container,'msg) State =
  {
    (* The subject of the drag when starting. *)
    getSubject : Point -> 'container -> 'object option ;
    (* The target of the drag when dragging. *)
    getTarget : Point -> 'container -> 'object option ;
    (* Determine whether two objects are the same (for enter/leave) *)
    isSameObject : 'container -> 'object -> 'object -> bool ;
    (* Create a pickup message at drag start. *)
    authorPickupMsg : Point -> 'container -> 'object -> 'msg ;
    (* Create a move message when dragging (optional target) *)
    authorMoveMsg : Point -> Point -> 'container -> 'object option -> 'object -> 'msg ;
    (* Create a drop message when dragging (optional target) *)
    authorDropMsg : Point -> Point -> 'container -> 'object option -> 'object -> 'msg ;
    (* Create an enter message when target changes. *)
    authorEnterMsg : Point -> Point -> 'container -> 'object -> 'msg ;
    (* Create a leave message when target is left. *)
    authorLeaveMsg : Point -> Point -> 'container -> 'object -> 'msg ;
    (* Start of the drag *)
    dragStart : Point option ;
    (* Current point of the drag *)
    dragEnd : Point option ;
    (* The object we grabbed. *)
    grabbedObject : 'object option ;
    (* The target object we're currently over, if any *)
    targetObject : 'object option ;
  }

(* Create an empty drag controller with default functions and values.
 * Provides a starting point for users.  Since there's a message yielded
 * from the author functions, we have a default here to fulfill the
 * interface. *)
let emptyDragController msg =
  { getSubject = fun pt cont -> None ;
    getTarget = fun pt cont -> None ;
    isSameObject = fun cont o1 o2 -> false ;
    authorPickupMsg = fun mmg cont obj -> msg ;
    authorMoveMsg = fun st dr cont tgt obj -> msg ;
    authorDropMsg = fun st dr cont tgt obj -> msg ;
    authorEnterMsg = fun st dr cont obj -> msg ;
    authorLeaveMsg = fun st dr cont obj -> msg ;
    dragStart = None ;
    dragEnd = None ;
    grabbedObject = None ;
    targetObject = None ;
  }

type ('object,'container,'msg) UpdateResult =
  { dragger : ('object,'container,'msg) State ;
    dispatched : 'msg list
  }

let doStartDrag pt cont (report : ('object,'container','msg) UpdateResult) =
  match report.dragger.getSubject pt cont with
  | Some s ->
     { report with 
       dragger = 
         { report.dragger with
           dragStart = Some pt ; 
           dragEnd = Some pt ;
           grabbedObject = Some s ;
           targetObject = None
         } ; 
       dispatched = 
         (report.dragger.authorPickupMsg pt cont s) :: report.dispatched
     }
  | None ->
     report

let doEnterLeave st pt oldTarget newTarget cont report dispatched =
  match (oldTarget,newTarget,report.dragger.grabbedObject) with
  | (None, None, _) -> dispatched
  | (Some o, None, _) -> 
     (report.dragger.authorLeaveMsg st pt cont o) :: dispatched
  | (None, Some o, Some i) -> 
     if not (report.dragger.isSameObject cont o i) then
       (report.dragger.authorEnterMsg st pt cont o) :: dispatched
     else
       dispatched
  | (Some o, Some n, _) ->
     if not (report.dragger.isSameObject cont o n) then
       (report.dragger.authorEnterMsg st pt cont n) ::
         ((report.dragger.authorLeaveMsg st pt cont o) ::
            dispatched)
     else
       dispatched

let doMove st pt cont report =
  match report.dragger.grabbedObject with
  | Some sub ->
     let oldTarget = report.dragger.targetObject in
     let newTarget = report.dragger.getTarget pt cont in
     let dispatch = 
       doEnterLeave 
         st pt oldTarget newTarget cont report
         ((report.dragger.authorMoveMsg st pt cont newTarget sub) :: report.dispatched)
     in
     { report with
       dragger =
         { report.dragger with
           targetObject = newTarget ;
           dragEnd = Some pt
         } ;
       dispatched = dispatch
     }
  | None -> report

let doEndDrag st pt cont sub report =
  let dispatch =
    (report.dragger.authorDropMsg st pt cont report.dragger.targetObject sub) :: 
      report.dispatched
  in
  { report with
    dragger =
      { report.dragger with
        dragStart = None ;
        dragEnd = None ;
        grabbedObject = None ;
        targetObject = None
      } ;
    dispatched = dispatch
  }

let update msg cont state =
  let dragState = (msg, state.dragStart, state.grabbedObject) in
  let emptyReport = { dragger = state ; dispatched = [] }
  match dragState with
  | (MouseDown pt, _, _) -> 
     emptyReport
     |> doStartDrag pt cont
     |> doMove pt pt cont 
  | (MouseMove pt, Some st, Some obj) ->
     emptyReport
     |> doMove st pt cont
  | (MouseUp, Some st, Some obj) ->
     emptyReport
     |> doEndDrag st st cont obj
  | _ -> { dragger = state ; dispatched = [] }
