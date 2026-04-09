(** Parse vector tables *)

open Common
open Vector_table
open CalendarLib
open Logging

type parsing_stage =
  | Header
  | NewRow
  | Position
  | Velocity
  | Misc
  | Done
  | Fail of string

(** Parse an individual line of a vector table file *)
let parse_line (center_body, target_body, entries, stage) line =
  let open Str in
  if String.trim line = "$$EOE" then
    (center_body, target_body, entries, Done)
  else
    match stage with
    | Fail s ->
        (center_body, target_body, entries, stage)
    | Header ->
        let center_rexp =
          regexp {|^Center body name: \(.*\) (\(-?[0-9]+\)).*$|}
        in
        let target_rexp =
          regexp {|^Target body name: \(.*\) (\(-?[0-9]+\)).*$|}
        in
        (* Check for center body *)
        let center_body =
          if string_match center_rexp line 0 then
            Some
              { name= matched_group 1 line
              ; id= int_of_string (matched_group 2 line) }
          else
            center_body
        in
        (* Check for target body *)
        let target_body =
          if string_match target_rexp line 0 then
            Some
              { name= matched_group 1 line
              ; id= int_of_string (matched_group 2 line) }
          else
            target_body
        in
        ( center_body
        , target_body
        , []
        , if line = "$$SOE" then
            NewRow
          else
            Header )
    | NewRow ->
        let time_rexp =
          regexp
            {|^[0-9]+\.[0-9]+ = A.D. \([0-9]+-[A-Z][a-z]+-[0-9]+ [0-9]+:[0-9]+:[0-9]+\).[0-9]+.*|}
        in
        if string_match time_rexp line 0 then
          ( center_body
          , target_body
          , ( Some
                (Calendar.to_unixfloat
                   (Printer.Calendar.from_fstring "%Y-%b-%d %H:%M:%S"
                      (matched_group 1 line |> String.trim) ) )
            , None
            , None
            , None
            , line )
            :: entries
          , Position )
        else
          ( center_body
          , target_body
          , []
          , Fail (Printf.sprintf "Expected time but got line: %s" line) )
    | Position ->
        let pos_rexp = regexp {|.*X =\(.*\) Y =\(.*\) Z =\(.*\)|} in
        let time, tl =
          match entries with
          | [] ->
              [%unreachable]
          | (time, _, _, _, _) :: tl ->
              (time, tl)
        in
        if string_match pos_rexp line 0 then
          ( center_body
          , target_body
          , ( time
            , Some
                { x= float_of_string (matched_group 1 line |> String.trim)
                ; y= float_of_string (matched_group 2 line |> String.trim)
                ; z= float_of_string (matched_group 3 line |> String.trim) }
            , None
            , None
            , line )
            :: tl
          , Velocity )
        else
          ( center_body
          , target_body
          , []
          , Fail (Printf.sprintf "Expected position but got line: %s" line) )
    | Velocity ->
        let pos_rexp = regexp {|.*VX=\(.*\) VY=\(.*\) VZ=\(.*\)|} in
        let time, pos, tl =
          match entries with
          | [] ->
              [%unreachable]
          | (time, pos, _, _, _) :: tl ->
              (time, pos, tl)
        in
        if string_match pos_rexp line 0 then
          ( center_body
          , target_body
          , ( time
            , pos
            , Some
                { x= float_of_string (matched_group 1 line |> String.trim)
                ; y= float_of_string (matched_group 2 line |> String.trim)
                ; z= float_of_string (matched_group 3 line |> String.trim) }
            , None
            , line )
            :: tl
          , Misc )
        else
          ( center_body
          , target_body
          , []
          , Fail (Printf.sprintf "Expected velocity but got line: %s" line) )
    | Misc ->
        let pos_rexp = regexp {|.*LT=\(.*\) RG=\(.*\) RR=\(.*\)|} in
        let time, pos, vel, tl =
          match entries with
          | [] ->
              [%unreachable]
          | (time, pos, vel, _, _) :: tl ->
              (time, pos, vel, tl)
        in
        if string_match pos_rexp line 0 then
          ( center_body
          , target_body
          , ( time
            , pos
            , vel
            , Some
                { x= float_of_string (matched_group 1 line |> String.trim)
                ; y= float_of_string (matched_group 2 line |> String.trim)
                ; z= float_of_string (matched_group 3 line |> String.trim) }
            , line )
            :: tl
          , NewRow )
        else
          ( center_body
          , target_body
          , []
          , Fail (Printf.sprintf "Expected velocity but got line: %s" line) )
    | Done ->
        (center_body, target_body, entries, stage)

(** Parse a vector table file *)
let parse (p : path) : vtable =
  let lines = In_channel.input_lines (open_in p) in
  match List.fold_left parse_line (None, None, [], Header) lines with
  | _, _, _, Fail s ->
      fatal rc_Parsing "%s" s
  | Some center_body, Some target_body, entries, _ ->
      let entries =
        List.mapi
          (fun i (time, pos, vel, misc, line) ->
            match (time, pos, vel, misc) with
            | Some time, Some pos, Some vel, Some misc ->
                {time; pos; vel; lt= misc.x; rt= misc.y; rr= misc.z}
            | _, _, _, _ ->
                fatal rc_Parsing "Incomplete vector table entry on line %d: %s"
                  i line )
          entries
        |> List.rev
      in
      {center_body; target_body; entries}
  | _ ->
      fatal rc_Parsing "Failed to parse center bbody or target body"
