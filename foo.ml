open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml

let display x =
  Dom.appendChild (Dom_html.getElementById "output") (Tyxml_js.To_dom.of_element x)


module RList = ReactiveData.RList

let rdata,rdset = React.S.create ""

let rl, rhandle = RList.create []

let li_rl = RList.map (fun x -> Tyxml_js.Html.(li [ txt x ])) rl

let ul_elt = Tyxml_js.R.Html.ul li_rl

let todo_input = 
  let on_input_change (evt : Dom_html.event Js.t) = 
    Js.Opt.(
      let s = bind evt##.target (fun s -> Dom_html.CoerceTo.input s) in
      map s (fun l -> rdset (Js.to_string l##.value)) |> ignore;
    );
    false 
  in
  Tyxml_js.R.Html.(
  let p, _setP = React.S.create "Todo App" in
  input  ~a:[ a_placeholder p
            ; a_value rdata
            ; a_oninput on_input_change] ()
) 

let snoc s = RList.snoc s rhandle

let cons s = RList.cons s rhandle

let insert s pos = RList.insert s pos rhandle

let remove pos = RList.remove pos rhandle

let time_signal =
  let s, set = React.S.create (Sys.time ()) in
  let rec loop () : unit Lwt.t =
    set (Sys.time ());
    Lwt.bind (Lwt_js.sleep 1.) loop
  in
  Lwt.async loop;
  s


let div_elt =
  Tyxml_js.(
    Html.(
      div
        [ 
          (* h4
            [ txt "Uptime is "
            ; R.Html.txt
                (React.S.map (fun s -> string_of_int (int_of_float s)) time_signal)
            ; txt " s"
            ]
            ;  *)
            div [
              todo_input
            ; button 
              ~a:[ a_class ["btn"]
                 ; a_onclick (fun (_e) -> 
                        let d = React.S.value rdata in
                        Firebug.console##log d;
                        if (d = "") then ()
                        else 
                            snoc d;
                            rdset "";
                        ;
                        true
                          
                  )
                 ]
              [ txt "add"]
            ]
        ; ul_elt
        ]))


let _ = display div_elt