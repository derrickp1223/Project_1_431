
let testList = ["lions"; "tiger"; "bears";"wasps";"crabs";"snake";"rhino"]

let getRandElement list =
  let randIndex = Random.int (List.length list) in
  List.nth list randIndex
;;

print_endline (getRandElement testList);;

open Bogue
module W = Widget
module L = Layout

let makeBoxWidgit color = 
  W.box ~w:50 ~h:50 ~style:Style.(of_bg (opaque_bg (color)))
let makeBoxLayout color text =
  L.superpose ~w:50 ~h:50 ~center:true 
  [
    L.resident ((makeBoxWidgit color) ());
    L.resident (W.label text ~size:48 ~fg: Draw.(opaque(251,251,251)));
  ]


let rec getWidgetsFromLayout layout =
  match L.get_content layout with
  | Resident widget -> [widget]
  | Rooms rooms -> List.flatten (List.map getWidgetsFromLayout rooms)

(* Function to extract text from widgets *)


(* Colors 
let green = (1,154,1)
let yellow = (255,196,37)
*)

let grey = (128,128,128)

let makeRow colorList charList =
  L.flat ~scale_content:false ~align:Center (List.map2 (fun color char -> makeBoxLayout color char) colorList charList) 

let testRow = makeRow [(1,154,1);(255,196,37);(1,154,1);(255,196,37);(1,154,1)] ["A"; "S"; "D"; "F"; "G"]

let getTextStringsFromWidgets widgets =
  List.filter_map (fun w -> Some (W.get_text w)) widgets

let stringsFromTestRow = getTextStringsFromWidgets (getWidgetsFromLayout testRow)


(* Global list reference to store the text *)
let keysPressed = ref []
let pressedButton = ref None
let getPressedButton () =
  let b = !pressedButton
  in pressedButton := None;
  b

  let makeButton text =
    W.button text ~action:(fun _ -> 
      pressedButton := Some text;
    )

let clickKeyboard = 
  let firstRow = L.flat ~align:Center ~scale_content:true (List.map (fun b -> L.resident (makeButton b)) ["Q"; "W"; "E"; "R"; "T"; "Y"; "U"; "I"; "O"; "P";"Back"]) in
  let secondRow = L.flat ~align:Center ~scale_content:true (List.map (fun b -> L.resident (makeButton b)) ["A"; "S"; "D"; "F"; "G"; "H"; "J"; "K"; "L"]) in
  let thirdRow = L.flat ~align:Center ~scale_content:true (List.map (fun b -> L.resident (makeButton b)) ["Z"; "X"; "C"; "V"; "B"; "N"; "M"]) in  
  L.tower ~align:Center ~background:(L.color_bg Draw.(opaque(find_color "dark_grey"))) [firstRow;secondRow;thirdRow]
  let updateLayout layout =
    getPressedButton ()
    |> Option.iter (fun x -> 
        if x = "Back" then (
          if List.length !keysPressed > 0 then (
            keysPressed := List.rev (List.tl (List.rev !keysPressed));
            print_endline (String.concat ", " !keysPressed);
            let dis = L.flat ~align:Center ~scale_content:false (List.map (fun x -> makeBoxLayout grey x) !keysPressed) in
            L.set_rooms ~sync:false layout [dis];
          )
        ) else if List.length !keysPressed < 5 then (
          keysPressed := !keysPressed @ [x];
          print_endline (String.concat ", " !keysPressed);
          let dis = L.flat ~align:Center ~scale_content:false (List.map (fun x -> makeBoxLayout grey x) !keysPressed) in
          L.set_rooms ~sync:false layout [dis];
        )
      )

let main () =
  print_endline (String.concat ", " stringsFromTestRow);
  let displayLayout = L.superpose ~w:290 ~h:70 ~background:(L.color_bg Draw.(opaque(find_color "grey"))) [] in
  let finalLayout = L.tower ~scale_content:false ~name:"Not Wordle" ~align:Center ~background:(L.color_bg Draw.(opaque(find_color "dark_grey"))) [displayLayout;clickKeyboard] in
  let before_display () =
    Some ()
    |> Option.iter (fun _ -> updateLayout displayLayout) in
  Bogue.run ~before_display (Bogue.of_layout finalLayout)

let () = main ();
  Bogue.quit()