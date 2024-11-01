
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
  W.box ~w:50 ~h:50 ~style:Style.(of_bg (opaque_bg (Draw.find_color color)))
let makeBoxLayout color text =
  L.superpose ~w:50 ~h:50 
  [
    L.resident (W.label text ~fg: Draw.(opaque(find_color "white")));
    L.resident ((makeBoxWidgit color) ()) 
  ]

  let makeRow colorList charList =
    L.flat ~scale_content:false ~align:Center (List.map2 (fun color char -> makeBoxLayout color char) colorList charList)

  let row1 = 
    makeRow ["green"; "yellow"; "grey"; "green"; "grey"] [""; ""; ""; ""; ""]

  let row2 =
    makeRow ["green"; "grey"; "grey"; "green"; "yellow"] [""; ""; ""; ""; ""]


let main () =

  let main_layout = L.tower ~name:"Not Wordle" ~align:Center ~background:(L.color_bg Draw.(opaque(find_color "dark_grey"))) [row1;row2;] in
  
  Bogue.(run(of_layout main_layout))

let () = main ();
  Bogue.quit()