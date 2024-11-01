
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

  let makeRow colorList charList =
    L.flat ~scale_content:false ~align:Center (List.map2 (fun color char -> makeBoxLayout color char) colorList charList)


  let green = (1,154,1)
  let yellow = (255,196,37)
  let grey = (128,128,128)
  let row0 = 
    makeRow [grey; grey; grey; grey; grey] [""; ""; ""; ""; ""]
  let row1 = 
    makeRow [green; yellow; grey; green; grey] ["L"; "I"; "O"; "N"; "S"]

  let row2 =
    makeRow [green; grey; yellow; green; grey] [""; ""; ""; ""; ""]


let main () =

  let main_layout = L.tower ~name:"Not Wordle" ~align:Center ~background:(L.color_bg Draw.(opaque(find_color "dark_grey"))) [row0;row1;row2;] in
  
  Bogue.(run(of_layout main_layout))

let () = main ();
  Bogue.quit()