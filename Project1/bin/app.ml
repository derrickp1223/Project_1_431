
let testList = ["lions"; "tiger"; "bears";"wasps";"crabs";"snake";"rhino"]

let getRandElement list =
  let randIndex = Random.int (List.length list) in
  List.nth list randIndex
;;

print_endline (getRandElement testList);;

open Bogue
module W = Widget
module L = Layout

let makeBox color text =
  L.resident ~w:50 ~h:50 ~background:(L.color_bg Draw.(opaque(find_color color))) 
  (W.label text ~fg: Draw.(opaque(find_color "white")))

let firstRowOfBoxes = [makeBox "grey" "";makeBox "grey" "";makeBox "grey" "";makeBox "grey" "";makeBox "grey" "";]

let rowOfBoxes name list = 
  L.flat ~name:name ~scale_content:false ~align:Center list

let rowsOfRows = 
  L.tower [rowOfBoxes "firstRow" firstRowOfBoxes;]

let main () =
  let main_layout = L.tower ~name:"Not Wordle" ~align:Max ~background:(L.color_bg Draw.(opaque(find_color "dark_grey"))) [rowsOfRows] in
    Bogue.(run(of_layout main_layout))

let () = main ();
  Bogue.quit()