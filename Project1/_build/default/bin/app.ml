
let testList = ["LIONS"; "TIGER"; "BEARS";"WASPS";"CRABS";"SNAKE";"RHINO"]

let getRandElement list =
  Random.self_init ();  (* Initializes the random generator with a new seed *)
  let randIndex = Random.int (List.length list) in
  List.nth list randIndex
;;

(* Read lines from file*)
let readLinesFromFile filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;
;;

(* Picks a random line from the file *)
let getRandomStringFile filename =
  let lines = readLinesFromFile filename in
    getRandElement lines
;;


(* Selects a string from our list and converts it to a list of characters *)
let stringToCharList str =
  List.init (String.length str) (fun i -> str.[i])
;;

(* Turns our string list to a char list to compare them *)
let stringListToCharList list =
  List.concat (List.map (fun str -> List.of_seq (String.to_seq str)) list)
;;

(* Make the char list uppercase to match our input *)
let charListToUppercase list =
  let str = String.of_seq (List.to_seq list) in   (* Convert char list to string *)
  let upper_str = String.uppercase_ascii str in         (* Convert string to uppercase *)
  List.of_seq (String.to_seq upper_str) 
;;

(* Test answer *)
let testAnswer = stringToCharList (getRandElement testList);;
let answer = charListToUppercase (stringToCharList (getRandomStringFile "words"));;

(* Prints a list of characters*)
let printCharList list =
  List.iter (fun c -> Printf.printf "%c " c) list;
  print_endline ("");
;;

printCharList testAnswer;;
printCharList answer;;



open Bogue
module W = Widget
module L = Layout

let makeBoxLayout color = 
  L.resident ~w:50 ~h:50 (W.box ~style:Style.(of_bg (opaque_bg (color))) ())
let createBoxAndLabel color text =
  L.superpose ~w:50 ~h:50 ~center:true 
  [
    makeBoxLayout color;
    L.resident ~w:50 (W.label text ~size:48 ~fg: Draw.(opaque(251,251,251)));
  ]


(* Function to extract W.labels from a layout *)
let rec getWidgetsWithText layout =
  match L.get_content layout with
  | Resident widget -> if W.get_text widget <> "" then [widget] else []
  | Rooms rooms -> List.flatten (List.map getWidgetsWithText rooms)
  (* Function to extract widgets without text from a layout *)
let rec getWidgetsWithoutText layout =
  match L.get_content layout with
  | Resident widget -> if W.get_text widget = "" then [widget] else []
  | Rooms rooms -> List.flatten (List.map getWidgetsWithoutText rooms)

    
(* Function to extract text from widgets *)


let green = (1,154,1)
let yellow = (255,196,37)
let grey = (128,128,128)

let makeRow colorList charList =
  L.flat ~scale_content:false ~align:Center (List.map2 (fun color char -> createBoxAndLabel color char) colorList charList) 

(* Global list reference to store the text *)
let keysPressed = ref []

(* Global int reference for # of tries *)
let currentTry = ref 0

let gameOver = ref false

(* Global reference to store the button pressed *)

let pressedButton = ref None
let getPressedButton () =
  let b = !pressedButton
  in pressedButton := None;
  b
let lettersCorrect = ref [] 

let makeButton text =
  W.button text ~action:(fun _ -> 
    pressedButton := Some text;
  )

let clickKeyboard = 
  let firstRow = L.flat ~align:Center ~scale_content:true (List.map (fun b -> L.resident (makeButton b)) ["Q"; "W"; "E"; "R"; "T"; "Y"; "U"; "I"; "O"; "P";"Back"]) in
  let secondRow = L.flat ~align:Center ~scale_content:true (List.map (fun b -> L.resident (makeButton b)) ["A"; "S"; "D"; "F"; "G"; "H"; "J"; "K"; "L"; "Enter"]) in
  let thirdRow = L.flat ~align:Center ~scale_content:true (List.map (fun b -> L.resident (makeButton b)) ["Z"; "X"; "C"; "V"; "B"; "N"; "M"]) in  
  L.tower ~align:Center ~background:(L.color_bg Draw.(opaque(find_color "dark_grey"))) [firstRow;secondRow;thirdRow]

let updateText layoutList index =
  getPressedButton ()
  |> Option.iter (fun x -> 
    let layout = List.nth layoutList index in
      if x = "Back" && !gameOver == false then (
        if List.length !keysPressed > 0 then (
          (* Removes last character from list *)
          keysPressed := List.rev (List.tl (List.rev !keysPressed));
          (* Prints our keysPressed list *)
          print_endline (String.concat ", " !keysPressed);
          let widgets = getWidgetsWithText layout in
          List.iteri (fun i w -> 
            if i < List.length !keysPressed then
              W.set_text w (List.nth !keysPressed i)
            else
              W.set_text w " "
          ) widgets;
        )
      ) else if List.length !keysPressed < 5 && x <> "Enter" then (
          (* Adds character entered to list *)
          keysPressed := !keysPressed @ [x];
          (* Prints our keysPressed list *)
          print_endline (String.concat ", " !keysPressed);
          let widgets = getWidgetsWithText layout in
          List.iteri (fun i w -> 
            if i < List.length !keysPressed then
              W.set_text w (List.nth !keysPressed i)
            else
              W.set_text w " "
        ) widgets;
      ) else if x = "Enter" && List.length !keysPressed = 5 then (
        (* If the entered string matches our answer returns correct! *)
        if stringListToCharList !keysPressed = testAnswer then 
          print_endline ("Correct")
        (* Else compares each letter and returns true or false for each*)
        else 
          print_endline ("Wrong");
          let comList = stringListToCharList !keysPressed in
          List.iteri (fun i _ ->
            if i < List.length comList then 
              if List.nth comList i = List.nth answer i then
                lettersCorrect := !lettersCorrect @ ['t']
              else if List.mem (List.nth comList i) answer then
                lettersCorrect := !lettersCorrect @ ['i']
              else 
                lettersCorrect := !lettersCorrect @ ['f']
          ) comList;
          printCharList !lettersCorrect;

        
        (* Updates square colors to green if true or yellow TODO*)
        let box_widgets = getWidgetsWithoutText layout in
        List.iteri (fun i _ -> 
          if i < List.length !lettersCorrect then
            if List.nth !lettersCorrect i = 't' then
              (*set color green*)
                Box.set_style (W.get_box (List.nth box_widgets i)) Style.(of_bg (opaque_bg (green)))
            else if List.nth !lettersCorrect i = 'i' then
              (*set color yellow*)
                Box.set_style (W.get_box (List.nth box_widgets i)) Style.(of_bg (opaque_bg (yellow)))
            else if List.nth !lettersCorrect i = 'f' then
              (*set color dark_grey*)
                Box.set_style (W.get_box (List.nth box_widgets i)) Style.(of_bg (opaque_bg (Draw.find_color "dark_grey")))
        ) box_widgets;
        lettersCorrect := []; (* Reset letters correct *)
        
        (* Prints our keysPressed list *)
        print_endline (String.concat ", " !keysPressed);
        let widgets = getWidgetsWithText layout in
        List.iteri (fun i w -> 
          if i < List.length !keysPressed then
            W.set_text w (List.nth !keysPressed i)
          else
          W.set_text w " "
        ) widgets;

        (* Goes to the next row *)
        if !currentTry < 5 then (
          currentTry := !currentTry + 1;
          (* Resets keysPressed *)  
          keysPressed := [];
        ) else (
          gameOver := true;
          print_endline ("Game Over");
        )
      ) 
    )

let makeDisplayLayout name =
  L.superpose ~name:name ~w:290 ~h:70 ~background:(L.color_bg Draw.(opaque(find_color "grey"))) [makeRow [grey;grey;grey;grey;grey] [" ";" ";" ";" ";" "]]

let displayRows = [makeDisplayLayout "row1";makeDisplayLayout "row2";makeDisplayLayout "row3";makeDisplayLayout "row4";makeDisplayLayout "row5";makeDisplayLayout "row6"]

let main () =
  let finalLayout = L.tower ~scale_content:false ~name:"Not Wordle" ~align:Center ~background:(L.color_bg Draw.(opaque(find_color "dark_grey"))) 
  (displayRows @ [clickKeyboard]) in
  let before_display () =
    Some ()
    |> Option.iter (fun _ -> updateText displayRows !currentTry) in
  Bogue.run ~before_display (Bogue.of_layout finalLayout)

let () = main ();
  Bogue.quit()