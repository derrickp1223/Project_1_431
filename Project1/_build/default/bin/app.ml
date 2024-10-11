let () = print_endline "Hello, World!"

let testList = ["lions"; "tiger"; "bears"]

let rec getElement list index =
  match list with
  | [] -> None
  | hd :: tl -> if index = 0 then Some hd else getElement tl (index - 1)
;;
getElement testList 0;;