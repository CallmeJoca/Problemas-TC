let string1 = String.concat "" [" ";read_line()];;
let string2 = String.concat "" [" ";read_line()];;

let char_pos str pos = String.get str (pos);;

let compare_letter a b = if a = b then 0 else 1;;

let compare_dna str1 str2 = 
  let matriz = Array.make_matrix (String.length(str1)) (String.length(str2)) (0) in
    for x = 0 to (String.length(str1) -1) do matriz.(x).(0) <- x done;
    for y = 0 to (String.length(str2)  -1) do matriz.(0).(y) <- y done;
    for x = 1 to (String.length(str1) -1) do 
      (for y = 1 to (String.length(str2)  -1) do 
        (matriz.(x).(y) <- List.hd(List.sort compare [ (matriz.(x -1).(y -1) + compare_letter (char_pos str1 (x)) (char_pos str2 (y))) ; (matriz.(x).(y -1) + 1) ; (matriz.(x -1).(y) + 1) ]));
      done);
    done; 
    matriz.(String.length(str1) -1).(String.length(str2) -1);;

let () = Printf.printf "%d\n" (compare_dna string1 string2);;