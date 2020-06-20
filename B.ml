
(*Funçoes de leitura das linhas de input.
  Adiciona-se um espaço vazio ao inicio de cada string para deixar a posiçao 0 0 da matriz vazio.*)
let string1 = String.concat "" [" ";read_line()];;
let string2 = String.concat "" [" ";read_line()];;

(*Funçao que cria uma matriz onde cada posiçao contem um valor integer a representar o grau de characteres iguais consecutivos.
  Funçao começa por gerar uma matriz inicializada a 0s.
  De seguida percorre cada posiçao das strings str1 e str2 a comparar todos os characteres individuais de uma string com todos
  os characteres da outra e registando o grau consecutivo de igualdade na matriz.
  No fim de iterar todos os characteres e completar a matriz, esta é devolvida.*)
let matriz str1 str2 = 
  (*Inicializaçao de uma matriz com todos os valores a 0*)
  let matriz = Array.make_matrix (String.length(str1)) (String.length(str2)) (0) in
    for x = 1 to (String.length str1 -1) do 
      (for y = 1 to (String.length str2 -1) do 
        (*Iteraçao dos characters nas posiçoes x e y num If statement.
          Caso ambos os characters sejam iguais, a matriz atualiza a posiçao x y para ser o valor anterior na diagonal(x-1 y-1) mais 1.
          Caso sejam diferentes, a matriz mantem o valor na posiçao x y a 0.*)
        if (String.get str1 (x)) = (String.get str2 (y)) then matriz.(x).(y) <- matriz.(x -1).(y -1) +1 else matriz.(x).(y) <- 0;
      done);
    done;
    matriz;;

(*Funçao que percorre recursivamente a matriz gerada acima para encontrar o valor mais elevado presente nesta.
  Esta funçao compara o valor na posiçao x y da matriz como o valor de counter. Caso o valor seja maior, o a funçao volta a ser chamada para a proxima posiçao da matriz com o counter atualizado.
  Caso contrario, chama-se a mesma a funçao para a proxima posiçao sem atualizar o counter.
  Quando se atinge a posiçao final da matriz, a funçao compara uma ultima vez o valor da matriz com o counter e faz print do counter.*)
let rec highest_num matriz x y counter = if y = Array.length matriz.(x) -1 then 
  (if x = (Array.length matriz -1) then (if matriz.(x).(y) > counter then Printf.printf "%d\n" matriz.(x).(y) else Printf.printf "%d\n" counter) else 
      highest_num matriz (x+1) 0 (if matriz.(x).(y) > counter then matriz.(x).(y) else counter)) else 
    highest_num matriz x (y+1) (if matriz.(x).(y) > counter then matriz.(x).(y) else counter);;

(*Funçao inical que chama o resultado das funçoes string1 e string2(funçoes que recebem os strings de input) aplicadas a funçao matriz e aplica esta à funçao highest_num*)
let () = highest_num (matriz string1 string2) 0 0 0;;