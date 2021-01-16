let matchloop(regexp, text) = if matchhere(regexp, text) then true else if text++

let match (regexp, text) = 
  match regexp with
  '^' -> matchhere(regexp+1,text)
  | _ -> matchloop(regexp, text)