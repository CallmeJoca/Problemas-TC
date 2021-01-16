

let matchf (char * regexp, char * text) = 
  if regexp[0] == '^' then 
  return matchhere(regexp+1, text) else
  while *text++ != "\0"