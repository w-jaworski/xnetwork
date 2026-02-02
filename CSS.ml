open WWWtypes

let rec string_of_css_fmt spaces = function
  | CSSdeclarations(sels,decls) -> 
      spaces ^ String.concat ", " sels ^ " {\n" ^ 
      String.concat "\n" (List.rev (Xlist.rev_map decls (fun s -> spaces ^ "  " ^ s ^ ";"))) ^ "}"
  | CSSgroup(sels,decls) -> 
      spaces ^ String.concat ", " sels ^ " {\n" ^ 
      String.concat "\n" (List.rev (Xlist.rev_map decls (string_of_css_fmt (spaces ^ "  ")))) ^ "}"
  | CSScomment s -> spaces ^ s ^ ";"
  
let parse_selectors s =
  let l = Xstring.split "," s in
  let l = Xlist.map l remove_white in
(*   Xlist.iter l print_endline; *)
  l
    
let parse_declarations s = 
  let l = Xstring.split ";" s in
  let l = Xlist.map l remove_white in
(*   Xlist.iter l print_endline; *)
  l
    
let rec parse_css_rec rev = function  
    Lexer.T selectors :: Lexer.B("{","}",[Lexer.T declarations]) :: l -> 
      let selectors = parse_selectors selectors in
      let declarations = parse_declarations declarations in
      parse_css_rec ((CSSdeclarations(selectors,declarations)) :: rev) l
  | Lexer.T selectors :: Lexer.B("{","}",declarations) :: l -> 
      let selectors = parse_selectors selectors in
      let declarations =  parse_css_rec [] declarations in
      parse_css_rec ((CSSgroup(selectors,declarations)) :: rev) l
  | Lexer.T s :: l when Xstring.check_prefix "/*" s && Xstring.check_sufix "*/" s -> parse_css_rec (CSScomment s :: rev) l
  | Lexer.T s :: _ -> failwith ("parse_css_rec: " ^ s)
  | [] -> List.rev rev
  | _ -> failwith "parse_css_rec"
(*   | l -> print_endline (Lexer.string_of_token_list_space l) *)
   
let parse s =
  let text = Lexer.split "{\\|}" s in
  let text = List.rev (Xlist.fold text [] (fun text -> function 
      Lexer.T s -> 
        let s = remove_white s in
        if s = "" then text else (Lexer.T s) :: text
    | t -> t :: text)) in
  let text = Lexer.find_brackets ["{","}"] [] text in
  parse_css_rec [] text
