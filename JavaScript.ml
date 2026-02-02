open WWWtypes

let rec fold t s f =
  let s = match t with
      InstrBlock l -> Xlist.fold l s (fun s t -> fold t s f)
    | If(_,b1,b2) -> Xlist.fold [b1;b2] s (fun s t -> fold t s f)
    | _ -> s in
  f s t

let rec js_expr_to_string_fmt spaces = function
    JStrue -> "true"
  | JSfalse -> "false"
  | JSnull -> "null"
  | JSstring s -> "\"" ^ s ^ "\""
  | JSnumber s -> s
  | JSmap l -> "{" ^ String.concat "," (Xlist.map l (fun (e,t) -> "\n" ^ "  " ^ spaces ^ e ^ ": " ^ js_expr_to_string_fmt ("  " ^ spaces) t)) ^ "}"
  | JSlist l -> "[" ^ String.concat "," (Xlist.map l (fun t -> "\n" ^ "  " ^ spaces ^ js_expr_to_string_fmt ("  " ^ spaces) t)) ^ "]"
  | JSneg t -> "!" ^ js_expr_to_string_fmt spaces t
  | JSdot l -> String.concat "." (Xlist.map l (js_expr_to_string_fmt spaces))
  | JSident s -> s
  | JSlambda(args,body) ->
      "function(" ^ String.concat "," (Xlist.map args (js_expr_to_string_fmt ("  " ^ spaces))) ^ ") {\n" ^
      js_to_string_fmt ("  " ^ spaces) body ^ "}"
  | JScall(a,l) -> js_expr_to_string_fmt "" a ^ "(" ^ String.concat "," (Xlist.map l (js_expr_to_string_fmt ("  " ^ spaces))) ^ ")"
  | UnknownExpr s -> s

and js_to_string_fmt spaces = function
    InstrBlock l -> String.concat "\n" (Xlist.map l (fun t -> js_to_string_fmt ("  " ^ spaces) t ^ ";"))
  | If(cond,b1,b2) ->
      spaces ^ "if " ^ js_expr_to_string_fmt ("  " ^ spaces) cond ^ "{\n" ^
      js_to_string_fmt ("  " ^ spaces) b1 ^ "}\n" ^
      spaces ^ "else {\n" ^
      js_to_string_fmt ("  " ^ spaces) b2 ^ "}"
  | Assign(a,b) -> spaces ^ js_expr_to_string_fmt "" a ^ " = " ^ js_expr_to_string_fmt ("  " ^ spaces) b
  | Call(a,l) -> spaces ^ js_expr_to_string_fmt "" a ^ "(" ^ String.concat "," (Xlist.map l (js_expr_to_string_fmt ("  " ^ spaces))) ^ ")"
  | Var(s,t) -> spaces ^ "var " ^ s ^ " = " ^ js_expr_to_string_fmt ("  " ^ spaces) t
  | Unknown s -> spaces ^ s


type syntax =
      T of string
    | X of string
    | O of string
    | C of string
    | D of string
    | I of string
    | B of string * string * syntax list

let rec string_of_syntax = function
      O s -> "„" ^ s ^ "”"
    | X s -> (*"'" ^*) s (*^ "'"*)
    | T s -> "\"" ^ s ^ "\""
    | C s -> s
    | D s -> s
    | I s -> s
    | B(s,t,l) -> s ^ string_of_syntax_list l ^ t

and string_of_syntax_list l =
    String.concat " " (Xlist.map l string_of_syntax)

let is_number s =
  Int.fold 0 (String.length s-1) true (fun b i ->
    if String.get s i >= '0' && String.get s i <= '9' then b else false)

let is_identifier s =
  Int.fold 0 (String.length s-1) true (fun b i ->
    if (String.get s i >= '0' && String.get s i <= '9') ||
       (String.get s i >= 'a' && String.get s i <= 'z') ||
       (String.get s i >= 'A' && String.get s i <= 'Z') ||
       String.get s i = '_' then b else false)

let rec find_atomic_symbols = function
      T t :: l -> T t :: (find_atomic_symbols l)
    | X "[" :: l -> O "[" :: (find_atomic_symbols l)
    | X "]" :: l -> O "]" :: (find_atomic_symbols l)
    | X "{" :: l -> O "{" :: (find_atomic_symbols l)
    | X "}" :: l -> O "}" :: (find_atomic_symbols l)
    | X "(" :: l -> O "(" :: (find_atomic_symbols l)
    | X ")" :: l -> O ")" :: (find_atomic_symbols l)
    | X "," :: l -> O "," :: (find_atomic_symbols l)
    | X "." :: l -> O "." :: (find_atomic_symbols l)
    | X ":" :: l -> O ":" :: (find_atomic_symbols l)
    | X ";" :: l -> O ";" :: (find_atomic_symbols l)
    | X "!" :: X "=" :: X "=" :: l -> O "!==" :: (find_atomic_symbols l)
    | X "!" :: X "=" :: l -> O "!=" :: (find_atomic_symbols l)
    | X "!" :: l -> O "!" :: (find_atomic_symbols l)
    | X "=" :: X "=" :: X "=" :: l -> O "===" :: (find_atomic_symbols l)
    | X "=" :: X "=" :: l -> O "==" :: (find_atomic_symbols l)
    | X "=" :: l -> O "=" :: (find_atomic_symbols l)
    | X "<" :: l -> O "<" :: (find_atomic_symbols l)
    | X ">" :: l -> O ">" :: (find_atomic_symbols l)
    | X "+" :: X "+" :: l -> O "++" :: (find_atomic_symbols l)
    | X "+" :: l -> O "+" :: (find_atomic_symbols l)
    | X "-" :: l -> O "-" :: (find_atomic_symbols l)
    | X "*" :: l -> O "*" :: (find_atomic_symbols l)
    | X "^" :: l -> O "^" :: (find_atomic_symbols l)
    | X "~" :: l -> O "~" :: (find_atomic_symbols l)
    | X "/" :: l -> O "/" :: (find_atomic_symbols l)
    | X "\\" :: l -> O "\\" :: (find_atomic_symbols l)
    | X "?" :: l -> O "?" :: (find_atomic_symbols l)
    | X "$" :: l -> O "$" :: (find_atomic_symbols l)
    | X "&" :: X "&" :: l -> O "&&" :: (find_atomic_symbols l)
    | X "|" :: X "|" :: l -> O "||" :: (find_atomic_symbols l)
    | X "|" :: l -> O "|" :: (find_atomic_symbols l)
    | X "null" :: l -> C "null" :: (find_atomic_symbols l)
    | X "true" :: l -> C "true" :: (find_atomic_symbols l)
    | X "false" :: l -> C "false" :: (find_atomic_symbols l)
    | X "if" :: l -> C "if" :: (find_atomic_symbols l)
    | X "var" :: l -> C "var" :: (find_atomic_symbols l)
    | X "function" :: l -> C "function" :: (find_atomic_symbols l)
    | X "new" :: l -> C "new" :: (find_atomic_symbols l)
    | X "throw" :: l -> C "throw" :: (find_atomic_symbols l)
    | X x :: l when is_number x -> D x :: (find_atomic_symbols l)
    | X x :: l when is_identifier x -> I x :: (find_atomic_symbols l)
    | X x :: l -> (*print_endline ("JavaScript.find_atomic_symbols: " ^ x);*) X x :: (find_atomic_symbols l)
    | [] -> []
    | _ -> failwith "JavaScript.find_atomic_symbols"

exception Closing_bracket_not_found

let rec find_brackets brackets rev = function
      (O s) :: l ->
         (try
           let t = Xlist.assoc brackets s in
(*            print_endline ("find_brackets 1: " ^ string_of_syntax_list ((O s) :: l)); *)
           let found,l = find_rbracket t brackets [] l in
(*            print_endline ("find_brackets 2: " ^ string_of_syntax_list found); *)
           find_brackets brackets (B(s,t,found) :: rev) l
         with Not_found -> find_brackets brackets ((O s) :: rev) l)
    | B _ :: _ -> failwith "JavaScript.find_brackets"
    | t :: l -> find_brackets brackets (t :: rev) l
    | [] -> List.rev rev

and find_rbracket rb brackets rev = function
      (O s) :: l ->
         if s = rb then List.rev rev, l else
         (try
           let t = Xlist.assoc brackets s in
(*            print_endline ("find_rbracket 1: " ^ string_of_syntax_list ((O s) :: l)); *)
           let found,l = find_rbracket t brackets [] l in
(*            print_endline ("find_rbracket 2: " ^ string_of_syntax_list found); *)
           find_rbracket rb brackets ((B(s,t,found)) :: rev) l
         with Not_found -> find_rbracket rb brackets ((O s) :: rev) l)
    | (B _) :: _ -> failwith "JavaScript.find_rbracket 1"
    | t :: l -> find_rbracket rb brackets (t :: rev) l
    | [] -> raise Closing_bracket_not_found

let rec split_op_comma found rev = function
      (O ",") :: l -> split_op_comma ((List.rev rev) :: found) [] l
    | s :: l -> split_op_comma found (s :: rev) l
    | [] -> if rev = [] then List.rev found else List.rev ((List.rev rev) :: found)

let rec merge_quoted2 rev = function
      [] -> failwith "JavaScript.merge_quoted2 1"
    | "\"" :: tokens -> String.concat "" (List.rev rev), tokens
    | "\\" :: "\"" :: tokens -> merge_quoted2 ("\"" :: rev) tokens
    | "\\" :: "\\" :: tokens -> merge_quoted2 ("\\" :: rev) tokens
    | "\\" :: s :: tokens ->
       (match String.get s 0 with
         'b' -> merge_quoted2 ("\b" :: rev) (Xstring.cut_prefix "b" s :: tokens)
	   | 'f' -> merge_quoted2 ("\012" :: rev) (Xstring.cut_prefix "f" s :: tokens)
	   | 'n' -> merge_quoted2 ("\n" :: rev) (Xstring.cut_prefix "n" s :: tokens)
	   | 'r' -> merge_quoted2 ("\r" :: rev) (Xstring.cut_prefix "r" s :: tokens)
	   | 't' -> merge_quoted2 ("\t" :: rev) (Xstring.cut_prefix "t" s :: tokens)
	   | 'u' ->
	      if Xstring.size s < 5 then failwith "JavaScript.merge_quoted2 2" else
	      let t = String.sub s 1 4 in
	      let x = Scanf.sscanf t "%x" (fun y -> y) in
	      let t2 = Xunicode.string_of_uchar x in
	      merge_quoted2 (t2 :: rev) (Xstring.cut_prefix "u0000" s :: tokens)
	   | '/' -> merge_quoted2 ("/" :: rev) (Xstring.cut_prefix "/" s :: tokens)
	   | 'v' -> merge_quoted2 ("\\" :: rev) (s :: tokens)
	   | _ -> failwith ("JavaScript.merge_quoted2 3: " ^ s))
    | "\\" :: _ -> failwith "JavaScript.merge_quoted2 4"
    | s :: tokens -> merge_quoted2 (s :: rev) tokens

let rec merge_quoted3 rev = function
      [] -> failwith "JavaScript.merge_quoted3 1"
    | "'" :: tokens -> String.concat "" (List.rev rev), tokens
    | "\\" :: "'" :: tokens -> merge_quoted3 ("'" :: rev) tokens
    | "\\" :: "\\" :: tokens -> merge_quoted3 ("\\" :: rev) tokens
    | "\\" :: s :: tokens ->
       (match String.get s 0 with
         'b' -> merge_quoted3 ("\b" :: rev) (Xstring.cut_prefix "b" s :: tokens)
	   | 'f' -> merge_quoted3 ("\012" :: rev) (Xstring.cut_prefix "f" s :: tokens)
	   | 'n' -> merge_quoted3 ("\n" :: rev) (Xstring.cut_prefix "n" s :: tokens)
	   | 'r' -> merge_quoted3 ("\r" :: rev) (Xstring.cut_prefix "r" s :: tokens)
	   | 't' -> merge_quoted3 ("\t" :: rev) (Xstring.cut_prefix "t" s :: tokens)
	   | 'u' ->
	      if Xstring.size s < 5 then failwith "JavaScript.merge_quoted3 2" else
	      let t = String.sub s 1 4 in
	      let x = Scanf.sscanf t "%x" (fun y -> y) in
	      let t2 = Xunicode.string_of_uchar x in
	      merge_quoted3 (t2 :: rev) (Xstring.cut_prefix "u0000" s :: tokens)
	   | '/' -> merge_quoted3 ("/" :: rev) (Xstring.cut_prefix "/" s :: tokens)
	   | 'x' -> merge_quoted3 ("\\" :: rev) (s :: tokens)
	   | _ -> failwith ("JavaScript.merge_quoted3 3: " ^ s))
    | "\\" :: _ -> failwith "JavaScript.merge_quoted3 4"
    | s :: tokens -> merge_quoted3 (s :: rev) tokens

let rec merge_quoted rev = function
      [] -> List.rev rev
    | "\"" :: tokens -> let s, tokens = merge_quoted2 [] tokens in merge_quoted ((T s) :: rev) tokens
    | "'" :: tokens -> let s, tokens = merge_quoted3 [] tokens in merge_quoted ((T s) :: rev) tokens
    | x :: tokens -> merge_quoted ((X x) :: rev) tokens

let rec merge_quoted_tail rev = function
      [] -> List.rev rev, []
    | "\"" :: tokens ->
       (try
         let s, tokens = merge_quoted2 [] tokens in merge_quoted_tail ((T s) :: rev) tokens
       with _ -> List.rev rev, "\"" :: tokens)
    | [x] -> List.rev rev, [x]
    | x :: tokens -> merge_quoted_tail ((X x) :: rev) tokens

let make_split s =
  List.rev (Xlist.rev_map (Str.full_split
                         (Str.regexp "\\]\\| \\|\t\\|\n\\|\r\\|\\:\\|{\\|}\\|(\\|)\\|\\+\\|-\\|<\\|>\\|~\\|\\*\\|/\\|,\\|\\^\\|\\$\\|\\\\\\|\\.\\|;\\|!\\|\\?\\||\\|&\\|=\\|\\[\\|'\\|\"\\|\\") s) (function
              Str.Text s -> s
            | Str.Delim s -> s))

let remove_white tokens =
  List.rev (Xlist.fold tokens [] (fun tokens -> function
          X " " -> tokens
        | X "\t" -> tokens
        | X "\n" -> tokens
        | X "\r" -> tokens
        | t -> t :: tokens))

let rec split_op op found rev = function
      (O o) :: l when o = op -> split_op op ((List.rev rev) :: found) [] l
    | s :: l -> split_op op found (s :: rev) l
    | [] -> if rev = [] then List.rev found else List.rev ((List.rev rev) :: found)


let rec parse_expr_tokens = function
    [C "true"] -> JStrue
  | [C "false"] -> JSfalse
  | [C "null"] -> JSnull
  | [D s] -> JSnumber s
  | [O "-";D s] -> JSnumber ("-" ^ s)
  | [T s] -> JSstring s
  | [I s] -> JSident s
  | [B("{","}",l)] -> JSmap(List.rev (Xlist.rev_map (split_op "," [] [] l) parse_entry))
  | [B("[","]",l)] -> JSlist(List.rev (Xlist.rev_map (split_op "," [] [] l) parse_expr_tokens))
  | [C "function"; B("(",")",args); B("{","}",body)] ->
      JSlambda(List.rev (Xlist.rev_map (split_op "," [] [] args) parse_expr_tokens), parse_tokens body)
  | O "!" :: l -> JSneg(parse_expr_tokens l)
  | l ->
      (match split_op "." [] [] l with
        x :: y :: l -> JSdot(Xlist.map (x :: y :: l) parse_expr_tokens)
      | _ ->
        (match List.rev l with
          B("(",")",l1) :: l2 ->
            JScall(parse_expr_tokens (List.rev l2),
              Xlist.map (split_op "," [] [] l1) (fun arg -> parse_expr_tokens arg))
        | _ ->
(*           print_endline ("JavaScript.parse_expr_tokens: " ^ string_of_syntax_list l ^ "\n"); *)
          UnknownExpr (string_of_syntax_list l)))

and parse_entry = function
    T e :: O ":" :: l -> e, parse_expr_tokens l
  | l -> "???", parse_expr_tokens l
(*   | _ -> failwith "JavaScript.parse_entry" *)

and parse_tokens l =
  let ll = split_op ";" [] [] l in
  InstrBlock(Xlist.map ll (function
      C "if" :: B("(",")",l1) :: B("{","}",l2) :: l3 -> If(parse_expr_tokens l1,parse_tokens l2,parse_tokens l3)
    | C "var" :: I i :: O "=" :: l -> Var(i, parse_expr_tokens l)
    | l ->
      (match split_op "=" [] [] l with
         [l1; l2] -> Assign(parse_expr_tokens l1,parse_expr_tokens l2)
      | _ ->
        (match List.rev l with
          B("(",")",l1) :: l2 ->
            Call(parse_expr_tokens (List.rev l2),
              Xlist.map (split_op "," [] [] l1) (fun arg -> parse_expr_tokens arg))
        | _ ->
(*           print_endline ("JavaScript.parse_tokens: " ^ string_of_syntax_list l ^ "\n"); *)
          Unknown (string_of_syntax_list l)))))

let parse s =
  let tokens = make_split s in
  let tokens = merge_quoted [] tokens in
  let tokens = remove_white tokens in
  let l = find_atomic_symbols tokens in
  let l = find_brackets ["(",")";"{","}";"[","]"] [] l in
  let code = parse_tokens l in
  code
(*  Xlist.iter ll (fun l -> print_endline (string_of_syntax_list l));
  raise Not_found*)

let parse_expr s =
  let tokens = make_split s in
  let tokens = merge_quoted [] tokens in
  let tokens = remove_white tokens in
  let l = find_atomic_symbols tokens in
  let l = find_brackets ["(",")";"{","}";"[","]"] [] l in
  let code = parse_expr_tokens l in
  code
(*  Xlist.iter ll (fun l -> print_endline (string_of_syntax_list l));
  raise Not_found*)
