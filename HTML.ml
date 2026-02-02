open WWWtypes
open Xstd

let rec map t f =
  let t = match t with 
      Tag(s,a,l) -> Tag(s,a,List.rev (Xlist.rev_map l (fun t -> map t f)))
    | t -> t in
  f t

let rec fold t s f =
  let s = match t with 
      Tag(_,_,l) -> Xlist.fold l s (fun s t -> fold t s f)
    | _ -> s in
  f s t
  
let string_of_html_attributes l =
  if l = [] then "" else
  " " ^ String.concat " " (Xlist.map l (fun (k,v) -> k ^ "=\"" ^ v ^ "\""))
  
let rec string_of_html_fmt spaces = function
    Text s -> spaces ^ s ^ "\n"
  | Comment s -> spaces ^ s ^ "\n"
  | Tag(t,a,[]) -> spaces ^ "<" ^ t ^ string_of_html_attributes a ^ "/>" ^ "\n"
  | Tag(t,a,c) -> 
      (*"x" ^*) string_of_html_fmt spaces (Otag(t,a)) ^ 
      String.concat "" (List.rev (Xlist.rev_map c (string_of_html_fmt (spaces ^ "  ")))) ^ 
      (*"x" ^*) string_of_html_fmt spaces (Ctag t)
  | Otag(t,a) -> (*"!" ^*) spaces ^ "<" ^ t ^ string_of_html_attributes a ^ ">" ^ "\n"
  | Ctag t -> (*"!" ^*) spaces ^ "</" ^ t ^ ">" ^ "\n"
  | JSON json -> spaces ^ Xjson.json_to_string_fmt2 spaces json ^ "\n"
  | JavaScript js -> JavaScript.js_to_string_fmt spaces js ^ "\n"
  | JavaScriptExpr js -> spaces ^ JavaScript.js_expr_to_string_fmt spaces js ^ "\n"
  | CSS l -> String.concat "" (List.rev (Xlist.rev_map l (fun t -> CSS.string_of_css_fmt spaces t ^ "\n")))
  
let rec string_of_html_fmt_bounded n spaces = function
    Text s -> spaces ^ s ^ "\n"
  | Comment s -> spaces ^ s ^ "\n"
  | Tag(t,a,[]) -> spaces ^ "<" ^ t ^ string_of_html_attributes a ^ "/>" ^ "\n"
  | Tag(t,a,_) when n = 0 -> spaces ^ "<" ^ t ^ string_of_html_attributes a ^ ">...</" ^ t ^ ">\n"
  | Tag(t,a,c) -> 
      (*"x" ^*) string_of_html_fmt_bounded n spaces (Otag(t,a)) ^ 
      String.concat "" (List.rev (Xlist.rev_map c (string_of_html_fmt_bounded (n-1) (spaces ^ "  ")))) ^ 
      (*"x" ^*) string_of_html_fmt_bounded n spaces (Ctag t)
  | Otag(t,a) -> (*"!" ^*) spaces ^ "<" ^ t ^ string_of_html_attributes a ^ ">" ^ "\n"
  | Ctag t -> (*"!" ^*) spaces ^ "</" ^ t ^ ">" ^ "\n"
  | JSON json -> spaces ^ Xjson.json_to_string_fmt2 spaces json ^ "\n"
  | JavaScript js -> spaces ^ JavaScript.js_to_string_fmt spaces js ^ "\n"
  | JavaScriptExpr js -> spaces ^ JavaScript.js_expr_to_string_fmt spaces js ^ "\n"
  | CSS l -> String.concat "" (List.rev (Xlist.rev_map l (fun t -> CSS.string_of_css_fmt spaces t ^ "\n")))
  
let rec merge_dquoted2 rev = function
    "\"" :: l -> String.concat "" (List.rev ("\"" :: rev)), l
  | s :: l -> merge_dquoted2 (s :: rev) l
  | [] -> failwith "merge_dquoted2"
        
let rec merge_dquoted rev = function
    "\"" :: l -> 
(*       (try  *)
(*         print_endline ("merge_dquoted 1: " ^ (String.concat "|" (Xlist.prefix 10 l))); *)
        let s,l = merge_dquoted2 ["\""] l in 
(*         print_endline ("merge_dquoted 2: " ^ s); *)
        merge_dquoted (s :: rev) l
(*      with Failure "merge_dquoted2" -> (*merge_dquoted ("\"" :: rev) l*)(
        print_endline ("XXXX " ^ String.concat "" (Xlist.prefix 5 ("\"" :: l))); 
        failwith "merge_dquoted")) *)
  | s :: l -> merge_dquoted (s :: rev) l
  | [] -> List.rev rev
  
let rec merge_quoted2 rev = function
    "'" :: l -> String.concat "" (List.rev ("'" :: rev)), l
  | s :: l -> merge_quoted2 (s :: rev) l
  | [] -> failwith "merge_quoted2"
        
let rec merge_quoted rev = function
    "'" :: l -> 
(*       (try  *)
(*         print_endline ("merge_quoted 1: " ^ (String.concat "|" (Xlist.prefix 10 l))); *)
        let s,l = merge_quoted2 ["'"] l in 
(*         print_endline ("merge_quoted 2: " ^ s); *)
        merge_quoted (s :: rev) l
(*      with Failure "merge_quoted2" -> (*merge_quoted ("'" :: rev) l*)(
        print_endline ("XXXX " ^ String.concat "" (Xlist.prefix 5 ("'" :: l))); 
        failwith "merge_quoted")) *)
  | s :: l -> merge_quoted (s :: rev) l
  | [] -> List.rev rev
  
let rec merge_slash rev = function
    "<" :: _ | ">" :: _  -> failwith "merge_slash"
  | s :: t :: l when 
      s = " " || s = "\t" || s = "\r" || s = "\n" || s = "=" || 
      t = " " || t = "=" || t = "\t" || t = "\r" || t = "\n" -> merge_slash (s :: rev) (t :: l)
  | s :: "/" :: "/" :: t :: l -> merge_slash rev ((s ^ "//" ^ t) :: l)
  | s :: "/" :: t :: l -> merge_slash rev ((s ^ "/" ^ t) :: l)
  | s :: t :: l -> 
(*       print_endline ("merge_slash: " ^ s ^ " " ^ t); *)
      merge_slash (s :: rev) (t :: l)
  | [s] -> List.rev (s :: rev)
  | [] -> List.rev rev

(*let rec merge_ordinary rev = function
    s :: l when s = "<" || s = ">" || s = "/" -> merge_ordinary (s :: rev) l
  | t :: s :: l when s = "<" || s = ">" || s = "/" -> merge_ordinary (s :: t :: rev) l
  | s :: t :: l -> merge_ordinary rev ((s ^ t) :: l)
  | [s]  -> List.rev (s :: rev)
  | [] -> List.rev rev*)
  
let remove_quots s =
  if Xstring.check_prefix "\"" s && Xstring.check_sufix "\"" s && Xstring.size s > 1 then
    Xstring.cut_prefix "\"" (Xstring.cut_sufix "\"" s) else
  if Xstring.check_prefix "'" s && Xstring.check_sufix "'" s && Xstring.size s > 1 then
    Xstring.cut_prefix "'" (Xstring.cut_sufix "'" s)
  else s
    
let rec parse_tag_attrs found = function
    s :: "=" :: t :: l when s <> " " && s <> "=" && t <> " " && t <> "=" -> 
      parse_tag_attrs ((s,remove_quots t) :: found) l
  | "allowfullscreen" :: l  -> parse_tag_attrs (("allowfullscreen","") :: found) l
  | "async" :: l  -> parse_tag_attrs (("async","") :: found) l
  | "crossorigin" :: l  -> parse_tag_attrs (("crossorigin","") :: found) l
  | "defer" :: l  -> parse_tag_attrs (("defer","") :: found) l
  | "x-cloak" :: l  -> parse_tag_attrs (("x-cloak","") :: found) l
  | "x-clock" :: l  -> parse_tag_attrs (("x-clock","") :: found) l
  | "value" :: l  -> parse_tag_attrs (("value","") :: found) l
  | "disabled" :: l  -> parse_tag_attrs (("disabled","") :: found) l
  | "data-filters" :: l  -> parse_tag_attrs (("data-filters","") :: found) l
  | "data-results" :: l  -> parse_tag_attrs (("data-results","") :: found) l
  | "data-loadmore" :: l  -> parse_tag_attrs (("data-loadmore","") :: found) l
  | "data-player" :: l  -> parse_tag_attrs (("data-player","") :: found) l
  | "data-option" :: l  -> parse_tag_attrs (("data-option","") :: found) l
  | "hidden" :: l  -> parse_tag_attrs (("hidden","") :: found) l
  | "style" :: l  -> parse_tag_attrs (("style","") :: found) l
  | "href" :: l  -> parse_tag_attrs (("href","") :: found) l
  | "readonly" :: l  -> parse_tag_attrs (("readonly","") :: found) l
  | "itemscope" :: l  -> parse_tag_attrs (("itemscope","") :: found) l
  | [] -> List.rev found
  | l -> print_endline ("parse_tag_attrs: " ^ String.concat " " (Xlist.prefix 10 l)); List.rev found
  
let rec remove_white_tokens rev = function
  | s :: l when s = " " || s = "\t" || s = "\r" || s = "\n" -> remove_white_tokens rev l
  | s :: l -> remove_white_tokens (s :: rev) l
  | [] -> List.rev rev

  
(*let parse_tag tag =
  let l = Xstring.full_split " \\|=\\|\"" tag in
  match l with
    [s] when s <> " " && s <> "=" && s <> "\"" -> s,[]
  | s :: " " :: l when s <> " " && s <> "=" && s <> "\"" -> s, parse_tag_attrs [] (merge_dquoted [] l)
  | _ -> failwith ("parse_tag: " ^ tag)
  
let rec find_script_contents rev = function
    "<" :: "/" :: "script" :: ">" :: l -> String.concat "" (List.rev rev), "<" :: "script" :: "/" :: ">" :: l
  | s :: l -> find_script_contents (s :: rev) l
  | [] -> failwith "find_script_contents"
  
let rec parse_html_rec rev = function
    "<" :: tag :: ">" :: l -> 
      if Xstring.check_prefix "!DOCTYPE " tag then 
        parse_html_rec ((Text ("<" ^ tag ^ ">")) :: rev) l else
      if Xstring.check_prefix "!-- " tag && Xstring.check_sufix " --" tag then
        parse_html_rec ((Text ("<" ^ tag ^ ">")) :: rev) l else (* FIXME: może lepiej Comment *)
      let a,b = parse_tag tag in 
      if a = "script" then 
        let script, l = find_script_contents [] l in
        if script = "" then parse_html_rec ((Otag(a,b)) :: rev) l
        else parse_html_rec ((Text script) :: (Otag(a,b)) :: rev) l
      else parse_html_rec ((Otag(a,b)) :: rev) l
  | "<" :: tag :: "/" :: ">" :: l -> let a,b = parse_tag tag in parse_html_rec ((Tag(a,b,[])) :: rev) l
  | "<" :: "/" :: tag :: ">" :: l -> let a,b = parse_tag tag in parse_html_rec ((Ctag(a,b)) :: rev) l
  | s :: l -> 
      let rev = match rev with
          Text t :: r -> Text(t ^ s) :: r
        | _ -> (Text s) :: rev in
      parse_html_rec rev l
  | [] -> List.rev rev
(*   | l -> print_endline ("\"" ^ String.concat "\" :: \"" (Xlist.prefix 10 l) ^ "\"") *)*)

let recognize_tags = function
    ["/"; tag] -> Ctag tag
  | [tag] -> Otag(tag,[])
  | "!DOCTYPE" :: attrs | "!doctype" :: attrs -> 
(*       print_endline ("recognize_tags 1: " ^ (String.concat "|" (Xlist.prefix 10 attrs))); *)
      Otag("!DOCTYPE",List.rev (Xlist.fold (merge_quoted [] (merge_dquoted [] attrs)) [] (fun l -> function
          " " | "\n" | "\r" | "\t" -> l
        | "/" -> failwith "recognize_tags !DOCTYPE"
        | s -> (s,"") :: l)))
  | "?xml" :: attrs when List.hd (List.rev attrs) = "?" -> 
(*       print_endline ("recognize_tags 0: " ^ String.concat " " ("?xml" :: attrs)); *)
      Tag("?xml",parse_tag_attrs [] (remove_white_tokens [] (merge_slash [] (merge_quoted [] (merge_dquoted [] (List.rev (List.tl (List.rev attrs))))))),[])
  | tag :: attrs when List.hd (List.rev attrs) = "/" -> 
(*       print_endline ("recognize_tags 1: " ^ String.concat " " (tag :: attrs)); *)
      Tag(tag,parse_tag_attrs [] (remove_white_tokens [] (merge_slash [] (merge_quoted [] (merge_dquoted [] (List.rev (List.tl (List.rev attrs))))))),[])
  | tag :: attrs ->  
(*       print_endline ("recognize_tags 2: " ^ String.concat " " (tag :: attrs)); *)
      Otag(tag,parse_tag_attrs [] (remove_white_tokens [] (merge_slash [] (merge_quoted [] (merge_dquoted [] attrs)))))
  | _ -> failwith "recognize_tags"
  
type t =
    T of string list
  | C of string list
  | O of string list
  
(* FIXME: pomijam eskejpowanie backslasha *)
(*let is_escaped = function
    s :: _ -> Xstring.check_sufix "\\" s
  | [] -> false*)
  
let is_script_begin = function
    ["script"] -> true
  | "script" :: l -> List.hd (List.rev l) <> "/"
  | _ :: _ -> false
  | [] -> failwith "is_script_begin"

let is_style_begin = function
    ["style"] -> true
  | "style" :: l -> List.hd (List.rev l) <> "/"
  | _ :: _ -> false
  | [] -> failwith "is_style_begin"

let rec find_comment rev = function
    a :: ">" :: l when Xstring.check_sufix "--" a -> List.rev (">" :: a :: rev), l
  | s :: l -> find_comment (s :: rev) l
  | [] -> failwith "find_comment"
  
let rec find_dquote_finish rev = function
    "\"" :: l -> (*if is_escaped rev then find_dquote_finish ("\"" :: rev) l else*) 
      "\"" :: rev, l (* UWAGA: w htmlu nie ma eskejpowania backslashem *)
  | s :: l -> find_dquote_finish (s :: rev) l
  | [] -> raise Not_found
  
let rec find_quote_finish rev = function
    "'" :: l -> (*if is_escaped rev then find_quote_finish ("'" :: rev) l else*) 
      "'" :: rev, l (* UWAGA: w htmlu nie ma eskejpowania backslashem *)
  | s :: l -> find_quote_finish (s :: rev) l
  | [] -> raise Not_found
  
let rec find_tag_finish rev = function
    ">" :: l -> List.rev rev, l
  | "\"" :: l -> 
      (try let rev,l = find_dquote_finish ("\"" :: rev) l in find_tag_finish rev l
      with Not_found -> failwith ("find_tag_finish: " ^ String.concat "" ("\"" :: l)))
  | "'" :: l -> 
      (try let rev,l = find_quote_finish ("'" :: rev) l in find_tag_finish rev l
      with Not_found -> failwith ("find_tag_finish: " ^ String.concat "" ("'" :: l)))
  | s :: l -> find_tag_finish (s :: rev) l
  | [] -> failwith "find_tag_finish"
  
let rec find_script rev = function
    "<" :: "/" :: "script" :: ">" :: l -> List.rev rev, "<" :: "/" :: "script" :: ">" :: l
  | s :: l -> find_script (s :: rev) l
  | [] -> failwith "find_script"
  
let rec find_style rev = function
    "<" :: "/" :: "style" :: ">" :: l -> List.rev rev, "<" :: "/" :: "style" :: ">" :: l
  | s :: l -> find_style (s :: rev) l
  | [] -> failwith "find_style"
  
let rec find_other rev = function
    "<" :: l -> List.rev rev, "<" :: l
  | s :: l -> find_other (s :: rev) l
  | [] -> List.rev rev, [] (*failwith ("find_other: " ^ String.concat "" (List.rev rev))*)
  
let rec merge_tags rev = function
    "<" :: a :: l when Xstring.check_prefix "!--" a -> let s,l = find_comment ["<"] (a :: l) in merge_tags (C s :: rev) l
  | "<" :: l -> 
      let s,l = find_tag_finish [] l in
      if is_script_begin s then 
        let script,l = find_script [] l in
        if script = [] then merge_tags (T s :: rev) l else
        merge_tags (O script :: T s :: rev) l else
      if is_style_begin s then
        let style,l = find_style [] l in
        if style = [] then merge_tags (T s :: rev) l else
        merge_tags (O style :: T s :: rev) l else
      merge_tags (T s :: rev) l
  | s :: l -> let s,l = find_other [] (s :: l) in merge_tags (O s :: rev) l
  | [] -> List.rev rev
  
let known_tags = ref StringSet.empty
let known_dattrs = ref StringSet.empty
let known_attrs = ref StringSet.empty

let initialize () =
  if !validate then (
    known_tags := StringSet.of_list (File.load_lines "data/known_tags.tab");
    known_dattrs := StringSet.of_list (File.load_lines "data/known_dattrs.tab");
    known_attrs := StringSet.of_list (File.load_lines "data/known_attrs.tab"))
  
let validate_tags filename l =
  let tqmap,lqmap,dqmap = Xlist.fold l (StringQMap.empty,StringQMap.empty,StringQMap.empty) (fun (tqmap,lqmap,dqmap) -> function
      Tag(t,l,[]) | Otag(t,l) -> 
        if t = "!DOCTYPE" then 
          tqmap, lqmap, Xlist.fold l dqmap (fun dqmap (a,_) -> 
            if StringSet.mem !known_dattrs a then dqmap else StringQMap.add dqmap a) else
        (if StringSet.mem !known_tags t then tqmap else StringQMap.add tqmap t),
        (Xlist.fold l lqmap (fun lqmap (a,_) -> 
            if StringSet.mem !known_attrs a then lqmap else StringQMap.add lqmap a)), dqmap            
    | Ctag t -> 
        (if StringSet.mem !known_tags t then tqmap else StringQMap.add tqmap t), lqmap, dqmap            
    | Tag _ -> failwith "validate_tags"
    | CSS _ | JavaScript _ | JavaScriptExpr _ | JSON _ | Text _ | Comment _ -> tqmap,lqmap,dqmap) in
  if StringQMap.size tqmap > 0 then (
    print_endline ("Not validated tags in file " ^ filename ^ ":");
    print_stringqmap_simple tqmap);
  if StringQMap.size lqmap > 0 then (
    print_endline ("Not validated attributes in file " ^ filename ^ ":");
    print_stringqmap_simple lqmap);
  if StringQMap.size dqmap > 0 then (
    print_endline ("Not validated !DOCTYPE attributes in file " ^ filename ^ ":");
    print_stringqmap_simple dqmap);
  ()  
  
let rec wrap_ctag rev = function
    Ctag s :: l -> rev, Tag("",[],[Ctag s]) :: l
  | t :: l -> wrap_ctag (t :: rev) l
  | [] -> rev, []
  
let unwrap_ctags t =
  map t (function
      Tag("",[],[Ctag s]) -> Ctag s
    | t -> t)
  
let rec find_content tag rev = function
    Otag(s,args) :: l when s = tag -> args, rev, l
  | Ctag _ :: _ -> raise Not_found
  | t :: l -> find_content tag (t :: rev) l
  | [] -> raise Not_found

let rec close_tags_rec is_changed rev = function
    Ctag tag :: l ->
      (try 
        let args,content,l = find_content tag [] l in
        close_tags_rec true ((Tag(tag,args,content)) :: rev) l
      with Not_found -> close_tags_rec is_changed ((Ctag tag) :: rev) l)
  | t :: l -> close_tags_rec is_changed (t :: rev) l
  | [] -> List.rev rev, is_changed

(* Możnaby wprowadzić zamykanie tagów typu div (i span) na początku *)
let rec close_tags l =
  let l,is_changed = close_tags_rec false [] l in
  if is_changed then close_tags l else 
  let a,b = wrap_ctag [] (List.rev l) in
  if b = [] then l else
    close_tags (List.rev b @ a)

let parse_scripts t =
  map t (function
      Tag("script",[],[Text s]) ->
        (try Tag("script",[],[JavaScript (JavaScript.parse s)])
        with Not_found -> print_endline ("\n\n\n" ^ s ^ "\n\n\n"); Tag("script",[],[Text s]))
    | Tag("script",["crossorigin",cr],[Text s]) ->
        (try Tag("script",["crossorigin",cr],[JavaScript (JavaScript.parse s)])
        with Not_found -> print_endline ("\n\n\n" ^ s ^ "\n\n\n"); Tag("script",["crossorigin",cr],[Text s]))
    | Tag("script",["data-id",di;"nonce",nn],[Text s]) ->
        (try Tag("script",["data-id",di;"nonce",nn],[JavaScript (JavaScript.parse s)])
        with Not_found -> print_endline ("\n\n\n" ^ s ^ "\n\n\n"); Tag("script",["data-id",di;"nonce",nn],[Text s]))
    | Tag("script",["nonce",nn],[Text s]) ->
        (try Tag("script",["nonce",nn],[JavaScript (JavaScript.parse s)])
        with Not_found -> print_endline ("\n\n\n" ^ s ^ "\n\n\n"); Tag("script",["nonce",nn],[Text s]))
    | Tag("script",["type","application/ld+json"],[Text s]) ->
        (try Tag("script",["type","application/ld+json"],[JavaScriptExpr (JavaScript.parse_expr s)])
        with Not_found -> print_endline ("\n\n\n" ^ s ^ "\n\n\n"); Tag("script",[],[Text s]))
    | Tag("script",["src",src],[]) -> Tag("script",["src",src],[])
    | Tag("script",["src",src;"nonce",nn],[]) -> Tag("script",["src",src;"nonce",nn],[])
    | Tag("script",["src",src;"crossorigin",nn],[]) -> Tag("script",["src",src;"crossorigin",nn],[])
    | Tag("script",["src",src;"onerror",onerror],[]) ->
        (try Tag("script",["src",src],[Tag("onerror",[],[JavaScript (JavaScript.parse onerror)])])
        with Not_found -> print_endline ("\n\n\n" ^ onerror ^ "\n\n\n"); Tag("script",["src",src;"onerror",onerror],[]))
    | Tag("script",["src",src;"crossorigin",nn;"onerror",onerror],[]) ->
        (try Tag("script",["src",src;"crossorigin",nn],[Tag("onerror",[],[JavaScript (JavaScript.parse onerror)])])
        with Not_found -> print_endline ("\n\n\n" ^ onerror ^ "\n\n\n"); Tag("script",["src",src;"onerror",onerror],[]))
    | Tag("script",_,_) as tag -> print_endline ("Unknown script: \n" ^ string_of_html_fmt "  " tag); tag
    | t -> t)


let rec validate_structure_rec rev = function
    Tag(_,_,l) -> Xlist.fold l rev validate_structure_rec
  | Ctag t -> t :: rev
  | _ -> rev

let validate_structure filename t =
  let l = validate_structure_rec [] t in
  if l <> [] then (
    print_endline ("Not validated structure in file " ^ filename ^ ":");
    Xlist.iter l print_endline)

let parse filename data =
  let l = Xstring.full_split "<\\|>\\|/\\|\"\\|'\\| \\|=\\|\t\\|\r\\|\n" data in
(*  let l = merge_dquoted [] l in
  let l = merge_ordinary [] l in
  let l = parse_html_rec [] l in*)
  let l = merge_tags [] l in
  let l = List.rev (Xlist.fold l [] (fun found -> function 
      T l -> recognize_tags l :: found
    | C l -> Comment (String.concat "" l) :: found
    | O l -> let s = remove_white (String.concat "" l) in if s = "" then found else Text s :: found)) in
  if !validate then validate_tags filename l;
  let l = List.rev (close_tags (List.rev l)) in
  let t = unwrap_ctags (Tag("ROOT",[],l)) in
  let t = parse_scripts t in
  if !validate then validate_structure filename t;
  t

let find_hrefs t =
  fold t [] (fun l -> function
      Tag(s,a,_) | Otag(s,a) when s = "a" || s = "link" -> (try (Xlist.assoc a "href") :: l with Not_found -> l)
    | _ -> l)
   
let find_img_srcs t =
  fold t [] (fun l -> function
      Tag(s,a,_) | Otag(s,a) when s = "img" -> (try (Xlist.assoc a "src") :: l with Not_found -> l)
    | _ -> l)
   
let find_meta_property_contents prop t =
  fold t [] (fun l -> function
      Tag(s,a,_) | Otag(s,a) when s = "meta" -> 
        let property = try Xlist.assoc a "property" with Not_found -> "" in
        if property <> prop then l else
        (try Xlist.assoc a "content" :: l with Not_found -> l)
    | _ -> l)
    
let find_forms t =
  fold t [] (fun l -> function
      Tag("form",a,v) -> (Tag("form",a,v)) :: l
    | Otag("form",_) -> failwith ("find_forms: incomplete form")
    | _ -> l)
   
let find_inputs t =
  fold t [] (fun l -> function
      Tag("input",a,v) -> (Tag("input",a,v)) :: l
    | Otag("input",a) -> (Otag("input",a)) :: l
    | _ -> l)
   
let find_subtrees tag attrs t =
  fold t [] (fun l -> function
      Tag(s,a,v) when s = tag && a = attrs -> (Tag(s,a,v)) :: l
    | Otag(s,a) when s = tag && a = attrs -> failwith ("find_subtrees: incomplete tag")
    | _ -> l)

let find_video t =
  fold t [] (fun l -> function
      Tag("video",_,_) as t -> t :: l
    | _ -> l)
   
let find_java_scripts t =
  fold t [] (fun l -> function
      Tag("script",_,[JavaScript t]) -> t :: l
    | _ -> l)

let match_attributes vars pats attrs =
  Xlist.fold pats [vars] (fun varsl -> function
      Includes(ik,iv) -> Xlist.fold attrs [] (fun found (k,v) ->
        if k = ik && v = iv then varsl @ found else found)
    | VarIncludes(ik,vv) -> Xlist.fold attrs [] (fun found (k,v) ->
        if k = ik then Xlist.fold varsl found (fun found vars -> 
          if StringMap.mem vars vv then found else (StringMap.add vars vv v) :: found) 
        else found))
    
let rec match_pattern_rec vars = function
    PTag(ps,pa,pv), Tag(s,a,v) when ps = s -> (* pv powinny się wykluczać nawzajem *)
      let found = List.flatten (Xlist.rev_map (match_attributes vars pa a) (fun vars0 ->
        Xlist.fold pv [vars0] (fun found1 p ->
          List.flatten (Xlist.rev_map found1 (fun vars1 -> 
            List.flatten (Xlist.rev_map v (fun v -> match_pattern_rec vars1 (p,v)))))))) in
(*            Xlist.fold v [vars1] (fun found2 v -> 
              List.flatten (Xlist.rev_map found2 (fun vars2 -> match_pattern_rec vars2 (p,v)))(* @ found2*))))))) in*)
(*      if found <> [] then print_endline ("\nPTag(" ^ ps ^ ")");
      if found <> [] then print_endline (string_of_html_fmt "" (Tag(s,a,v)));
      if found <> [] then Printf.printf "%d matching found\n" (Xlist.size found);*)
      found
  | PTag(ps,pa,_), Otag(s,a) when ps = s && match_attributes vars pa a <> [] -> failwith ("match_pattern_rec: incomplete tag")
  | PText ps, Text s when ps = s -> [vars]
  | VarText vs, Text s -> if StringMap.mem vars vs then [] else [StringMap.add vars vs s]
  | _ -> []
    
let match_pattern t pat =
  fold t [] (fun l t -> (match_pattern_rec StringMap.empty (pat,t)) @ l)
    
    
