open Xstd

type css =
    CSSdeclarations of string list * string list
  | CSSgroup of string list * css list
  | CSScomment of string
  
type js_expr =
    JStrue
  | JSfalse
  | JSnull
  | JSstring of string
  | JSnumber of string
  | JSmap of (string * js_expr) list
  | JSlist of js_expr list
  | JSneg of js_expr
  | JSdot of js_expr list
  | JSident of string
  | JSlambda of js_expr list * js_instr
  | JScall of js_expr * js_expr list
  | UnknownExpr of string

and js_instr =
    InstrBlock of js_instr list
  | If of js_expr * js_instr * js_instr
  | Assign of js_expr * js_expr
  | Call of js_expr * js_expr list
  | Var of string * js_expr
  | Unknown of string

type html =
    Otag of string * (string * string) list
  | Tag of string * (string * string) list * html list
  | Ctag of string
  | Text of string
  | JavaScript of js_instr
  | JavaScriptExpr of js_expr
  | JSON of Xjson.json
  | CSS of css list
  | Comment of string

type attr_pat =
    Includes of string * string (* label * value *)
  | VarIncludes of string * string (* label * var *)
  
type pat =
    PTag of string * attr_pat list * pat list
  | PText of string
  | VarText of string
    
  
let validate = ref false

let print_stringqmap qmap =
  StringQMap.iter qmap (fun k v -> Printf.printf "%5d %s\n" v k)
  
let print_stringqmap_simple qmap =
  StringQMap.iter qmap (fun k _ -> Printf.printf "%s\n" k)

let rec remove_trailing_white s =
  if s = "" then s else
  if Xstring.check_sufix " " s then
    remove_trailing_white (Xstring.cut_sufix " " s) else
  if Xstring.check_sufix "\n" s then
    remove_trailing_white (Xstring.cut_sufix "\n" s) else
  if Xstring.check_sufix "\r" s then
    remove_trailing_white (Xstring.cut_sufix "\r" s) else
  if Xstring.check_sufix "\t" s then
    remove_trailing_white (Xstring.cut_sufix "\t" s) else
  s
  
let rec remove_heading_white s =
  if s = "" then s else
  if Xstring.check_prefix " " s then
    remove_heading_white (Xstring.cut_prefix " " s) else
  if Xstring.check_prefix "\n" s then
    remove_heading_white (Xstring.cut_prefix "\n" s) else
  if Xstring.check_prefix "\r" s then
    remove_heading_white (Xstring.cut_prefix "\r" s) else
  if Xstring.check_prefix "\t" s then
    remove_heading_white (Xstring.cut_prefix "\t" s) else
  s
  
let remove_white s =
  remove_heading_white (remove_trailing_white s)
