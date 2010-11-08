(*
  Python-like string utilities for OCaml

  Baris Metin <baris ! metin.org>
*)

(** Strip string using a list of characters
    @param s Input string
    @param lst List of characters to be removed
    @param ~right Boolean, if true strip from right
 *)
let strip_with_direction s lst ~right =
  let rec strip s lst' =
    match lst' with 
      [] -> s
    | hd::tail -> 
        let len = String.length s in
        let index = if right then (len - 1) else 0 in
        if (String.get s index) = hd then
          let cropped = if right then String.sub s 0 index else String.sub s 1 (len-1) in
          strip cropped lst
        else
          strip s tail
  in
  strip s lst

let lstrip s lst = strip_with_direction s lst ~right:false
let rstrip s lst = strip_with_direction s lst ~right:true
let strip s lst = lstrip (rstrip s lst) lst

(** Check if a string s starts with another m
    @param s Input string
    @param m string to match with
 *)
let startswith s m = 
  let len_s = String.length s in
  let len_m = String.length m in
  if len_s < len_m then false else
  let s' = String.sub s 0 len_m in
  s' = m

(** Check if a string s ends with another m
    @param s Input string
    @param m string to match with
 *)
let endswith s m = 
  let len_s = String.length s in
  let len_m = String.length m in
  if len_s < len_m then false else
  let s' = String.sub s (len_s - len_m) len_m in
  s' = m

