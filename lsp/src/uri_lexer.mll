{
type t =
  { scheme : string option
  ; authority : string
  ; path : string
  }
}

rule path buff= parse
| '/'? (['A'-'Z']|['a'-'z'] as driver_letter) ':' 
      { Buffer.add_char buff (Char.lowercase_ascii driver_letter); Buffer.add_char buff ':'; path1 buff lexbuf }
| [^ '/'] as c { Buffer.add_char buff '/'; Buffer.add_char buff c; path1 buff lexbuf }
| _ as c { Buffer.add_char buff c ; path1 buff lexbuf }
| eof { Buffer.add_char buff '/' ; Buffer.contents buff }
and path1 buf = parse
(* | '\\' { Buffer.add_char buf '/' ; path1 buf lexbuf } *)
(* | "%5" ['c' 'C'] { Buffer.add_char buf '/' ; path1 buf lexbuf }
| "%3" ['a' 'A'] { Buffer.add_char buf ':' ; path1 buf lexbuf }
| "%3" ['d' 'D'] { Buffer.add_char buf '=' ; path1 buf lexbuf }
| "%3" ['f' 'F'] { Buffer.add_char buf '?' ; path1 buf lexbuf }
| "%20" { Buffer.add_char buf ' ' ; path1 buf lexbuf } *)
| _ as c { Buffer.add_char buf c ; path1 buf lexbuf }
| eof { Buffer.contents buf }

and uri buff = parse
| ([^ ':']+) as scheme ':' { uri1 buff (Some scheme) lexbuf }
| "" { uri1 buff None lexbuf }
and uri1 buff scheme = parse
| "//" ([^ '/']* as authority) { uri2 buff scheme authority lexbuf }
| "" { uri2 buff scheme "" lexbuf }
and uri2 buff scheme authority = parse
| "" { { scheme ; authority ; path = path buff lexbuf } }

{
  let escape_path s =
    let lexbuf = Lexing.from_string s in
    path (Buffer.create 12) lexbuf

  let of_string s =
    let lexbuf = Lexing.from_string s in
    uri (Buffer.create 12) lexbuf
}
