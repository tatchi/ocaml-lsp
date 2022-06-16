module Private = struct
  let win32 = ref Sys.win32
end

type scheme = File

type t =
  { scheme : scheme
  ; path : string
  }

let path t = t.path

let backslash_to_slash =
  Stdune.String.map ~f:(function
    | '\\' -> '/'
    | _ as c -> c)

let slash_to_backslash =
  Stdune.String.map ~f:(function
    | '/' -> '\\'
    | _ as c -> c)

let of_path path =
  let path = if !Private.win32 then backslash_to_slash path else path in
  { scheme = File; path }

let to_path t =
  let path = t.path in
  let path =
    if Stdune.String.length path < 3 then path
    else
      let c0 = path.[0] in
      let c1 = path.[1] in
      let c2 = path.[2] in
      if
        c0 = '/'
        && ((c1 >= 'A' && c1 <= 'Z') || (c1 >= 'a' && c1 <= 'z'))
        && c2 = ':'
      then
        Stdune.String.make 1 (Char.lowercase_ascii c1)
        ^ Stdune.String.drop path 2
      else path
  in

  if !Private.win32 then slash_to_backslash path else path
