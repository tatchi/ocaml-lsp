module Uri = Lsp.Uri1

let run_with_modes f =
  print_endline "Unix:";
  Uri.Private.win32 := false;
  f ();
  print_endline "Windows:";
  Uri.Private.win32 := true;
  f ()

let%expect_test "test of_path -> to_path" =
  let test_of_path =
    let test s =
      let uri = Uri.of_path s in
      Printf.printf "%s -> %s\n" s (Uri.to_path uri)
    in
    fun uris -> run_with_modes (fun () -> List.iter test uris)
  in
  test_of_path
    [ "c:/win/path"
    ; "c:/win/path/"
    ; "C:/win/path"
    ; "/c:/win/path"
    ; "./c/win/path"
    ; "c:\\win\\path"
    ; "c:\\win/path"
    ; ""
    ; "a.file"
    ; "\\\\shäres\\path\\c#\\plugin.json"
    ; "c:\\test with %25\\path"
    ];
  [%expect
    {|
    Unix:
    c:/win/path -> c:/win/path
    c:/win/path/ -> c:/win/path/
    C:/win/path -> c:/win/path
    /c:/win/path -> c:/win/path
    ./c/win/path -> /./c/win/path
    c:\win\path -> c:\win\path
    c:\win/path -> c:\win/path
     -> /
    a.file -> /a.file
    \\shäres\path\c#\plugin.json -> /\\shäres\path\c#\plugin.json
    c:\test with %25\path -> c:\test with %25\path
    Windows:
    c:/win/path -> c:\win\path
    c:/win/path/ -> c:\win\path\
    C:/win/path -> c:\win\path
    /c:/win/path -> c:\win\path
    ./c/win/path -> \.\c\win\path
    c:\win\path -> c:\win\path
    c:\win/path -> c:\win\path
     -> \
    a.file -> \a.file
    \\shäres\path\c#\plugin.json -> \\shäres\path\c#\plugin.json
    c:\test with %25\path -> c:\test with %25\path
    |}]
