module Uri = Lsp.Uri

let run_with_modes f =
  print_endline "Unix:";
  Uri.Private.win32 := false;
  f ();
  print_endline "Windows:";
  Uri.Private.win32 := true;
  f ()

let%expect_test "of_path -> to_string (file#toString)" =
  let test_of_path_to_string =
    let test path =
      let uri = Uri.of_path path in
      Printf.printf "%s -> %s\n" path (Uri.to_string uri)
    in
    fun paths -> run_with_modes (fun () -> List.iter test paths)
  in
  test_of_path_to_string
    [ "c:/win/path"
    ; "C:/win/path"
    ; "c:/win/path/"
    ; "/c:/win/path"
    ; "c:\\win\\path"
    ; "c:\\win/path"
    ; "\\\\localhost\\c$\\GitDevelopment\\express"
    ; "c:\\test with %\\path"
    ; "c:\\test with %25\\path"
    ; "c:\\test with %25\\c#code"
    ; "\\\\shäres\\path\\c#\\plugin.json"
    ; "a.file"
    ; "/Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js"
    ];
  [%expect
    {|
    Unix:
    c:/win/path -> file:///c%3A/win/path
    C:/win/path -> file:///c%3A/win/path
    c:/win/path/ -> file:///c%3A/win/path/
    /c:/win/path -> file:///c%3A/win/path
    c:\win\path -> file:///c%3A%5Cwin%5Cpath
    c:\win/path -> file:///c%3A%5Cwin/path
    \\localhost\c$\GitDevelopment\express -> file:///%5C%5Clocalhost%5Cc%24%5CGitDevelopment%5Cexpress
    c:\test with %\path -> file:///c%3A%5Ctest%20with%20%25%5Cpath
    c:\test with %25\path -> file:///c%3A%5Ctest%20with%20%2525%5Cpath
    c:\test with %25\c#code -> file:///c%3A%5Ctest%20with%20%2525%5Cc%23code
    \\shäres\path\c#\plugin.json -> file:///%5C%5Csh%C3%A4res%5Cpath%5Cc%23%5Cplugin.json
    a.file -> file:///a.file
    /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js -> file:///Users/jrieken/Code/_samples/18500/M%C3%B6del%20%2B%20Other%20Th%C3%AEng%C3%9F/model.js
    Windows:
    c:/win/path -> file:///c%3A/win/path
    C:/win/path -> file:///c%3A/win/path
    c:/win/path/ -> file:///c%3A/win/path/
    /c:/win/path -> file:///c%3A/win/path
    c:\win\path -> file:///c%3A/win/path
    c:\win/path -> file:///c%3A/win/path
    \\localhost\c$\GitDevelopment\express -> file://localhost/c%24/GitDevelopment/express
    c:\test with %\path -> file:///c%3A/test%20with%20%25/path
    c:\test with %25\path -> file:///c%3A/test%20with%20%2525/path
    c:\test with %25\c#code -> file:///c%3A/test%20with%20%2525/c%23code
    \\shäres\path\c#\plugin.json -> file://sh%C3%A4res/path/c%23/plugin.json
    a.file -> file:///a.file
    /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js -> file:///Users/jrieken/Code/_samples/18500/M%C3%B6del%20%2B%20Other%20Th%C3%AEng%C3%9F/model.js
    |}]

let%expect_test "of_path -> to_path (file#fsPath)" =
  let test_of_path_to_path =
    let test path =
      let uri = Uri.of_path path in
      Printf.printf "%s -> %s\n" path (Uri.to_path uri)
    in
    fun paths -> run_with_modes (fun () -> List.iter test paths)
  in
  test_of_path_to_path
    [ "c:/win/path"
    ; "c:/win/path/"
    ; "C:/win/path"
    ; "/c:/win/path"
    ; "./c/win/path"
    ; "c:\\win\\path"
    ; "c:\\win/path"
    ; "\\\\localhost\\c$\\GitDevelopment\\express"
    ; "\\\\shares"
    ; "\\\\shäres\\path\\c#\\plugin.json"
    ; "c:\\test with %\\path"
    ; "c:\\test with %25\\c#code"
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
    \\localhost\c$\GitDevelopment\express -> /\\localhost\c$\GitDevelopment\express
    \\shares -> /\\shares
    \\shäres\path\c#\plugin.json -> /\\shäres\path\c#\plugin.json
    c:\test with %\path -> c:\test with %\path
    c:\test with %25\c#code -> c:\test with %25\c#code
    Windows:
    c:/win/path -> c:\win\path
    c:/win/path/ -> c:\win\path\
    C:/win/path -> c:\win\path
    /c:/win/path -> c:\win\path
    ./c/win/path -> \.\c\win\path
    c:\win\path -> c:\win\path
    c:\win/path -> c:\win\path
    \\localhost\c$\GitDevelopment\express -> \\localhost\c$\GitDevelopment\express
    \\shares -> \
    \\shäres\path\c#\plugin.json -> \\shäres\path\c#\plugin.json
    c:\test with %\path -> c:\test with %\path
    c:\test with %25\c#code -> c:\test with %25\c#code
    |}]

let%expect_test "of_string -> to_path" =
  let test_of_string_to_path =
    let test s =
      let uri = Uri.t_of_yojson (`String s) in
      Printf.printf "%s -> %s\n" s (Uri.to_path uri)
    in
    fun s -> run_with_modes (fun () -> List.iter test s)
  in
  test_of_string_to_path
    [ "file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp"
    ; "file://shares/pröjects/c%23/#l12"
    ; "file:///_:/path"
    ; "file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins"
    ];
  [%expect
    {|
    Unix:
    file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp -> /
    file://shares/pröjects/c%23/#l12 -> //shares/pröjects/c#/
    file:///_:/path -> /_:/path
    file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins -> c:/Source/Zürich or Zurich (ˈzjʊərɪk,/Code/resources/app/plugins
    Windows:
    file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp -> \
    file://shares/pröjects/c%23/#l12 -> \\shares\pröjects\c#\
    file:///_:/path -> \_:\path
    file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins -> c:\Source\Zürich or Zurich (ˈzjʊərɪk,\Code\resources\app\plugins
    |}]

let%expect_test "of_string -> to_string" =
  let test_of_string_to_string =
    let test s =
      let uri = Uri.t_of_yojson (`String s) in
      Printf.printf "%s -> %s\n" s (Uri.to_string uri)
    in
    fun s -> run_with_modes (fun () -> List.iter test s)
  in
  test_of_string_to_string
    [ "file://shares/pröjects/c%23/#l12"
    ; "file://sh%c3%a4res/path"
    ; "untitled:c:/Users/jrieken/Code/abc.txt"
    ; "untitled:C:/Users/jrieken/Code/abc.txt"
    ; "/Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js"
    ; "file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins"
    ; "file:foo/bar"
    ];
  [%expect
    {|
    Unix:
    file://shares/pröjects/c%23/#l12 -> file://shares/pr%C3%B6jects/c%23/
    file://sh%c3%a4res/path -> file://sh%C3%A4res/path
    untitled:c:/Users/jrieken/Code/abc.txt -> untitled:c%3A/Users/jrieken/Code/abc.txt
    untitled:C:/Users/jrieken/Code/abc.txt -> untitled:c%3A/Users/jrieken/Code/abc.txt
    /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js -> file:///Users/jrieken/Code/_samples/18500/M%C3%B6del%20%2B%20Other%20Th%C3%AEng%C3%9F/model.js
    file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins -> file:///c%3A/Source/Z%C3%BCrich%20or%20Zurich%20%28%CB%88zj%CA%8A%C9%99r%C9%AAk%2C/Code/resources/app/plugins
    file:foo/bar -> file:///foo/bar
    Windows:
    file://shares/pröjects/c%23/#l12 -> file://shares/pr%C3%B6jects/c%23/
    file://sh%c3%a4res/path -> file://sh%C3%A4res/path
    untitled:c:/Users/jrieken/Code/abc.txt -> untitled:c%3A/Users/jrieken/Code/abc.txt
    untitled:C:/Users/jrieken/Code/abc.txt -> untitled:c%3A/Users/jrieken/Code/abc.txt
    /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js -> file:///Users/jrieken/Code/_samples/18500/M%C3%B6del%20%2B%20Other%20Th%C3%AEng%C3%9F/model.js
    file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins -> file:///c%3A/Source/Z%C3%BCrich%20or%20Zurich%20%28%CB%88zj%CA%8A%C9%99r%C9%AAk%2C/Code/resources/app/plugins
    file:foo/bar -> file:///foo/bar
    |}]

let%expect_test "of_string -> to_path" =
  let test_of_string_to_path =
    let test s =
      let uri = Uri.t_of_yojson (`String s) in
      Printf.printf "%s -> %s\n" s (Uri.to_path uri)
    in
    fun s -> run_with_modes (fun () -> List.iter test s)
  in
  test_of_string_to_path
    [ "file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp"
    ; "file://shares/pröjects/c%23/#l12"
    ; "file:///_:/path"
    ];
  [%expect
    {|
      Unix:
      file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp -> /
      file://shares/pröjects/c%23/#l12 -> //shares/pröjects/c#/
      file:///_:/path -> /_:/path
      Windows:
      file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp -> \
      file://shares/pröjects/c%23/#l12 -> \\shares\pröjects\c#\
      file:///_:/path -> \_:\path
      |}]

(* let%expect_test "full" = let test_full = let test s = let uri =
   Uri.t_of_yojson (`String s) in let path = Uri.to_path uri in let uri2 =
   Uri.of_path path in let equal = Uri.to_string uri = Uri.to_string uri2 in
   Printf.printf "%s -> %s - Uri's equal: %b\n" s (Uri.to_path uri) equal in fun
   paths -> run_with_modes (fun () -> List.iter test paths) in test_full [
   "file:///c:/alex.txt" ;
   "file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins"
   ; "file://monacotools/folder/isi.txt" ;
   "file://monacotools1/certificates/SSL/" ]; [%expect {| Unix:
   file:///c:/alex.txt -> c:/alex.txt - Uri's equal: true
   file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins
   -> c:/Source/Zürich or Zurich (ˈzjʊərɪk,/Code/resources/app/plugins - Uri's
   equal: true file://monacotools/folder/isi.txt -> //monacotools/folder/isi.txt
   - Uri's equal: true file://monacotools1/certificates/SSL/ ->
   //monacotools1/certificates/SSL/ - Uri's equal: true Windows:
   file:///c:/alex.txt -> c:\alex.txt - Uri's equal: true
   file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins
   -> c:\Source\Zürich or Zurich (ˈzjʊərɪk,\Code\resources\app\plugins - Uri's
   equal: true file://monacotools/folder/isi.txt -> \\monacotools\folder\isi.txt
   - Uri's equal: true file://monacotools1/certificates/SSL/ ->
   \\monacotools1\certificates\SSL\ - Uri's equal: true |}] *)
