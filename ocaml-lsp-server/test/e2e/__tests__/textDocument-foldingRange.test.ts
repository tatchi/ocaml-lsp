import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";

import * as Types from "vscode-languageserver-types";

describe("textDocument/foldingRange", () => {
  let languageServer = null;

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  async function openDocument(source: string) {
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "ocaml",
        0,
        source,
      ),
    });
  }

  async function foldingRange() {
    return await languageServer.sendRequest("textDocument/foldingRange", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
    });
  }

  it("returns folding ranges for modules", async () => {
    await openDocument(outdent`
          module type X = sig
            type t =
              | A
              | B
              | C
          end

          module X = struct
            module type T = sig
              module T1 : sig
                type t =
                  | A
                  | B
                  | C
              end
              type t
              val uri : t -> Uri.t
              val dispose : t -> unit
            end

            module Y = struct
              let bar a =
                match a with
                | None ->
                  Stdlib.print_endline "";
                  Stdlib.print_endline "";
                  Stdlib.print_endline ""
                | Some b ->
                  Stdlib.print_endline b;
                  Stdlib.print_endline b;
                  Stdlib.print_endline b
            end

            let foo () =
              let x =
                let y = 5 in
                let z = 3 in
                let open Stdlib in
                let () =
                  let () = print_endline "" in
                  let () = print_endline "" in
                  let () = print_endline "" in
                  ()
                in
                let open Promise.Syntax in
                let%lwt result =
                  let a = 5 in
                  Promise.resolve a
                in
                let+ result_letop =
                  let a = 5 in
                  Promise.resolve a
                in
                z + y
              in
              x + 3
          end

          class foobar =
            let a =
              let () = Stdlib.print_endline "" in
              let () = Stdlib.print_endline "" in
              let () = Stdlib.print_endline "" in
              ()
            in
            object
              method add x y =
                let z =
                  let a = 5 in
                  let b = 6 in
                  let () =
                    let () = Stdlib.print_endline "" in
                    let () = Stdlib.print_endline "" in
                    let () = Stdlib.print_endline "" in
                    ()
                  in
                  a + b
                in
                x + y + z
            end
        `);

    let result = await foldingRange();
    expect(result).toMatchInlineSnapshot(`
      Array [
        Object {
          "endCharacter": 3,
          "endLine": 5,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 0,
        },
        Object {
          "endCharacter": 7,
          "endLine": 4,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 1,
        },
        Object {
          "endCharacter": 3,
          "endLine": 56,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 7,
        },
        Object {
          "endCharacter": 5,
          "endLine": 18,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 8,
        },
        Object {
          "endCharacter": 7,
          "endLine": 14,
          "kind": "region",
          "startCharacter": 4,
          "startLine": 9,
        },
        Object {
          "endCharacter": 11,
          "endLine": 13,
          "kind": "region",
          "startCharacter": 6,
          "startLine": 10,
        },
        Object {
          "endCharacter": 5,
          "endLine": 31,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 20,
        },
        Object {
          "endCharacter": 30,
          "endLine": 30,
          "kind": "region",
          "startCharacter": 4,
          "startLine": 21,
        },
        Object {
          "endCharacter": 9,
          "endLine": 55,
          "kind": "region",
          "startCharacter": 2,
          "startLine": 33,
        },
        Object {
          "endCharacter": 5,
          "endLine": 79,
          "kind": "region",
          "startCharacter": 0,
          "startLine": 58,
        },
      ]
    `);
  });
});
