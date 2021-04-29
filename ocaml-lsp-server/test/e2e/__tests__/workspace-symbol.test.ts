import * as path from "path";
import * as child_process from "child_process";
import * as Types from "vscode-languageserver-types";
import * as LanguageServer from "./../src/LanguageServer";

describe("workspace/symbol", () => {
  let languageServer = null;

  let testWorkspacePath = path.join(__dirname, "workspace_symbol/");

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize({
      rootUri: LanguageServer.toURI(testWorkspacePath),
    });
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  async function queryWorkspaceSymbol(query: string) {
    return await languageServer.sendRequest("workspace/symbol", {
      query,
    });
  }

  it("returns symbols from workspace", async () => {
    child_process.execSync("dune build", { cwd: testWorkspacePath });

    let result: Types.SymbolInformation[] = await queryWorkspaceSymbol("");

    expect(
      result.map((symbolInfo) => ({
        ...symbolInfo,
        location: {
          ...symbolInfo.location,
          uri: symbolInfo.location.uri.replace(testWorkspacePath, '<testWorkspacePath>/'),
        },
      })),
    ).toMatchSnapshot();
  });
});
