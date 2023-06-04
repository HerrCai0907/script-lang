import { exec } from "child_process";
import { readdirSync } from "fs";
import { join } from "path";

const basePath = join(__dirname, "codegen");
const binPath = join(__dirname, "../../build/scriptlang");

const blackList = ["index.d.ts"];
const v = readdirSync(basePath).filter((p) => !blackList.includes(p));

v.forEach((testCase) => {
  test(testCase.replace(/\.ts$/, ""), async () => {
    const testPath = join(basePath, testCase);
    return new Promise<void>((resolved, rejected) => {
      exec(`${binPath} ${testPath}`, (error, stdout, stderr) => {
        if (error == null) {
          expect(stdout).toMatchSnapshot();
          resolved();
        } else {
          rejected(stderr);
        }
      });
    });
  });
});
