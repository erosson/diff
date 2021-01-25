import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";
import * as Diff from "diff";

const flags = {};
const app = Elm.Main.init({ flags });
app.ports.diffRequest.subscribe(async (req) => {
  const res = await (async () => {
    try {
      const [before, after] = await Promise.all([
        diffText(req.before),
        diffText(req.after),
      ]);
      const data = runDiff(req, before, after);
      return { status: "success", data };
    } catch (e) {
      console.error(e);
      return { status: "error", error: e.message || e + "" };
    }
  })();
  app.ports.diffResponse.send(res);
});

function runDiff(req, before, after) {
  switch (req.difftype) {
    case "char":
      return Diff.diffChars(before, after);
    case "word":
      return Diff.diffWords(before, after);
    case "sentence":
      return Diff.diffSentences(before, after);
    case "json":
      return Diff.diffJson(
        JSON.stringify(JSON.parse(before), null, 2),
        JSON.stringify(JSON.parse(after), null, 2)
      );
    case "sortjson":
      return Diff.diffJson(sortJson(before), sortJson(after));
    default:
      throw new Error("no such difftype: " + req.difftype);
  }
}
function sortJson(raw) {
  const json = JSON.parse(raw);
  const ord = sortJsonWalk(json);
  return JSON.stringify(ord, null, 2);
}
function sortJsonWalk(json) {
  if (json == null) {
    // because typeof null === "object" for some reason
    return json;
  } else if (Array.isArray(json)) {
    return json.map(sortJsonWalk);
  } else if (typeof json == "object") {
    return Object.entries(json)
      .sort()
      .reduce((obj, [key, val]) => ({ ...obj, [key]: sortJsonWalk(val) }), {});
  } else {
    return json;
  }
}

async function diffText(ireq) {
  switch (ireq.method) {
    case "url":
      const res = await fetch(ireq.url);
      return await res.text();
    case "text":
      return Promise.resolve(ireq.text);
    default:
      throw new Error("no such method: " + ireq.method);
  }
}

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
