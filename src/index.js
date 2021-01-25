import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";
import Diff from "diff";

const flags = {};
const app = Elm.Main.init({ flags });
app.ports.diffRequest.subscribe((req) => {
  console.log("diffRequest", req);
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
