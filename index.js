import { registerCustomElement, registerPorts } from "elm-mapbox";
import "mapbox-gl/dist/mapbox-gl.css";
import { Elm } from "./src/Main.elm";

const token = "pk.eyJ1Ijoic3BvdHRpeWVyIiwiYSI6ImNqZmQyZnVkejIwbGgyd29iZnR3bGVvMXUifQ.fVrLRiLoyIoPfAGm5ozmMg";

registerCustomElement({ token: token });
var app = Elm.Main.init({ node: document.body });
registerPorts(app);
