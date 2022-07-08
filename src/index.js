import "./reset.css";
import "../css/reveal.scss";
import "./custom.css";
// night serif simple
import "../css/theme/source/night.scss";
import "../plugin/highlight/monokai.css";
import * as RevealHighlight from "../plugin/highlight/highlight.js";
import Reveal from "../js/";
import { main } from "../output/Main";
import "@fortawesome/fontawesome-free/css/all.css";
import { Notyf } from "notyf";
import "notyf/notyf.min.css"; // for React, Vue and Svelte

// More info about initialization & config:
// - https://revealjs.com/initialization/
// - https://revealjs.com/config/
document.addEventListener("DOMContentLoaded", () =>
	Reveal.initialize({
		hash: true,
		plugins: [RevealHighlight],
	})
);

main();
