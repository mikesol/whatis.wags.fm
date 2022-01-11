import "./reset.css";
import "../css/reveal.scss";
// night serif simple
import "../css/theme/source/night.scss";
import "../plugin/highlight/monokai.css";
import Reveal from "../js/";
import main from "../output/Main";
import "@fortawesome/fontawesome-free/css/all.css";
import { Notyf } from 'notyf';
import 'notyf/notyf.min.css'; // for React, Vue and Svelte

// More info about initialization & config:
// - https://revealjs.com/initialization/
// - https://revealjs.com/config/
document.addEventListener("DOMContentLoaded", () =>
	Reveal.initialize({
		hash: true,
	})
);

const mainFx = () => {
	var osc = {};
	Reveal.on("slidechanged", () => {
		for (const [_, value] of Object.entries(osc)) {
			value();
		}
	});
	return {
		registerSlideChange: (key) => (value) => () => (osc[key] = value),
		unregisterSlideChange: (key) => () => delete osc[key],
		notfy: () => new Notyf({ duration: 0, dismissible: true })
	};
};
main.main(mainFx)();
