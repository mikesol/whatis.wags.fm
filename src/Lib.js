"use strict";

const awfulHack = {
	push() {
		return function () {};
	},
};

exports.setErrorText_ = (text) => () => {
	const $wagsErrorMessage = $("#wagsErrorMessage");
	$wagsErrorMessage.html(text);
};

exports.sanitizeUsingRegex_ = (str) => {
	const badUCode = new RegExp("[\u00A0]+", "g");
	const out = str.replace(badUCode, "");
	return out;
};

exports.getAwfulHack_ = () => awfulHack.push;

exports.startIosAudio = function () {
	document.getElementById("wagsSilenceHack").play();
};

exports.stopIosAudio = function () {
	document.getElementById("wagsSilenceHack").pause();
};
