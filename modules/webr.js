/**
 * Good day, and welcome to webr.js
 * 
 * Core setup stuff for WebR
 * 
 * @module webr
 */


import { WebR } from '/webr/webr.mjs';

globalThis.webR = new WebR({
	WEBR_URL: "/webr/",
	SW_URL: "/w/phones-by-region/",
});

await globalThis.webR.init();

export const webR = globalThis.webR;

export const source = await webR.evalR('source')
export const library = await webR.evalR('library')

