/**
* Good day, and welcome to main.js.
*
* @module main
*/

// render the page scaffold as quickly as possible
import { renderMarkdown, renderContent  } from "./render.js";
await renderMarkdown(
	`${import.meta.url.split('/').pop().split('.')[ 0 ]}`,
	"ayu-dark",
	[ 'javascript', 'r', 'json', 'md', 'xml' ],
	false
)

import * as d3 from "https://cdn.jsdelivr.net/npm/d3@7/+esm";
import { component, store } from './modules/reef.es.min.js';
import { slugify } from './modules/utils.js'

// reactive component for general messages
export let message = store({ text: null })
export function showMessage() {
	const { text } = message;
	return text === null ? "" : `${text}`
}
component('#message', showMessage);

// reactive component that will manage the region popup select menu
let region = store({ options: null })
function showRegion() {
	const { options } = region
	var opts = `<label for='region-select'> Region: <select id="region-select">`
	if (options !== null) {
   opts += options.map(o => `<option id='reg-opt-${slugify(o)}'>${o}</option>`)
	} 
	opts += "</select></label>"
	return(opts)
}
component('#region', showRegion);

/**
 * Take a region and return an svg barlot
 * 
 * @param {string} region 
 * @returns {string}
 */
async function renderPlot(region) {
	return Promise.resolve(
		(await R.webR.evalRString(`
	s = svgstring(width = 8, height = 4, pointsize = 8, id = "region-plot", standalone = FALSE)

	par(
		bg = theme$panel.fill,
		fg = theme$label.color
	)

	barplot(
		WorldPhones[, "${region}" ],
		main = "${region}",
		col = theme$bar.fill,
		sub = "Data from AT&T (1961) The World's Telephones",
		ylab = "Number of Telephones (K)",
		xlab = "Year",
		border = NA,
		col.axis = theme$axis.color,
		col.lab = theme$label.color,
		col.sub = theme$subtitle.color,
		col.main = theme$title.color
	)

	dev.off()

	s()
	`)).replace(/width='\d+(\.\d+)?pt'/, "width='100%'")
			.replace(/height='\d+(\.\d+)?pt'/, "")
	)
}

// re-render plot when the popup changes
d3.select("#region").on('change', async (event) => {
	selectedRegion.svgText = await renderPlot(event.target.value)
})

// reactive component which will trigger the barplot re-render
export let selectedRegion = store({ svgText: null })
export function showSelectedRegion() {
	const { svgText } = selectedRegion;
	return svgText === null ? "" : `${svgText}`
}
component('#selected-region', showSelectedRegion);

message.text = `Loading WebR…`;

// this triggers the WebR environment creation
import * as R from "./modules/webr.js"

message.text = `${crossOriginIsolated ? '🔵' : '🌕'} WebR Initialized!`;

await R.webR.installPackages([ "svglite" ])

await R.library(`svglite`)
await R.library(`datasets`)

// our base plot theme
await R.webR.evalR(`
list(
  panel.fill = "#001e38",
  bar.fill = "#4a6d88",
  axis.color = "#c6cdd7",
  label.color = "#c6cdd7",
  subtitle.color = "#c6cdd7",
  title.color = "#c6cdd7",
  ticks.color = "#c6cdd7",
  axis.color =  "#c6cdd7"
) -> theme
`)

message.text = `${crossOriginIsolated ? '🔵' : '🌕'} {svglite} loaded.`;

// load regions
region.options = await (await R.webR.evalR(`colnames(WorldPhones)`)).toArray()

// start with the first region
selectedRegion.svgText = await renderPlot(region.options[ 0 ]);

message.text = `${crossOriginIsolated ? '🔵' : '🌕'} Ready`;
