/**
* Good day, and welcome to render.js.
* 
* @module render
*/

import MarkdownIt from 'https://cdn.jsdelivr.net/npm/markdown-it@13.0.1/+esm';
import markdownItFrontMatter from 'https://cdn.jsdelivr.net/npm/markdown-it-front-matter@0.2.3/+esm'
import * as d3 from "https://cdn.jsdelivr.net/npm/d3@7/+esm";
import { createMetaTag } from './modules/utils.js';

/**
 * Parsed version of what got passed into us from the JSON md doc header
 */
export let frontMatter = {};


/**
 * rendering theme used by Shiki
 */
export let currentTheme = {};

/**
 * Generic content renderer using Shiki
 * 
 * Pass in a string, and what that string is, and sending it back hilighted.
 * 
 * @param {string} contentStringToRender 
 * @param {string} lang (e.g. 'javascript', 'r', 'json', 'md', etc. anything Shiki has)
 * @returns string 
 */
export async function renderContent(contentStringToRender, lang) {

	lang = lang === "svg" ? "xml" : "lang"

	const highlighter = await shiki
		.getHighlighter({
			theme: currentTheme,
			langs: [ lang ]
		})
	
	return Promise.resolve(
		highlighter.codeToHtml(contentStringToRender, { lang: lang })
	)
	
}

/**
 * Core renderer (highlighter) for the app. (I should likely refactor this)
 * 
 * @param {string} markdownFile markdown file base name that must be in the 'md/' folder
 * @param {string} theme short name of theme that must be in the 'themes/` folder
 * @param {string[]} langs array of languages you want the core renderer to be able to handle
 * @param {boolean} renderFrontmatter if `true` then generate OG tags;
 */
export async function renderMarkdown(markdownFile, theme, langs = [ 'javascript', 'r', 'json', 'md', 'xml' ], renderFrontmatter = true) {
	
	currentTheme = await d3.json(`./themes/${theme}.json`);

	await shiki
		.getHighlighter({
			theme: currentTheme,
			langs: [ 'javascript', 'r', 'json', 'md', 'xml' ]
		})
		.then(async highlighter => {

			const md = new MarkdownIt({
				html: true,
				highlight: (code, lang) => {
					return highlighter.codeToHtml(code, { lang })
				}
			}).use(markdownItFrontMatter, async fm => {

				// extract frontmatter and setup meta tags and title
				frontMatter = JSON.parse(fm);
				
				if (renderFrontmatter) {
					const matterKeys = Object.keys(frontMatter);

					const head = document.getElementsByTagName("head")[ 0 ];

					if (matterKeys.includes("title")) {
						document.title = frontMatter.title;
						head.appendChild(createMetaTag("og:title", frontMatter.title))
						head.appendChild(createMetaTag("twitter:title", frontMatter.title))
					}

					if (matterKeys.includes("og")) {
						frontMatter.og.description && head.appendChild(createMetaTag("og:description", frontMatter.og.description))
						frontMatter.og.description && head.appendChild(createMetaTag("twitter:description", frontMatter.og.description))
						frontMatter.og.url && head.appendChild(createMetaTag("og:site", frontMatter.og.url))
						frontMatter.og.site_name && head.appendChild(createMetaTag("og:site_name", frontMatter.og.site_name))
						frontMatter.og.image.url && head.appendChild(createMetaTag("og:image:url", frontMatter.og.image.url))
						frontMatter.og.image.width && head.appendChild(createMetaTag("og:image:width", frontMatter.og.image.width))
						frontMatter.og.image.height && head.appendChild(createMetaTag("og:image:height", frontMatter.og.image.height))
						frontMatter.og.image.alt && head.appendChild(createMetaTag("og:image:alt", frontMatter.og.image.alt))
					
					}

					if (matterKeys.includes("twitter")) {
						frontMatter.twitter.site && head.appendChild(createMetaTag("twitter:site_name", frontMatter.twitter.site))
						frontMatter.twitter.domain && head.appendChild(createMetaTag("twitter:domain", frontMatter.twitter.domain))
						frontMatter.og.image.url && head.appendChild(createMetaTag("twitter:image", frontMatter.og.image.url))
						frontMatter.og.image.url && head.appendChild(createMetaTag("twitter:card", "summary_large_image"))
					}

					head.appendChild(createMetaTag("article:published_time", new Date().toISOString()))
			}

			})

			// parse the rest of the document and add it to the body
			const mdContent = await d3.text(`./md/${markdownFile}.md`);
			const readHTML = new DOMParser();
			const rendered = md.render(mdContent);
			const contentParsed = readHTML.parseFromString(rendered, "text/html");
			const bodyContent = contentParsed.getElementsByTagName("body")[ 0 ];
			const content = document.getElementById("content")
			while (bodyContent.childNodes.length > 0) {
				content.appendChild(bodyContent.childNodes[ 0 ]);
			}

		})

}