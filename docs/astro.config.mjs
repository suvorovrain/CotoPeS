// @ts-check
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";

// https://astro.build/config
export default defineConfig({
    // site: "https://suvorovrain.github.io",
    base: "CotoPeS",
    integrations: [
        starlight({
            title: "CotoPeS",
            social: {
                github: "https://github.com/suvorovrain/CotoPeS",
            },
            sidebar: [
                {
                    slug: "cps",
                },
            ],
        }),
    ],
});
